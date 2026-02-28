# Carregando pacotes ----
devtools::load_all("/home/smaug/home/diogo/Documentos/github/NicheEaseR/")
library(terra)
library(dplyr)
library(readr)
library(parallel)
library(purrr)

# Caminho base para salvar resultados ----
output <- "resultados/tudo/v01"

# Ocorrências ----
occ_all <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")

# Lista de espécies a modelar ----
spp <- unique(occ_all$searched)

# Variáveis preditoras ----
lista_fit  <- list.files(
  "dados/raster/bioclimaticas/brasil_sel/",
  pattern = "\\.tif$",
  full.names = TRUE
)

lista_proj <- list.files(
  "dados/raster/bioclimaticas/caatinga_sel/",
  pattern = "\\.tif$",
  full.names = TRUE
)

# Importando as variáveis preditoras
predictors_fit  <- rast(lista_fit)   # para ajuste/calibração
predictors_proj <- rast(lista_proj)  # para projeção

#--------------------------------------------------------------------
# Função que faz TODO o fluxo para uma espécie, com tryCatch
#--------------------------------------------------------------------
modelar_especie <- function(sp,
                            occ_all,
                            predictors_fit,
                            predictors_proj,
                            output) {
  
  tryCatch({
    message("Modelando espécie: ", sp)
    
    # Filtra ocorrências da espécie
    occ_xy <- occ_all[occ_all$searched == sp, , drop = FALSE]
    
    # Se tiver poucas ocorrências, pula
    if (nrow(occ_xy) < 10) {
      message("  - menos de 10 ocorrências, pulando.")
      return(
        tibble(
          species = sp,
          status  = "poucas_ocorrencias",
          msg     = NA_character_
        )
      )
    }
    
    # Garante que colunas de coord. existem
    if (!all(c("decimallongitude", "decimallatitude") %in% names(occ_xy))) {
      stop("Colunas 'decimalLongitude' e/ou 'decimallatitude' não encontradas em `occ_all`.")
    }
    
    # Renomeia coordenadas para lon/lat mantendo species intacta
    names(occ_xy)[names(occ_xy) == "decimallongitude"] <- "lon"
    names(occ_xy)[names(occ_xy) == "decimallatitude"]  <- "lat"
    
    # Área de calibração do modelo ----
    base_raster <- calibration_area(
      occ         = occ_xy[, c("lon", "lat")],
      base_raster = predictors_fit[[1]]
    )
    
    # Gerando pontos de pseudo-ausência ----
    pa_pts <- background(base_raster, n = 10000)
    
    # Garante data.frame e renomeia coord. de pseudo-ausência
    pa_df <- as.data.frame(pa_pts)
    # Assume que as duas primeiras colunas são coordenadas
    names(pa_df)[1:2] <- c("lon", "lat")
    
    # Particionando os dados (k-fold) ----
    occ_k <- kfold(occ_xy, k = 5)
    pa_k  <- kfold(pa_df,   k = 5)
    
    # Indicador de presença/ausência
    occ_k$pa <- 1
    pa_k$pa  <- 0
    
    # Criando a tabela com dados ambientais associados aos pontos ----
    sdm_data <- generateEnvTable(
      occ_pres    = occ_k,
      occ_abs     = pa_k,
      env_rasters = predictors_fit,
      lon_col     = "lon",
      lat_col     = "lat"
    )
    
    # Criando os diretórios para salvar os resultados ----
    output_folders(
      output_dir = output,
      sp_name    = sp
    )
    
    # Gerando modelos (partições) ----
    modeling(
      env_data          = sdm_data,
      species_name      = sp,
      env_rasters       = predictors_fit,                  # Brasil
      algorithms        = c("maxent", "randomforest", "svm"),
      partitions        = 5,
      project           = TRUE,
      env_rasters_proj  = list(caatinga = predictors_proj),# Caatinga
      models_to_combine = c("raw", "bin"),
      output_dir        = output,
      lon_col           = "lon",
      lat_col           = "lat",
      fold_col          = "K",
      pa_col            = "pa",
      save_calibration_maps = FALSE
    )
    
    # Combina modelos por algoritmo
    algo <- combine_models(
      species_name      = sp,
      algorithms        = "all",
      proj              = TRUE,      # <- como você quer: só projeções
      models_to_combine = c("raw", "bin"),
      output_dir        = output
    )
    
    # Ensemble final
    ens <- ensemble(
      species_name      = sp,
      models_to_combine = c("raw", "bin"),
      algorithms        = "all",
      proj              = TRUE,      # <- idem
      output_dir        = output
    )
    
    # Extraindo e consolidando desempenho dos modelos ----
    desempenho <- list.files(
      file.path(output, sp, "models"),
      pattern    = "^evaluate",
      full.names = TRUE
    ) %>%
      map(~ read_csv(.x, show_col_types = FALSE)) %>%
      map(~ mutate(.x,
                   kappa      = readr::parse_number(as.character(kappa)),
                   prevalence = readr::parse_number(as.character(prevalence))
      )) %>% 
      bind_rows() %>%
      mutate(species = sp)
    
    # Salvando tabela de desempenho consolidada para a espécie ----
    write_csv(
      desempenho,
      file.path(output, sp, "models_ensemble", paste0("evaluate_", sp, ".csv"))
    )
    
    tibble(
      species = sp,
      status  = "ok",
      msg     = NA_character_
    )
    
  }, error = function(e) {
    message("  - ERRO para espécie ", sp, ": ", conditionMessage(e))
    tibble(
      species = sp,
      status  = "erro",
      msg     = conditionMessage(e)
    )
  })
}

#--------------------------------------------------------------------
# Paralelização com mclapply
#--------------------------------------------------------------------
n_cores <- 3

resultado_list <- mclapply(
  X        = spp,
  FUN      = modelar_especie,
  occ_all  = occ_all,
  predictors_fit  = predictors_fit,
  predictors_proj = predictors_proj,
  output   = output,
  mc.cores = n_cores
)

# Consolida o resultado em uma tabela
resultado <- dplyr::bind_rows(resultado_list)
print(resultado)

write_csv(
  resultado,
  file.path(output, "resultado.csv"))
