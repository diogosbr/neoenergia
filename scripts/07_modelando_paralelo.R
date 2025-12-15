# Carregando pacotes ----
devtools::load_all("/home/smaug/home/diogo/Documentos/github/NicheEaseR/")
library(terra)
library(dplyr)
library(readr)
library(parallel)

# Caminho base para salvar resultados ----
output <- "resultados/chafariz/v01/"

# Ocorrências ----
occ_all <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")

# Lista de espécies a modelar ----
spp <- unique(occ_all$species)

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
    occ_xy <- occ_all[occ_all$species == sp, -1]
    
    # Se tiver poucas ocorrências, pula
    if (nrow(occ_xy) < 5) {
      message("  - menos de 5 ocorrências, pulando.")
      return(
        tibble(
          species = sp,
          status  = "poucas_ocorrencias",
          msg     = NA_character_
        )
      )
    }
    
    # IMPORTANTE: deixar occ_xy visível no .GlobalEnv do worker
    # porque calibration_area aparentemente procura esse objeto pelo nome
    assign("occ_xy", occ_xy, envir = .GlobalEnv)
    
    # Área de calibração do modelo ----
    base_raster <- calibration_area(occ_xy, predictors_fit[[1]])
    
    # Gerando pontos de pseudo-ausência ----
    pa_pts <- background(base_raster, n = 10000)
    
    # Particionando os dados (k-fold) ----
    occ_k <- kfold(occ_xy, k = 5)
    pa_k  <- kfold(pa_pts,  k = 5)
    
    # Ajusta nomes das colunas de coordenadas para lon/lat
    names(occ_k)[1:2] <- names(pa_k)[1:2] <- c("lon", "lat")
    
    # Indicador de presença/ausência
    occ_k$pa <- 1
    pa_k$pa  <- 0
    
    # Criando a tabela com dados ambientais associados aos pontos ----
    sdm_data <- generateEnvTable(
      presence         = occ_k,
      absence          = pa_k,
      independent_vars = predictors_fit,
      lon              = "lon",
      lat              = "lat"
    )
    
    # Criando os diretórios para salvar os resultados ----
    output_folders(path = output, sp_name = sp)
    
    # Gerando modelos (partições) ----
    modeling(
      sdm_data,
      species_name    = sp,
      predictors      = predictors_fit,
      project         = TRUE,
      predictors_proj = list(caatinga = predictors_proj),
      partitions      = 5,
      algorithms      = c("bioclim", "maxent", "randomforest", "svm"),
      output_path     = output
    )
    
    # Combinando modelos por algoritmo ----
    algo <- combine_models(
      species_name = sp,
      algorithms   = "all",
      output_path  = output
    )
    
    # Ensemble final ----
    ens <- ensemble(
      species_name = sp,
      algorithms   = "all",
      output_path  = output
    )
    
    # Extraindo e consolidando desempenho dos modelos ----
    desempenho <- list.files(
      paste0(output, sp, "/models"),
      pattern    = "^evaluate",
      full.names = TRUE
    ) %>%
      lapply(read_csv, col_types = "ddddddddddddcd") %>%
      bind_rows() %>%
      mutate(species = sp)
    
    # Salvando tabela de desempenho consolidada para a espécie ----
    write_csv(
      desempenho,
      paste0(output, sp, "/models_ensemble/evaluate_", sp, ".csv")
    )
    
    tibble(
      species = sp,
      status  = "ok",
      msg     = NA_character_
    )
    
  }, error = function(e) {
    # Não derruba o mclapply inteiro; registra o erro
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
n_cores <- 10

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
