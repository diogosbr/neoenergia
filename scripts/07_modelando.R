# Carregando pacotes ----
devtools::load_all("/home/smaug/home/diogo/Documentos/github/NicheEaseR/")
library(terra)
library(readr)

# Caminho base para salvar resultados ----
output <- "resultados/chafariz/v01/"

# Ocorrências ----
occ_all <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")

# Lista de espécies a modelar
spp <- unique(occ_all$species)

# Variáveis preditoras ----
# Listando os arquivos
lista_fit  <- list.files("dados/raster/bioclimaticas/brasil_sel/",
                         pattern = "\\.tif$", full.names = TRUE)
lista_proj <- list.files("dados/raster/bioclimaticas/caatinga_sel/",
                         pattern = "\\.tif$", full.names = TRUE)

# Importando as variáveis preditoras
predictors_fit  <- rast(lista_fit)   # para ajuste/calibração
predictors_proj <- rast(lista_proj)  # para projeção (usar depois)

# Loop de modelagem ----
for (sp in spp[1:10]) {
  
  message("Modelando espécie: ", sp)
  
  # Filtra ocorrências da espécie
  # assumindo que a primeira coluna é 'species'
  occ_xy <- occ_all[occ_all$species == sp, -1]
  
  # Se tiver poucas ocorrências, pula (evita erro no kfold)
  if (nrow(occ_xy) < 5) {
    message("  - menos de 5 ocorrências, pulando.")
    next
  }
  
  if (nrow(occ_xy) < 5) {
    message("  - menos de 5 ocorrências válidas (sem NA), pulando.")
    next
  }
  
  # Área de calibração do modelo ----
  # Usa o primeiro raster de preditores de ajuste como base
  base_raster <- calibration_area(occ_xy, predictors_fit[[1]])
  
  # Gerando pontos de pseudo-ausência ----
  # ajuste o 'n' conforme seu padrão no pacote
  pa_pts <- background(base_raster, n = 10000)
  
  # Particionando os dados (k-fold) ----
  occ_k <- kfold(occ_xy, k = 5)
  pa_k  <- kfold(pa_pts,  k = 5)
  
  names(occ_k)[1:2] <- names(pa_k)[1:2] <- c("lon", "lat")
  
  # Indicador de presença/ausência
  occ_k$pa <- 1
  pa_k$pa  <- 0
  
  # Criando a tabela com dados ambientais associados aos pontos ----
  # aqui eu uso o raster de calibração (brasil_sel) como vars_sel,
  # seguindo a estrutura do exemplo
  sdm_data <- generateEnvTable(
    presence = occ_k,
    absence  = pa_k,
    independent_vars = predictors_fit,
    lon = 'lon', lat = 'lat'
  )
  
  # Criando os diretórios para salvar os resultados ----
  output_folders(path = output, sp_name = sp)
  
  # Gerando modelos (partições) ----
  modeling(
    sdm_data,
    species_name    = sp,
    predictors      = predictors_fit,
    project         = T, 
    predictors_proj = list(caatinga = predictors_proj),
    partitions      = 5,
    algorithms      = c("bioclim", 'maxent', 'randomforest', 'svm'),
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
  
  # Neste ponto você já deve ter:
  # - modelos por algoritmo e partição em resultados/chafariz/v01/sp/
  # - objetos combinados (algo) e ensemble (ens)
  # Para projeção em 'predictors_proj', use a função de projeção do seu pacote,
  # se existir, ou terra::predict com 'algo'/'ens'.
desempenho <- list.files(paste0(output, sp, '/models'), pattern = "^evaluate", full.names = T) %>% 
  lapply(read_csv, col_types = "ddddddddddddcd") %>% 
  bind_rows() %>% 
  mutate(species = sp)

write_csv(desempenho, paste0(output, sp, '/models_ensemble/evaluate_', sp, ".csv"))
}
