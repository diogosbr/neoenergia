# Carregando os pacotes ---------------------------------------------------
library(readr)
library(CoordinateCleaner)
library(raster)
library(dismo)
library(dplyr)
library(spThin)

# Importando os pontos de ocorrência --------------------------------------
occ_raw <- read_csv("dados/tabelas/ocorrencias_limpas.csv")

# Verifica inicio da tabela
occ_raw

# Seleciona as colunas de interesse
occ_coord <- 
  occ_raw %>% 
    dplyr::select(species, decimallongitude, decimallatitude) %>% 
    filter(!is.na(decimallongitude))

# Checando os dados de ocorrencia
occ_clean <- cc_val(
  x    = occ_coord,
  lon  = "decimallongitude",
  lat  = "decimallatitude",
  value   = "clean",
  verbose = TRUE) %>% 
  clean_coordinates(species = "species",
                    lon = 'decimallongitude',
                    lat = 'decimallatitude',
                    tests = c("equal", "outliers", "zeros"),
                    value = "clean") %>% 
  distinct()

# Número de ocorrências únicas
nrow(occ_coord)
nrow(occ_clean)

spp_names <- unique(occ_clean$species)

for (sp in unique(occ_clean$species)) {
  occs <- subset(occ_clean, species == sp)
  
  thin(loc.data  = occs,
       long.col  = "decimallongitude",
       lat.col   = "decimallatitude",
       spec.col  = "species",
       thin.par  = 10,       # 10 km
       reps      = 1,
       write.files = TRUE,
       out.dir   = "dados/tabelas/occ_thin/",
       out.base  = paste0(sp, "_thinned"),
       write.log.file = FALSE)
}

occ_thin <- list.files("dados/tabelas/occ_thin/", full.names = T, pattern = "csv$") %>% 
  lapply(read_csv, show_col_types = FALSE) %>% bind_rows() %>% as.data.frame()

write_csv(occ_thin, "dados/tabelas/ocorrencias_thin.csv")

# Importando uma variável preditora
var1 <- raster('dados/raster/bioclimaticas/brasil_sel/bio_02.tif')

# Removendo dados com 'NA'
occ_modelagem <- occ_thin[!is.na(extract(var1, occ_thin[,-1])),]

# Número de ocorrências dentro do raster, com valores ambientais associados
# Número de ocorrências únicas por pixel, com valores e sem inconcistencias
nrow(occ_thin)
nrow(occ_modelagem)

# Salvando no disco
write_csv(occ_modelagem, "dados/tabelas/ocorrencias_modelagem.csv")
