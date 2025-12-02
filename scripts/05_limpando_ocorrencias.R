# Carregando os pacotes ---------------------------------------------------
library(readr)
library(CoordinateCleaner)
library(raster)
library(dismo)
library(dplyr)

# Importando os pontos de ocorrência --------------------------------------
occ_raw <- read_csv("dados/tabelas/ocorrencias_brutas_todas.csv")

# Verifica inicio da tabela
occ_raw

# Seleciona as colunas de interesse
occ_coord <- 
  occ_raw %>% 
    select(scientificname, decimallongitude, decimallatitude) %>% 
    filter(!is.na(decimallongitude))

# Checando os dados de ocorrencia
occ_clean <- clean_coordinates(occ_coord, 
                               species = "scientificname",
                               lon = 'decimallongitude',
                               lat = 'decimallatitude',
                               tests = c("equal", "outliers", "zeros", 'dupl'),
                               value = "clean")

# Número de ocorrências únicas
nrow(occ_coord)
nrow(occ_clean)





# Importando uma variável preditora
var1 <- raster('dados/abioticos/presente/bio_01.tif')

# Salvando o tabela com os registros únicos por pixel
occ_unique <- gridSample(occ_clean, var1, n = 1)

# Resetando os nomes das linhas
rownames(occ_unique) <- NULL

# Número de ocorrências únicas
nrow(occ_unique)

# Removendo dados com 'NA'
occ_modelagem <- occ_unique[!is.na(extract(var1, occ_unique)),]

# Número de ocorrências dentro do raster, com valores ambientais associados
# Número de ocorrências únicas por pixel, com valores e sem inconcistencias
nrow(occ_modelagem)

# Salvando no disco
write.csv(occ_modelagem, "dados/ocorrencias/ocorrencias_modelagem.csv",
          row.names = FALSE)




# remover duplicatas (mesmo ponto presente nas duas fontes)
occ_all_nodup <- occ_all %>%
  distinct(species, decimallongitude, decimallatitude, year, .keep_all = TRUE)