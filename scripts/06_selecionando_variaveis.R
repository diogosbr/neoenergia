# Carregando os pacotes ---------------------------------------------------
library(terra)
library(readr)
library(dplyr)
library(usdm)

# Listando os arquivos
lista_proj <- list.files("dados/raster/bioclimaticas/caatinga/", full.names = T)
lista_fit <- list.files("dados/raster/bioclimaticas/brasil/", full.names = T)

#lista_abio <- lista_abio[c(1,2,3,4,5,6,7,12,13,14,15)]

# Importando as variáveis preditoras
predictors_proj <- rast(lista_proj)
predictors_fit <- rast(lista_fit)

pts <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")[,-1]

# Plotando a primeira variável
plot(predictors_fit[[1]])
points(pts, pch = 16, cex = 0.7)

# Criando uma tabela com os valores por pixel
env_data <- extract(predictors_fit, pts, ID = FALSE)

head(env_data)
dim(env_data)

# Com extract das espécies
vif(env_data)
vif_results <- vifcor(env_data, size = 1e3, method = "spearman", th = 0.5)

vif_results
vif_results@excluded

vif_results@results$Variables

s <- paste(vif_results@results$Variables, collapse = "|")

from <- lista_fit[str_detect(lista_fit, s)]
to <- file.path("dados/raster/bioclimaticas/brasil_sel/", basename(from))
file.copy(from, to)

from <- lista_fit[str_detect(lista_proj, s)]
to <- file.path("dados/raster/bioclimaticas/caatinga_sel/", basename(from))
file.copy(from, to)

# # Selecionando as variáveis
# preditoras_selecionadas <- preditoras[[-variaveis_remover]]
# 
# # Criando a pasta para receber as variaveis selecionadas
# if(!dir.exists("data/rasters/selecionados/")){dir.create("data/rasters/selecionados/", recursive = TRUE)}
# 
# # Salvando as variáveis no disco
# for(i in 1:nlyr(preditoras_selecionadas)){
#   writeRaster(preditoras_selecionadas[[i]],
#               filename = paste0("data/rasters/selecionados/",
#                                 names(preditoras_selecionadas)[i], ".tif"),
#               gdal = "COMPRESS=DEFLATE",
#               overwrite = TRUE)
# }
