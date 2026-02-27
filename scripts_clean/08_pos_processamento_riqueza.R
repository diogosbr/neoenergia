library(terra)
library(dplyr)
library(readr)
library(dplyr)
library(terra)
library(readxl)

# Bacia nivel 5
bho_nv5 <- vect("sig/Shapes/geoft_bho_ach_otto_nivel_05.gpkg")

# Bacia nivel 4
# chafariz_buffer <- vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") %>% 
chafariz_buffer <- vect("sig/Shapes/01. CHAFARIZ/novos/AII_CEC_nova.shp") %>% 
  project("EPSG:4674")

# chafariz_pts <- vect("sig/Shapes/01. CHAFARIZ/prj_Aerogeradores_pt_CHAFARIZ.shp") %>% 
#   project("EPSG:4674")

# Bacias que contem chafariz
bho_chafariz <- bho_nv5[bho_nv5$wts_cd_pfafstetterbasin %in% c(75622, 75646, 75624, 75628, 75848),]

rm(bho_nv5)

# Tudo
spp_list <- 
  read_csv("dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv", show_col_types = FALSE) %>% 
  filter(Chafariz == "x") %>% select(`Nome válido`) %>% distinct() %>% pull()

# endemica
spp_list <- 
  read_csv("dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv", show_col_types = FALSE) %>% 
  filter(Chafariz == "x", grepl("Endêmica.*", .$Endêmica)) %>% select(`Nome válido`) %>% distinct() %>% pull()

# ameaçada
spp_list <- 
  read_csv("dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv", show_col_types = FALSE) %>% 
  filter(Chafariz == "x", grepl("VU|EN", .$`Grau de proteção`)) %>% select(`Nome válido`) %>% distinct() %>% pull()
  
# migratória
spp_list <- 
  read_csv("dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv", show_col_types = FALSE) %>% 
  filter(Chafariz == "x", grepl("igratória.*", .$Migratória)) %>% select(`Nome válido`) %>% distinct() %>% pull()

# com os 4 algoritmos  
# lista_geral <- list.files("resultados/chafariz/v03/", recursive = TRUE, full.names = TRUE)
# 
# lista_ensemble_bin <- lista_geral[grepl("models_ensemble/caatinga/bin_", lista_geral)]
# 
# lista_ensemble_bin <- lista_ensemble_bin[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)]

# só RF
lista_geral <- list.files("resultados/chafariz/v06/", recursive = TRUE, full.names = TRUE)

lista_ensemble_bin <- lista_geral[grepl("models_ensemble/caatinga/bin_", lista_geral)]

lista_ensemble_bin <- lista_ensemble_bin[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)]

# sem bioblim
# lista_ensemble_bin <- list.files("resultados/chafariz/ensemble_v03/bin//", recursive = TRUE, full.names = TRUE)
# lista_ensemble_bin <- lista_ensemble_bin[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)]

riqueza_bin <- rast(lista_ensemble_bin) |> sum()
riqueza_bin <- project(riqueza_bin, "EPSG:4674")

caatinga <- vect("sig/Shapes/Caatinga shape/Caatinga.shp") |>
  project("EPSG:4674")
buffer_impacto <- vect("sig/Shapes/01. CHAFARIZ/novos/AII_CEC_nova.shp") |>
  project("EPSG:4674")
area_impacto <- vect("sig/Shapes/01. CHAFARIZ/prj_Aerogeradores_pt_CHAFARIZ.shp") |>
  project("EPSG:4674")

# buffer_impacto <- vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") |>
#   project("EPSG:4674")


riqueza_buffer <- mask(riqueza_bin, buffer_impacto) |> trim()
riqueza_bho_nv5 <- mask(riqueza_bin, bho_chafariz) |> trim()

cols <- c(
  "#103779", "#1B6F88", "#249E85", "#18C249", "#47DF1A",
  "#C8F513", "#F1D822", "#ECAB21", "#D47631", "#C1543E"
)
pal <- grDevices::colorRampPalette(cols)


# Salvar
png("plots/mapa_riqueza_buffer.png", width = 2000, height = 1500, res = 300)
plot(riqueza_buffer, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

png("plots/mapa_riqueza_bho_nv5.png", width = 2000, height = 1500, res = 300)
plot(riqueza_bho_nv5, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(bho_chafariz, add = T, pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

png("plots/mapa_riqueza_caatinga.png", width = 2000, height = 1500, res = 300)
plot(riqueza_bin, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(bho_chafariz, add = T, pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()


boxplot(riqueza_buffer[])
summary(riqueza_buffer[])
hist(riqueza_buffer[], freq=T)

writeRaster(riqueza_bin, "resultados/riqueza_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_bho_nv5, "resultados/riqueza_bho.tif", overwrite = TRUE)
writeRaster(riqueza_buffer, "resultados/riqueza_buffer.tif", overwrite = TRUE)

png("plots/riqueza_buffer_endemicas.png", width = 2000, height = 1500, res = 300)
plot(riqueza_buffer, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

writeRaster(riqueza_buffer, "resultados/riqueza_buffer_endemicas.tif", overwrite = TRUE)

png("plots/riqueza_buffer_ameacadas.png", width = 2000, height = 1500, res = 300)
plot(riqueza_buffer, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

writeRaster(riqueza_buffer, "resultados/riqueza_buffer_ameacadas.tif", overwrite = TRUE)

png("plots/riqueza_buffer_migratorias.png", width = 2000, height = 1500, res = 300)
plot(riqueza_buffer, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

writeRaster(riqueza_buffer, "resultados/riqueza_buffer_migratorias.tif", overwrite = TRUE)



png("plots/riqueza_caat_endemicas.png", width = 2000, height = 1500, res = 300)
plot(riqueza_bin, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

writeRaster(riqueza_bin, "resultados/riqueza_caat_endemicas.tif", overwrite = TRUE)

png("plots/riqueza_caat_ameacadas.png", width = 2000, height = 1500, res = 300)
plot(riqueza_bin, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

writeRaster(riqueza_bin, "resultados/riqueza_caat_ameacadas.tif", overwrite = TRUE)

png("plots/riqueza_caat_migratorias.png", width = 2000, height = 1500, res = 300)
plot(riqueza_bin, col = pal(20))
plot(area_impacto, add = T, col = "white", pch = 16, cex = 0.6)
plot(buffer_impacto, add = T, border = "red", lwd = 2)
dev.off()

writeRaster(riqueza_bin, "resultados/riqueza_caat_migratorias.tif", overwrite = TRUE)

#---------------------------------#

library(terra)
library(dplyr)

riqueza_bin <- rast("resultados/riqueza_caatinga.tif")
riqueza_bho_nv5 <- rast("resultados/riqueza_bho.tif")
riqueza_buffer <- rast("resultados/riqueza_buffer.tif")


s_caat <- riqueza_bin %>% values(mat=F, na.rm = T) %>% table()
s_bho <- riqueza_bho_nv5 %>% values(mat=F, na.rm = T) %>% table()
s_buffer <- riqueza_buffer %>% values(mat=F, na.rm = T) %>% table()

plot(s_caat)
plot(s_bho)
plot(s_buffer)


riqueza_bin %>% values(mat=F, na.rm = T) %>% summary()
riqueza_bho_nv5 %>% values(mat=F, na.rm = T) %>% summary()
riqueza_buffer %>% values(mat=F, na.rm = T) %>% summary()





riqueza_bho_nv5 <- rast("resultados/dezembro_2025/riqueza_bho_endemicas.tif")
riqueza_buffer <- rast("resultados/riqueza_buffer_endemicas.tif")

s_bho <- riqueza_bho_nv5 %>% values(mat=F, na.rm = T) %>% table()
s_buffer <- riqueza_buffer %>% values(mat=F, na.rm = T) %>% table()

plot(s_bho)
plot(s_buffer)

riqueza_bho_nv5 %>% values(mat=F, na.rm = T) %>% summary()
riqueza_buffer %>% values(mat=F, na.rm = T) %>% summary()










