library(terra)
library(readr)
library(dplyr)
library(stringr)

# Bacia nivel 5
bho_nv5 <- vect("sig/Shapes/geoft_bho_ach_otto_nivel_05.gpkg")

# Bacias que contem chafariz
bho_chafariz <- bho_nv5[bho_nv5$wts_cd_pfafstetterbasin %in% c(75622, 75646, 75624, 75628, 75848),]
bho_luzia <- bho_nv5[bho_nv5$wts_cd_pfafstetterbasin %in% c(75622, 75624),]
bho_oitis <- bho_nv5[bho_nv5$wts_cd_pfafstetterbasin %in% c( 74482, 76174, 74492, 74424),]

rm(bho_nv5)

spp_list <- read_csv(
  "dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia_Oitis.csv",
  show_col_types = FALSE
)

# endemica
spp_endemic_chafariz <- spp_list %>% 
  filter(Chafariz == "x", grepl("Endêmica.*", .$Endêmica)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

spp_endemic_luzia <- spp_list %>% 
  filter(Luzia == "x", grepl("Endêmica.*", .$Endêmica)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

spp_endemic_oitis <- spp_list %>% 
  filter(Oitis == "x", grepl("Endêmica.*", .$Endêmica)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

# ameaçada
spp_threatened_chafariz <- spp_list %>% 
  filter(Chafariz == "x", grepl("VU|EN", .$`Grau de proteção IUCN`)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

spp_threatened_luzia <- spp_list %>% 
  filter(Luzia == "x", grepl("VU|EN", .$`Grau de proteção IUCN`)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

spp_threatened_oitis <- spp_list %>% 
  filter(Oitis == "x", grepl("VU|EN", .$`Grau de proteção IUCN`)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

# migratória
spp_migratory_chafariz <- spp_list %>% 
  filter(Chafariz == "x", grepl("igratória.*", .$Migratória)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

spp_migratory_luzia <- spp_list %>% 
  filter(Luzia == "x", grepl("igratória.*", .$Migratória)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

spp_migratory_oitis <- spp_list %>% 
  filter(Oitis == "x", grepl("igratória.*", .$Migratória)) %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]


# lista_chafariz <- list.files("resultados/areas/Chafariz/", pattern = "bin_", recursive = TRUE, full.names = TRUE)
# lista_luzia <- list.files("resultados/areas/Luzia//", pattern = "bin_", recursive = TRUE, full.names = TRUE)
# lista_oitis <- list.files("resultados/areas/Oitis//", pattern = "bin_", recursive = TRUE, full.names = TRUE)

lista_chafariz <- list.files("resultados/areas/Chafariz/", pattern = "bin_", recursive = TRUE, full.names = TRUE)
lista_luzia <- list.files("resultados/areas/Luzia//", pattern = "bin_", recursive = TRUE, full.names = TRUE)
lista_oitis <- list.files("resultados/areas/Oitis//", pattern = "bin_", recursive = TRUE, full.names = TRUE)

area_total_chafariz <- vect("sig/Shapes/novos/cálculo área total/AREA TOTAL CEC_.shp") %>%
  project("EPSG:4674")
area_empreendimento_chafariz <- vect("sig/Shapes/novos/cálculo empreendimento/AII CEC.shp") %>%
  project("EPSG:4674")

area_total_luzia <- vect("sig/Shapes/novos/cálculo área total/AREA TOTAL LUZIA_.shp") %>%
  project("EPSG:4674")
area_empreendimento_luzia <- vect("sig/Shapes/novos/cálculo empreendimento/AII LUZIA 2 E 3.shp") %>%
  project("EPSG:4674")

area_total_oitis <- vect("sig/Shapes/novos/cálculo área total/AREA TOTAL OITIS_.shp") %>%
  project("EPSG:4674")
area_empreendimento_oitis <- vect("sig/Shapes/novos/cálculo empreendimento/AII OITIS.shp") %>%
  project("EPSG:4674")

# Riqueza todas espécies ----
riqueza_bin_chafariz <- rast(lista_chafariz) %>% sum()
riqueza_bin_chafariz <- project(riqueza_bin_chafariz, "EPSG:4674")

riqueza_bin_luzia <- rast(lista_luzia) %>% sum()
riqueza_bin_luzia <- project(riqueza_bin_luzia, "EPSG:4674")

riqueza_bin_oitis <- rast(lista_oitis) %>% sum()
riqueza_bin_oitis <- project(riqueza_bin_oitis, "EPSG:4674")

riqueza_area_total_chafariz <- mask(riqueza_bin_chafariz, area_total_chafariz) %>% trim()
riqueza_empreendimento_chafariz <- mask(riqueza_bin_chafariz, area_empreendimento_chafariz) %>% trim()

riqueza_area_total_luzia <- mask(riqueza_bin_luzia, area_total_luzia) %>% trim()
riqueza_empreendimento_luzia <- mask(riqueza_bin_luzia, area_empreendimento_luzia) %>% trim()

riqueza_area_total_oitis <- mask(riqueza_bin_oitis, area_total_oitis) %>% trim()
riqueza_empreendimento_oitis <- mask(riqueza_bin_oitis, area_empreendimento_oitis) %>% trim()

# Riqueza espécies ameaçadas ----
riqueza_bin_ameacadas_chafariz <- lista_chafariz %>% 
  str_subset(
    str_c(spp_threatened_chafariz, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_ameacadas_chafariz <- project(riqueza_bin_ameacadas_chafariz, "EPSG:4674")

riqueza_bin_ameacadas_luzia <- lista_luzia %>% 
  str_subset(
    str_c(spp_threatened_luzia, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_ameacadas_luzia <- project(riqueza_bin_ameacadas_luzia, "EPSG:4674")

riqueza_bin_ameacadas_oitis <- lista_oitis %>% 
  str_subset(
    str_c(spp_threatened_oitis, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_ameacadas_oitis <- project(riqueza_bin_ameacadas_oitis, "EPSG:4674")

riqueza_ameacadas_area_total_chafariz <- mask(riqueza_bin_ameacadas_chafariz, area_total_chafariz) %>% trim()
riqueza_ameacadas_empreendimento_chafariz <- mask(riqueza_bin_ameacadas_chafariz, area_empreendimento_chafariz) %>% trim()

riqueza_ameacadas_area_total_luzia <- mask(riqueza_bin_ameacadas_luzia, area_total_luzia) %>% trim()
riqueza_ameacadas_empreendimento_luzia <- mask(riqueza_bin_ameacadas_luzia, area_empreendimento_luzia) %>% trim()

riqueza_ameacadas_area_total_oitis <- mask(riqueza_bin_ameacadas_oitis, area_total_oitis) %>% trim()
riqueza_ameacadas_empreendimento_oitis <- mask(riqueza_bin_ameacadas_oitis, area_empreendimento_oitis) %>% trim()

# Riqueza espécies endêmicas ----
riqueza_bin_endemicas_chafariz <- lista_chafariz %>% 
  str_subset(
    str_c(spp_endemic_chafariz, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_endemicas_chafariz <- project(riqueza_bin_endemicas_chafariz, "EPSG:4674")

riqueza_bin_endemicas_luzia <- lista_luzia %>% 
  str_subset(
    str_c(spp_endemic_luzia, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_endemicas_luzia <- project(riqueza_bin_endemicas_luzia, "EPSG:4674")

riqueza_bin_endemicas_oitis <- lista_oitis %>% 
  str_subset(
    str_c(spp_endemic_oitis, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_endemicas_oitis <- project(riqueza_bin_endemicas_oitis, "EPSG:4674")

riqueza_endemicas_area_total_chafariz <- mask(riqueza_bin_endemicas_chafariz, area_total_chafariz) %>% trim()
riqueza_endemicas_empreendimento_chafariz <- mask(riqueza_bin_endemicas_chafariz, area_empreendimento_chafariz) %>% trim()

riqueza_endemicas_area_total_luzia <- mask(riqueza_bin_endemicas_luzia, area_total_luzia) %>% trim()
riqueza_endemicas_empreendimento_luzia <- mask(riqueza_bin_endemicas_luzia, area_empreendimento_luzia) %>% trim()

riqueza_endemicas_area_total_oitis <- mask(riqueza_bin_endemicas_oitis, area_total_oitis) %>% trim()
riqueza_endemicas_empreendimento_oitis <- mask(riqueza_bin_endemicas_oitis, area_empreendimento_oitis) %>% trim()

# Riqueza espécies migratórias ----
riqueza_bin_migratorias_chafariz <- lista_chafariz %>% 
  str_subset(
    str_c(spp_migratory_chafariz, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_migratorias_chafariz <- project(riqueza_bin_migratorias_chafariz, "EPSG:4674")

riqueza_bin_migratorias_luzia <- lista_luzia %>% 
  str_subset(
    str_c(spp_migratory_luzia, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_migratorias_luzia <- project(riqueza_bin_migratorias_luzia, "EPSG:4674")

riqueza_bin_migratorias_oitis <- lista_oitis %>% 
  str_subset(
    str_c(spp_migratory_oitis, collapse = "|")) %>% 
  rast() %>% 
  sum()
riqueza_bin_migratorias_oitis <- project(riqueza_bin_migratorias_oitis, "EPSG:4674")

riqueza_migratorias_area_total_chafariz <- mask(riqueza_bin_migratorias_chafariz, area_total_chafariz) %>% trim()
riqueza_migratorias_empreendimento_chafariz <- mask(riqueza_bin_migratorias_chafariz, area_empreendimento_chafariz) %>% trim()

riqueza_migratorias_area_total_luzia <- mask(riqueza_bin_migratorias_luzia, area_total_luzia) %>% trim()
riqueza_migratorias_empreendimento_luzia <- mask(riqueza_bin_migratorias_luzia, area_empreendimento_luzia) %>% trim()

riqueza_migratorias_area_total_oitis <- mask(riqueza_bin_migratorias_oitis, area_total_oitis) %>% trim()
riqueza_migratorias_empreendimento_oitis <- mask(riqueza_bin_migratorias_oitis, area_empreendimento_oitis) %>% trim()


# Salvar


# riqueza todas spp ----
# Chafariz
writeRaster(riqueza_bin_chafariz, "resultados/areas_riqueza/chafariz_riqueza_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_empreendimento_chafariz, "resultados/areas_riqueza/chafariz_riqueza_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_area_total_chafariz, "resultados/areas_riqueza/chafariz_riqueza_area_total.tif", overwrite = TRUE)

# Luzia
writeRaster(riqueza_bin_luzia, "resultados/areas_riqueza/luzia_riqueza_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_empreendimento_luzia, "resultados/areas_riqueza/luzia_riqueza_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_area_total_luzia, "resultados/areas_riqueza/luzia_riqueza_area_total.tif", overwrite = TRUE)

# Oitis
writeRaster(riqueza_bin_oitis, "resultados/areas_riqueza/oitis_riqueza_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_empreendimento_oitis, "resultados/areas_riqueza/oitis_riqueza_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_area_total_oitis, "resultados/areas_riqueza/oitis_riqueza_area_total.tif", overwrite = TRUE)


# riqueza endemicas ----
# Chafariz
writeRaster(riqueza_bin_endemicas_chafariz, "resultados/areas_riqueza/chafariz_riqueza_endemicas_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_endemicas_empreendimento_chafariz, "resultados/areas_riqueza/chafariz_riqueza_endemicas_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_endemicas_area_total_chafariz, "resultados/areas_riqueza/chafariz_riqueza_endemicas_area_total.tif", overwrite = TRUE)

# Luzia
writeRaster(riqueza_bin_endemicas_luzia, "resultados/areas_riqueza/luzia_riqueza_endemicas_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_endemicas_empreendimento_luzia, "resultados/areas_riqueza/luzia_riqueza_endemicas_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_endemicas_area_total_luzia, "resultados/areas_riqueza/luzia_riqueza_endemicas_area_total.tif", overwrite = TRUE)

# Oitis
writeRaster(riqueza_bin_endemicas_oitis, "resultados/areas_riqueza/oitis_riqueza_endemicas_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_endemicas_empreendimento_oitis, "resultados/areas_riqueza/oitis_riqueza_endemicas_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_endemicas_area_total_oitis, "resultados/areas_riqueza/oitis_riqueza_endemicas_area_total.tif", overwrite = TRUE)

# riqueza ameaçadas ----
# Chafariz
writeRaster(riqueza_bin_ameacadas_chafariz, "resultados/areas_riqueza/chafariz_riqueza_ameacadas_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_ameacadas_empreendimento_chafariz, "resultados/areas_riqueza/chafariz_riqueza_ameacadas_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_ameacadas_area_total_chafariz, "resultados/areas_riqueza/chafariz_riqueza_ameacadas_area_total.tif", overwrite = TRUE)

# Luzia
writeRaster(riqueza_bin_ameacadas_luzia, "resultados/areas_riqueza/luzia_riqueza_ameacadas_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_ameacadas_empreendimento_luzia, "resultados/areas_riqueza/luzia_riqueza_ameacadas_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_ameacadas_area_total_luzia, "resultados/areas_riqueza/luzia_riqueza_ameacadas_area_total.tif", overwrite = TRUE)

# Oitis
writeRaster(riqueza_bin_ameacadas_oitis, "resultados/areas_riqueza/oitis_riqueza_ameacadas_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_ameacadas_empreendimento_oitis, "resultados/areas_riqueza/oitis_riqueza_ameacadas_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_ameacadas_area_total_oitis, "resultados/areas_riqueza/oitis_riqueza_ameacadas_area_total.tif", overwrite = TRUE)


# riqueza migratórias ----
# Chafariz
writeRaster(riqueza_bin_migratorias_chafariz, "resultados/areas_riqueza/chafariz_riqueza_migratorias_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_migratorias_empreendimento_chafariz, "resultados/areas_riqueza/chafariz_riqueza_migratorias_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_migratorias_area_total_chafariz, "resultados/areas_riqueza/chafariz_riqueza_migratorias_area_total.tif", overwrite = TRUE)

# Luzia
writeRaster(riqueza_bin_migratorias_luzia, "resultados/areas_riqueza/luzia_riqueza_migratorias_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_migratorias_empreendimento_luzia, "resultados/areas_riqueza/luzia_riqueza_migratorias_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_migratorias_area_total_luzia, "resultados/areas_riqueza/luzia_riqueza_migratorias_area_total.tif", overwrite = TRUE)

# Oitis
writeRaster(riqueza_bin_migratorias_oitis, "resultados/areas_riqueza/oitis_riqueza_migratorias_caatinga.tif", overwrite = TRUE)
writeRaster(riqueza_migratorias_empreendimento_oitis, "resultados/areas_riqueza/oitis_riqueza_migratorias_empreendimento.tif", overwrite = TRUE)
writeRaster(riqueza_migratorias_area_total_oitis, "resultados/areas_riqueza/oitis_riqueza_migratorias_area_total.tif", overwrite = TRUE)



# fim ----




cols <- c(
  "#03703a", "#2ea355",
  "#7fc864", "#b9e176",
  "#ecf7a4", "#fff2a5",
  "#ffc16b", "#f7864f",
  "#e1422e", "#ae0f1f")
pal <- grDevices::colorRampPalette(cols)

plot(riqueza_bin_chafariz, col = pal(40))
plot(riqueza_bin_luzia, col = pal(40))
plot(riqueza_bin_oitis, col = pal(40))


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










