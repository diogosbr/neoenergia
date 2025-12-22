library(dplyr)
library(terra)
library(tidyterra)
library(sf)
library(tmap)
library(grid)

tmap_mode("plot")

spp_list <- 
  read_csv("dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv", show_col_types = FALSE) %>% 
  filter(Chafariz == "x", grepl(pattern = "Endêmica", Endêmica)) %>% select(`Nome válido`) %>% distinct() %>% pull()

lista_geral <- list.files("resultados/chafariz/v06/", recursive = TRUE, full.names = TRUE)

lista_ensemble_bin <- lista_geral[grepl("models_ensemble/caatinga/bin_", lista_geral)]

lista_ensemble_bin <- lista_ensemble_bin[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)]

lista_ensemble_raw <- lista_geral[grepl("models_ensemble/caatinga/raw_", lista_geral)]

lista_ensemble_raw <- lista_ensemble_raw[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_raw)]





riqueza_bin <- rast(lista_ensemble_bin) |> sum()
riqueza_bin <- project(riqueza_bin, "EPSG:4674")
riqueza_raw <- rast(lista_ensemble_raw) |> sum()
riqueza_raw <- project(riqueza_raw, "EPSG:4674")

# Carregar shapes (corrigindo project() e removendo pipe solto)
brasil <- vect("/home/diogo/Área de trabalho/terrset_lulc_model/data/Limites_IBGE_v2017/Limites_IBGE_v2017/lim_pais_a.shp") |>
  project("EPSG:4674") |>
  filter(nome == "Brasil") |>
  st_as_sf()

caatinga <- vect("/home/smaug/home/diogo/Documentos/github/neoenergia/sig/Shapes/Caatinga shape/Caatinga.shp") |>
  project("EPSG:4674") |>
  st_as_sf()

estado <- vect("/home/diogo/Área de trabalho/terrset_lulc_model/data/estados_brasil/estados_brasil/UFEBRASIL.shp") |>
  project("EPSG:4674") |>
  filter(NM_ESTADO == "BAHIA") |>
  st_as_sf()

area_impacto <- vect("sig/Shapes/01. CHAFARIZ/prj_Aerogeradores_pt_CHAFARIZ.shp") |>
  project("EPSG:4674") |>
  st_as_sf()

buffer_impacto <- vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") |>
  project("EPSG:4674") |>
  st_as_sf()

# Garantir diretório de saída
dir.create("plots", showWarnings = FALSE, recursive = TRUE)

# Expandir bbox (corrigindo typos nos nomes)
area_impacto_bbox_expandida <- st_bbox(area_impacto)
area_impacto_bbox_expandida["xmin"] <- area_impacto_bbox_expandida["xmin"] - 0.10
area_impacto_bbox_expandida["xmax"] <- area_impacto_bbox_expandida["xmax"] + 0.24
area_impacto_bbox_expandida["ymin"] <- area_impacto_bbox_expandida["ymin"] - 0.14
area_impacto_bbox_expandida["ymax"] <- area_impacto_bbox_expandida["ymax"] + 0.08
area_impacto_bbox_expandida <- st_as_sfc(area_impacto_bbox_expandida)

# Detectar se é ponto ou polígono (evita tm_polygons em POINT)
is_point <- any(as.character(st_geometry_type(area_impacto)) %in% c("POINT", "MULTIPOINT"))

# Mapa principal (área de impacto)
cols <- c(
  "#103779", "#1B6F88", "#249E85", "#18C249", "#47DF1A",
  "#C8F513", "#F1D822", "#ECAB21", "#D47631", "#C1543E"
)
pal <- grDevices::colorRampPalette(cols)
breaks <- c(0, 1, 2, 4, 7, 11, 16, 21, 31, 41, 51, 66)

mapa_area_impacto <- tm_shape(qg, bbox = area_impacto_bbox_expandida) +
  tm_raster(
    style   = "fixed",
    breaks  = breaks,
    palette = pal(length(breaks) - 1),
    title   = "Riqueza (nº spp.)"
  ) +
  tm_shape(caatinga) +
  tm_borders(col = "gray40") +
  tm_shape(buffer_impacto) +
  tm_borders(col = "black", lwd = 1.2) +
  tm_shape(area_impacto) +
  {
    if (is_point) {
      tm_symbols(shape = 19, col = "red", size = 0.08)
    } else {
      tm_borders(col = "red", lwd = 1.2)
    }
  } +
  tm_layout(bg.color = "white", frame = TRUE) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))

# Centroide (para as linhas de referência)
centroide <- st_centroid(st_geometry(area_impacto)) |> st_coordinates()

linha_vertical <- st_linestring(rbind(
  c(centroide[1], -33),
  c(centroide[1],  6)
)) |> st_sfc(crs = st_crs(area_impacto))

linha_horizontal <- st_linestring(rbind(
  c(-74, centroide[2]),
  c(-34, centroide[2])
)) |> st_sfc(crs = st_crs(area_impacto))

# Mapa inset (Brasil + Bahia + ponto/centroide)
area_inset_pt <- st_centroid(area_impacto)

mapa_inset <- tm_shape(brasil) +
  tm_polygons(col = "white", border.col = "gray60") +
  tm_shape(estado) +
  tm_polygons(col = "gray90", border.col = "black") +
  tm_shape(area_inset_pt) +
  tm_symbols(shape = 19, col = "red", size = 0.10) +
  tm_shape(linha_horizontal) +
  tm_lines(col = "black", lwd = 1, lty = "dashed") +
  tm_shape(linha_vertical) +
  tm_lines(col = "black", lwd = 1, lty = "dashed") +
  tm_layout(bg.color = "white", frame = TRUE, legend.show = FALSE)

# Função para normalizar dimensões (mantida)
norm_dim <- function(obj){
  bbox <- st_bbox(obj)
  width  <- bbox[["xmax"]] - bbox[["xmin"]]
  height <- bbox[["ymax"]] - bbox[["ymin"]]
  w <- width / max(width, height)
  h <- height / max(width, height)
  unit(c(w, h), "snpc")
}

main_dim  <- norm_dim(area_impacto_bbox_expandida)
inset_dim <- norm_dim(brasil)

main_vp <- viewport(width = main_dim[1], height = main_dim[2])
inset_vp <- viewport(
  width  = inset_dim[1] * 0.35,
  height = inset_dim[2] * 0.35,
  x = unit(1, "npc") - unit(0.5, "cm"),
  y = unit(0.5, "cm"),
  just = c("right", "bottom")
)

# Plotar
grid.newpage()
print(mapa_area_impacto, vp = main_vp)
pushViewport(main_vp)
print(mapa_inset, vp = inset_vp)

# Salvar
png("plots/mapa_impacto_inset.png", width = 2000, height = 1500, res = 300)
grid.newpage()
print(mapa_area_impacto, vp = main_vp)
pushViewport(main_vp)
print(mapa_inset, vp = inset_vp)
dev.off()
