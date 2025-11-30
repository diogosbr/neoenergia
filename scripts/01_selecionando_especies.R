library(dplyr)
library(terra)
library(readxl)

# Flora que ocorre na caatinga
# lista_spp <- read_excel("dados/tabelas/speciesLink-20251128155518-0011302.xlsx")

# Aves que ocorre na caatinga
# lista_spp <- read_excel("dados/tabelas/speciesLink-20251129164127-0016075.xlsx")

# Espécies ameaçadas que ocorrem na caatinga
lista_spp <- read_excel("dados/tabelas/speciesLink-20251130113337-0026562_all_spp_caating_CR_EN_VU.xlsx")
lista_spp <- 
  lista_spp %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

# Bacia nivel 4
bho_nv4 <- vect("sig/Shapes/geoft_bho_ach_otto_nivel_04.gpkg")

# Bacia nivel 4
chafariz_buffer <- vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") %>% 
  project(crs("EPSG:4326"))

chafariz_pts <- vect("sig/Shapes/01. CHAFARIZ/prj_Aerogeradores_pt_CHAFARIZ.shp") %>% 
  project(crs("EPSG:4326"))

# lista com nome de bacias
bho_xls <- read_excel("sig/Shapes/bacias_nivel_4.xlsx")

# Bacias que contem chafariz
bho_chafariz <- bho_nv4[bho_nv4$wts_cd_pfafstetterbasin %in% c(7584, 7562, 7564),]

# plot
plot(bho_chafariz)
points(lista_spp[,-1], pch = 16, col = 'red')

#extract(bho_chafariz, lista_spp[,-1]) %>% na.omit()


# transformar lista_spp em SpatVector de pontos, usando as colunas longitude/latitude
pts_spp <- vect(
  lista_spp,
  geom = c("longitude", "latitude"),
  crs  = crs(bho_chafariz)  # garante mesmo SRC dos polígonos
)

# selecionar apenas os pontos que intersectam os polígonos
pts_spp_in <- pts_spp[bho_chafariz, ]

# voltar para tibble sem a geometria, se quiser só a tabela
lista_spp_chafariz <- pts_spp_in |> 
  as.data.frame(geom = "XY") |>
  as_tibble() |>
  distinct(scientificname, .keep_all = T)

lista_spp_chafariz


# plot
plot(bho_chafariz)
points(pts_spp_in, pch = 16, col = 'red')

plet(bho_chafariz %>% project(crs("EPSG:4326"))) |> 
  points(pts_spp_in %>% project(crs("EPSG:4326")), col = 'red')

plet(bho_chafariz %>% project(crs("EPSG:4326"))) |> 
  polys(chafariz_buffer, border = "darkred", popup = F, label = "Buffer") |>
  points(chafariz_pts, col = "red", label = chafariz_pts$Name) |>
  
  points(pts_spp %>% project(crs("EPSG:4326")), popup = TRUE, cex = 3,
         label = pts_spp$scientificname
         #clusterOptions = leaflet::markerClusterOptions()
         )


