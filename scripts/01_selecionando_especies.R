library(readr)
library(dplyr)
library(terra)
library(readxl)

# Espécies ameaçadas que ocorrem na caatinga
lista_spp <- read_excel("dados/tabelas/speciesLink-20251130113337-0026562_all_spp_caating_CR_EN_VU.xlsx")
lista_spp <- 
  lista_spp %>% 
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

# Bacia nivel 5
bho_nv5 <- vect("sig/Shapes/geoft_bho_ach_otto_nivel_05.gpkg")

# Área de impacto do empreendimento
buffer_aii <- vect("sig/Shapes/01. CHAFARIZ/novos/AII_CEC_nova.shp") %>% 
  project(crs("EPSG:4326"))

# Pontos dos aerogeradores
pts_aero <- vect("sig/Shapes/01. CHAFARIZ/prj_Aerogeradores_pt_CHAFARIZ.shp") %>% 
  project(crs("EPSG:4326"))

# Bacias que contem chafariz
bho_sel <- bho_nv5[bho_nv5$wts_cd_pfafstetterbasin %in% c(75622, 75646, 75624, 75628, 75848),]

# plot
plot(bho_sel)
points(lista_spp[,c("longitude", "latitude")], pch = 16, col = 'darkgreen')
plot(buffer_aii, add = T, border = "red", lty = 2, lwd = 2)
points(pts_aero, cex = 0.3, col = "red")

# transformar lista_spp em SpatVector de pontos, usando as colunas longitude/latitude
pts_spp <- vect(
  lista_spp,
  geom = c("longitude", "latitude"),
  crs  = crs(bho_sel)  # garante mesmo SRC dos polígonos
)

# selecionar apenas os pontos que intersectam os polígonos
pts_spp_in <- pts_spp[bho_sel, ]

# voltar para tibble sem a geometria, se quiser só a tabela
lista_spp <- pts_spp_in |> 
  as.data.frame(geom = "XY") |>
  as_tibble() |>
  distinct(scientificname, .keep_all = T)

lista_spp

write_csv(lista_spp, "dados/tabelas/spp_chafariz_bho_nv5.csv")

plet(bho_sel %>% project(crs("EPSG:4326"))) |> 
  points(pts_spp_in %>% project(crs("EPSG:4326")), col = 'red')

plet(bho_sel %>% project(crs("EPSG:4326"))) |> 
  polys(buffer_aii, border = "darkred", popup = F, label = "Buffer") |>
  points(pts_aero, col = "red", label = pts_aero$Name) |>
  
  points(pts_spp %>% project(crs("EPSG:4326")), popup = TRUE, cex = 3,
         label = pts_spp$scientificname
         #clusterOptions = leaflet::markerClusterOptions()
         )


library(terra)
library(dplyr)
library(htmlwidgets)

m <- plet(bho_sel %>% project(crs("EPSG:4326"))) |>
  polys(buffer_aii, border = "darkred", popup = FALSE, label = "Buffer") |>
  points(pts_aero, col = "red", label = pts_aero$Name) |>
  points(
    pts_spp %>% project(crs("EPSG:4326")),
    popup = TRUE,
    cex   = 3,
    label = pts_spp$scientificname
    # clusterOptions = leaflet::markerClusterOptions()
  )

# salvar como HTML (auto-contido)
saveWidget(
  widget       = m,
  file         = "resultados/mapas_dinamicos/mapa_chafariz.html",
  selfcontained = TRUE
)
