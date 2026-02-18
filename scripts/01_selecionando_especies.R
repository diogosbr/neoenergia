# Pacotes ----------------------------------------------------------------------
library(readr)
library(dplyr)
library(terra)
library(readxl)
library(htmlwidgets)

# Entradas ---------------------------------------------------------------------
# Tabela de ocorrências (speciesLink) - espécies ameaçadas na Caatinga
arq_spp   <- "dados/tabelas/speciesLink-20251130113337-0026562_all_spp_caating_CR_EN_VU.xlsx"

# Vetores (SIG)
arq_bho   <- "sig/Shapes/geoft_bho_ach_otto_nivel_05.gpkg"                 # Bacia (nível 5)
arq_aii   <- "sig/Shapes/01. CHAFARIZ/novos/AII_CEC_nova.shp"              # Área de impacto (AII)
arq_aero  <- "sig/Shapes/01. CHAFARIZ/prj_Aerogeradores_pt_CHAFARIZ.shp"   # Aerogeradores (pontos)

# Saídas
out_csv   <- "dados/tabelas/spp_chafariz_bho_nv5.csv"
out_html  <- "resultados/mapas_dinamicos/mapa_chafariz.html"

# Carregar e preparar ocorrências ---------------------------------------------
lista_spp <- read_excel(arq_spp) %>% 
  mutate(
    longitude = as.numeric(longitude),
    latitude  = as.numeric(latitude)
  )

# Carregar vetores e padronizar CRS -------------------------------------------
# Bacia (nível 5)
bho_nv5 <- vect(arq_bho)

# Área de impacto do empreendimento (AII) em WGS84 (EPSG:4326)
buffer_aii <- vect(arq_aii) %>%
  project(crs("EPSG:4326"))

# Pontos dos aerogeradores em WGS84 (EPSG:4326)
pts_aero <- vect(arq_aero) %>%
  project(crs("EPSG:4326"))

# Selecionar bacias de interesse ----------------------------------------------
# Bacias que contêm o empreendimento (IDs Pfafstetter)
bho_sel <- bho_nv5[
  bho_nv5$wts_cd_pfafstetterbasin %in% c(75622, 75646, 75624, 75628, 75848),
]

# Plot rápido (checagem visual) ------------------------------------------------
plot(bho_sel)
points(lista_spp[, c("longitude", "latitude")], pch = 16, col = "darkgreen")
plot(buffer_aii, add = T, border = "red", lty = 2, lwd = 2)
points(pts_aero, cex = 0.3, col = "red")

# Filtrar ocorrências por interseção -------------------------------------------
# Converter a tabela de ocorrências em SpatVector de pontos (lon/lat)
pts_spp <- vect(
  lista_spp,
  geom = c("longitude", "latitude"),
  crs  = crs(bho_sel)  # garante o mesmo SRC dos polígonos usados no filtro
)

# Selecionar apenas os pontos que intersectam os polígonos (bho_sel)
pts_spp_in <- pts_spp[bho_sel, ]

# Converter de volta para tibble e remover duplicatas por espécie --------------
lista_spp <- pts_spp_in  %>% 
  as.data.frame(geom = "XY")  %>% 
  as_tibble() %>% 
  distinct(scientificname, .keep_all = T)

# Conferência rápida do resultado em memória
lista_spp

# Exportar tabela --------------------------------------------------------------
write_csv(lista_spp, out_csv)

# Mapas interativos (plet) -----------------------------------------------------
# Visualização rápida: bacias + pontos filtrados
plet(bho_sel %>% project(crs("EPSG:4326"))) |>
  points(pts_spp_in %>% project(crs("EPSG:4326")), col = "red")

# Visualização rápida: bacias + buffer AII + aerogeradores + espécies
plet(bho_sel %>% project(crs("EPSG:4326")))  %>% 
  polys(buffer_aii, border = "darkred", popup = F, label = "Buffer") %>% 
  points(pts_aero, col = "red", label = pts_aero$Name) %>% 
  points(
    pts_spp %>% project(crs("EPSG:4326")),
    popup = TRUE,
    cex   = 3,
    label = pts_spp$scientificname
  )

# Salvar HTML ------------------------------------------------------------------
# Montar o widget e exportar como HTML (auto-contido)
m <- plet(bho_sel %>% project(crs("EPSG:4326"))) %>% 
  polys(buffer_aii, border = "darkred", popup = FALSE, label = "Buffer") %>% 
  points(pts_aero, col = "red", label = pts_aero$Name) %>% 
  points(
    pts_spp %>% project(crs("EPSG:4326")),
    popup = TRUE,
    cex   = 3,
    label = pts_spp$scientificname
  )

saveWidget(
  widget        = m,
  file          = out_html,
  selfcontained = TRUE
)
