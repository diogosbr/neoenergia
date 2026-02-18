# Pacotes ----------------------------------------------------------------------
library(readr)
library(CoordinateCleaner)
library(raster)
library(dismo)
library(dplyr)
library(spThin)

# Entrada ----------------------------------------------------------------------
# Ocorrências já consolidadas e sem duplicatas (todas as fontes)
occ_raw <- read_csv("dados/tabelas/ocorrencias_brutas_todas_nodup.csv")

# Checagem rápida (visual) -----------------------------------------------------
occ_raw

# Selecionar colunas mínimas para limpeza --------------------------------------
# Mantém: espécie pesquisada + coordenadas
occ_coord <-
  occ_raw %>%
  dplyr::select(searched, decimallongitude, decimallatitude) %>%
  filter(!is.na(decimallongitude))

# Limpeza de coordenadas -------------------------------------------------------
# 1) cc_val: validação geral (mantém apenas "clean")
# 2) clean_coordinates: aplica testes específicos (igualdade, outliers, zeros)
occ_clean <- cc_val(
  x       = occ_coord,
  lon     = "decimallongitude",
  lat     = "decimallatitude",
  value   = "clean",
  verbose = TRUE
) %>%
  clean_coordinates(
    species         = "searched",
    lon             = "decimallongitude",
    lat             = "decimallatitude",
    tests           = c("equal", "outliers", "zeros"),
    outliers_method = "quantile",
    outliers_mtp    = 7,
    outliers_size   = 10,
    value           = "clean"
  ) %>%
  distinct()

# Diagnóstico: número de ocorrências antes/depois da limpeza -------------------
nrow(occ_coord)
nrow(occ_clean)

# Lista de espécies após limpeza ------------------------------------------------
spp_names <- unique(occ_clean$searched)

# Rarefação espacial (spThin) --------------------------------------------------
# Objetivo: reduzir autocorrelação espacial (ex.: 10 km entre pontos), por espécie
for (sp in unique(occ_clean$searched)) {
  
  occs <- subset(occ_clean, searched == sp)
  
  thin(
    loc.data       = occs,
    long.col       = "decimallongitude",
    lat.col        = "decimallatitude",
    spec.col       = "searched",
    thin.par       = 10,       # 10 km
    reps           = 1,
    write.files    = TRUE,
    out.dir        = "dados/tabelas/occ_thin/",
    out.base       = paste0(sp, "_thinned"),
    write.log.file = FALSE
  )
}

# Consolidar arquivos gerados pelo spThin --------------------------------------
occ_thin <- list.files("dados/tabelas/occ_thin/", full.names = T, pattern = "csv$") %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  as.data.frame()

write_csv(occ_thin, "dados/tabelas/ocorrencias_thin.csv")

# Filtrar ocorrências válidas no raster ambiental ------------------------------
# Exemplo: usa uma variável preditora para remover pontos fora da cobertura
var1 <- raster("dados/raster/bioclimaticas/brasil_sel/bio_02.tif")

# Remove pontos que caem em células com NA (fora do raster/sem valor)
occ_modelagem <- occ_thin[!is.na(extract(var1, occ_thin[, -1])), ]

# Diagnóstico: total rarefeito vs. total com valores ambientais -----------------
nrow(occ_thin)
nrow(occ_modelagem)

# Saída ------------------------------------------------------------------------
# Tabela final de ocorrências para modelagem (limpas, rarefeitas e com valores)
write_csv(occ_modelagem, "dados/tabelas/ocorrencias_modelagem.csv")
