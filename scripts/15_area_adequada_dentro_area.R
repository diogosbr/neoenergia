library(terra)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# -----------------------------
# 0) INPUTS
# -----------------------------
dir_masked <- "resultados/chafariz/ensemble_v06/bin_masked_lulc/"
dir_bin    <- "resultados/chafariz/ensemble_v06/bin/"
target_crs <- "EPSG:5880"

#area_aoi <- terra::vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") |>
area_aoi <- terra::vect("sig/Shapes/01. CHAFARIZ/novos/AII_CEC_nova.shp") |>
  terra::project(target_crs) |>
  terra::aggregate()  # dissolve/union

# -----------------------------
# 1) FUNÇÕES AUXILIARES
# -----------------------------
species_from_file <- function(path) {
  nm <- tools::file_path_sans_ext(basename(path))
  nm <- sub("^bin_", "", nm)
  nm <- sub("__lulc_masked$", "", nm)
  nm <- gsub("_", " ", nm)
  stringr::str_squish(nm)
}

norm_key <- function(x) {
  x2 <- iconv(x, to = "ASCII//TRANSLIT")
  x2 <- tolower(x2)
  x2 <- gsub("[^a-z]", "", x2)
  x2
}

calc_share_bin <- function(r_bin, aoi){
  # garante CRS compatível
  if (!terra::same.crs(r_bin, aoi)) {
    aoi <- terra::project(aoi, terra::crs(r_bin))
  }
  
  # área fixa da célula (km²) — válido em CRS em metros (ex.: EPSG:5880)
  cell_area_km2 <- abs(prod(terra::res(r_bin))) / 1e6
  
  total_cells <- terra::global(r_bin == 1, "sum", na.rm = TRUE)[1,1]
  total_km2   <- total_cells * cell_area_km2
  
  r_in <- terra::mask(terra::crop(r_bin, aoi), aoi)
  in_cells <- terra::global(r_in == 1, "sum", na.rm = TRUE)[1,1]
  in_km2   <- in_cells * cell_area_km2
  
  share <- if (is.na(total_km2) || total_km2 == 0) NA_real_ else in_km2 / total_km2
  
  list(total_km2 = total_km2, in_km2 = in_km2, share = share)
}

# -----------------------------
# 2) LISTAR ARQUIVOS E PRIORIZAR MASKED
# -----------------------------
files_masked <- list.files(dir_masked, pattern = "\\.tif$", full.names = TRUE)
files_bin    <- list.files(dir_bin,    pattern = "\\.tif$", full.names = TRUE)

files_tbl_all <- bind_rows(
  tibble(file = files_masked, source = "masked"),
  tibble(file = files_bin,    source = "raw")
) %>%
  mutate(
    species  = map_chr(file, species_from_file),
    key      = norm_key(species),
    priority = if_else(source == "masked", 1L, 0L)
  )

# Escolhe 1 arquivo por espécie: masked > raw
files_tbl <- files_tbl_all %>%
  group_by(key) %>%
  arrange(desc(priority), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

# (opcional) diagnóstico: quantas espécies tinham as duas versões?
n_both <- files_tbl_all %>% count(key) %>% filter(n > 1) %>% nrow()
cat("Espécies com versão masked e raw:", n_both, "\n")
cat("Total de espécies processadas:", nrow(files_tbl), "\n")

# -----------------------------
# 3) CALCULAR ÁREAS/SHARE
# -----------------------------
out_a <- purrr::pmap_dfr(
  list(files_tbl$file, files_tbl$species, files_tbl$source),
  function(f, spp, src){
    
    r <- terra::rast(f)
    
    # projetar raster para CRS de área (equal-area) - categórico/binário -> near
    if (!terra::same.crs(r, area_aoi)) {
      r <- terra::project(r, terra::crs(area_aoi), method = "near")
    }
    
    s <- calc_share_bin(r, area_aoi)
    
    data.frame(
      species            = spp,
      source_raster      = src,              # masked ou raw
      suitable_km2_total = s$total_km2,
      suitable_km2_aoi   = s$in_km2,
      share_suitable     = s$share
    )
  }
)

out_a %>%
  arrange(desc(share_suitable)) %>%
  mutate(share_suitable = share_suitable * 100)

out_a %>%
  arrange(desc(share_suitable)) %>%
  mutate(share_suitable = share_suitable * 100) %>% 
  readr::write_csv("resultados/chafariz/ensemble_v06/share_suitable.csv")
