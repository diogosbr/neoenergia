library(terra)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# -----------------------------
# 0) INPUTS
# -----------------------------
file_mapbiomas <- "dados/raster/caatinga_original/Caatinga.tif"

lista_geral <- list.files("resultados/chafariz/v06/", recursive = TRUE, full.names = TRUE)
lista_ensemble_bin <- lista_geral[grepl("models_ensemble/caatinga/bin_", lista_geral)]

# -----------------------------
# 1) FUNÇÕES AUXILIARES (nomes)
# -----------------------------
species_from_file <- function(path) {
  nm <- basename(path)
  nm <- sub("\\.[Tt][Ii][Ff]{1,2}$", "", nm)   # remove .tif/.tiff
  nm <- sub("^bin_", "", nm)                  # remove prefixo bin_
  nm <- gsub("_", " ", nm)                    # underscores -> espaço
  str_squish(nm)
}

norm_key <- function(x) {
  x2 <- iconv(x, to = "ASCII//TRANSLIT")
  x2 <- tolower(x2)
  x2 <- gsub("[^a-z]", "", x2)
  x2
}

# -----------------------------
# 2) CARREGAR DICIONÁRIO LULC
# -----------------------------
allowed_list <- readRDS("resultados/chafariz/lulc_filter/allowed_list.rds") %>%
  mutate(key = norm_key(species))

# -----------------------------
# 3) INVENTÁRIO DE RASTERS SDM + MATCH COM allowed_list
# -----------------------------
files_tbl <- tibble(
  file = lista_ensemble_bin,
  species_guess = map_chr(lista_ensemble_bin, species_from_file),
  key = norm_key(species_guess)
) %>%
  left_join(
    allowed_list %>% select(key, species_ref = species, allowed_ids, n_classes),
    by = "key"
  )

# Normalizar allowed_ids e filtrar entradas problemáticas
files_tbl2 <- files_tbl %>%
  mutate(
    allowed_ids = purrr::map(
      allowed_ids,
      ~ if (is.null(.x) || length(.x) == 0 || all(is.na(.x))) integer(0) else as.integer(.x)
    ),
    n_classes2 = purrr::map_int(allowed_ids, length),
    ok = !is.na(species_ref) & n_classes2 > 0
  )

bad <- files_tbl2 %>% filter(!ok)
if (nrow(bad) > 0) {
  cat("ATENÇÃO: arquivos ignorados por falta de match ou classes vazias:\n")
  print(bad %>% select(file, species_guess, species_ref, n_classes2) %>% head(50))
}

to_run <- files_tbl2 %>% filter(ok)

if (nrow(to_run) == 0) stop("Nenhum raster SDM com match e classes válidas. Verifique o allowed_list e nomes dos arquivos.")

# -----------------------------
# 4) PERFORMANCE: MAPBIOMAS NA GRADE DO SDM (UMA VEZ)
# -----------------------------
template_sdm <- terra::rast(to_run$file[1])
lulc_30m <- terra::rast(file_mapbiomas)

file_mapbiomas_sdmgrid <- "dados/raster/caatinga_original/Caatinga__sdmgrid_30s.tif"
dir.create(dirname(file_mapbiomas_sdmgrid), recursive = TRUE, showWarnings = FALSE)

if (!file.exists(file_mapbiomas_sdmgrid)) {
  cat("Criando MapBiomas reamostrado para a grade do SDM (isso acontece só uma vez)...\n")
  
  if (!terra::same.crs(template_sdm, lulc_30m)) {
    lulc_30m <- terra::project(lulc_30m, template_sdm, method = "near")
  }
  
  lulc_sdmgrid <- terra::resample(lulc_30m, template_sdm, method = "near")
  
  terra::writeRaster(lulc_sdmgrid, file_mapbiomas_sdmgrid,
                     overwrite = TRUE, datatype = "INT2S")
} else {
  cat("Usando MapBiomas já reamostrado (cache):", file_mapbiomas_sdmgrid, "\n")
}

lulc_sdmgrid <- terra::rast(file_mapbiomas_sdmgrid)

if (!terra::compareGeom(template_sdm, lulc_sdmgrid, stopOnError = FALSE)) {
  stop("ERRO: lulc_sdmgrid não está alinhado ao template do SDM. Delete o cache e recrie.")
}

# -----------------------------
# 5) FUNÇÃO RÁPIDA: APLICA MÁSCARA (SEM RESAMPLE POR ESPÉCIE)
# -----------------------------
mask_sdm_by_lulc_fast <- function(sdm_file, allowed_ids, lulc_sdmgrid, out_file,
                                  set_outside = c("NA", "0")) {
  set_outside <- match.arg(set_outside)
  
  sdm <- terra::rast(sdm_file)
  
  if (!terra::compareGeom(sdm, lulc_sdmgrid, stopOnError = FALSE)) {
    stop("SDM com grid diferente do template. Verifique se todos os SDMs foram gerados na mesma grade.")
  }
  
  m <- lulc_sdmgrid %in% allowed_ids
  sdm2 <- if (set_outside == "NA") terra::ifel(m, sdm, NA) else terra::ifel(m, sdm, 0)
  
  terra::writeRaster(sdm2, out_file, overwrite = TRUE)
  invisible(out_file)
}

# -----------------------------
# 6) RODAR LOOP
# -----------------------------
out_dir <- "resultados/chafariz/ensemble_v06/bin_masked_lulc"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

set_outside <- "NA"

results <- purrr::pmap(
  list(to_run$file, to_run$allowed_ids, to_run$species_ref, to_run$n_classes2),
  function(f, ids, sp, ncl) {
    out_file <- file.path(out_dir, paste0("bin_", gsub(" ", "_", sp), "__lulc_masked.tif"))
    cat("Processando:", sp, "| classes:", ncl, "\n")
    mask_sdm_by_lulc_fast(f, ids, lulc_sdmgrid, out_file, set_outside = set_outside)
  }
)

cat("Arquivos gerados:", length(results), "\n")
