library(terra)
library(dplyr)

dir_bin <- "resultados/chafariz/ensemble_v03/bin/"  # 1 raster binário por espécie (0/1)
area_aoi <- vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") |> project("EPSG:5880") %>% aggregate()

target_crs <- "EPSG:5880"


calc_share_bin <- function(r_bin, aoi){
  # garante CRS compatível
  if (!same.crs(r_bin, aoi)) {
    aoi <- project(aoi, crs(r_bin))
  }
  
  # área fixa da célula (km²) — válido em CRS em metros (ex.: EPSG:5880)
  cell_area_km2 <- abs(prod(res(r_bin))) / 1e6
  
  total_cells <- global(r_bin == 1, "sum", na.rm = TRUE)[1,1]
  total_km2   <- total_cells * cell_area_km2
  
  r_in <- mask(crop(r_bin, aoi), aoi)
  in_cells <- global(r_in == 1, "sum", na.rm = TRUE)[1,1]
  in_km2   <- in_cells * cell_area_km2
  
  share <- if (is.na(total_km2) || total_km2 == 0) NA_real_ else in_km2 / total_km2
  
  list(total_km2 = total_km2, in_km2 = in_km2, share = share)
}

files <- list.files(dir_bin, pattern = "\\.tif$", full.names = TRUE)


area_aoi <- vect("sig/Shapes/01. CHAFARIZ/info_BUFFER_aerogeradores_pl_CHAFARIZ.shp") |>
  project("EPSG:5880")

out_a <- lapply(files, function(f){
  spp <- tools::file_path_sans_ext(basename(f))
  
  r <- rast(f)
  if (!same.crs(r, area_aoi)) {
    r <- project(r, crs(area_aoi), method = "near")
  }
  
  s <- calc_share_bin(r, area_aoi)
  
  data.frame(
    species = spp,
    suitable_km2_total = s$total_km2,
    suitable_km2_aoi   = s$in_km2,
    share_suitable     = s$share
  )
}) |> bind_rows()


out_a %>% arrange(desc(share_suitable)) %>% mutate(share_suitable = share_suitable * 100)

# 
# # área total da AOI (km²)
# aoi_area_km2 <- expanse(rast(files[1]) |> project("EPSG:5880", method="near") |> crop(area_aoi), unit="km") |> 
#   as.numeric()  # alternativa: usar expanse(vect(area_aoi), unit="km") se preferir
# 
# out_a <- out_a %>%
#   mutate(frac_aoi_occupied = suitable_km2_aoi / aoi_area_km2)
