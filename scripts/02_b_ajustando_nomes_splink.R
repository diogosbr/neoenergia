library(readr)


splink_files <- list.files(
  "dados/tabelas/splink",
  pattern    = "\\.csv$",
  full.names = TRUE
)

for(i in seq_along(splink_files)){
  
  spp_name <- stringr::str_extract(
    splink_files[i],
    "(?<=ocorrencias_).*(?=_splink)"
  ) %>% str_replace("_", " ")
  
  
  occ_spp <- read_csv(splink_files[i])
  
  occ_spp$scientificname <- spp_name
  
  write_csv(occ_spp,  splink_files[i])
}