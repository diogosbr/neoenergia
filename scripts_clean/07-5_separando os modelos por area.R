# separando os modelos por area

library(readr)
library(dplyr)
library(stringr)

lista_modelos <- list.files("resultados/tudo/v01/", pattern = "^raw_", recursive = T, full.names = T)
#lista_modelos <- list.files("resultados/tudo/v01/", pattern = "^bin_", recursive = T, full.names = T)

spp_list <- read_csv(
  "dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia_Oitis.csv",
  show_col_types = FALSE
)

spp <- spp_list %>% 
  filter(Oitis == "x") %>% 
  select(`Nome válido`) %>% 
  pull() %>% 
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]


for(i in seq_along(spp)) {
  sp <- spp[i]
  
  mod_i <- lista_modelos %>% stringr::str_subset(sp)
  to <- file.path("resultados/areas/Oitis/", basename(mod_i))
  
  file.copy(mod_i, to)
}
