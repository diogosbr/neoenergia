library(readr)
library(dplyr)

occ_all <- read_csv("dados/tabelas/ocorrencias_brutas_todas.csv")

sort(unique(occ_all$species)) %>% write_csv("dados/tabelas/sinonimos_species.csv")

spp_syn <- read_csv("dados/tabelas/sinonimos_species.csv",
                    show_col_types = FALSE)

occ_all <- occ_all %>%
  left_join(spp_syn, by = c("species" = "original")) %>%
  mutate(
    species_corr = dplyr::coalesce(aceito, species)  # se tiver sinônimo, usa; se não, mantém
  ) %>%
  select(-aceito)
