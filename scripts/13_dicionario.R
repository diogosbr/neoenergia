library(terra)
library(readr)

data_raw <- read_csv(
  "dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv",
  show_col_types = FALSE
)

spp_list <- data_raw %>%
  filter(Chafariz == "x") %>%
  select(`Nome válido`) %>%
  distinct() %>%
  pull()

lista_geral <- list.files("resultados/chafariz/v06/", recursive = TRUE, full.names = TRUE)

lista_ensemble_bin <- lista_geral[grepl("models_ensemble/caatinga/bin_", lista_geral)]

lista_ensemble_bin <- lista_ensemble_bin[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)]

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)

# 1) Ler sua tabela bruta
data_raw <- read_csv(
  "dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv",
  show_col_types = FALSE
)

# 2) Lista de espécies-alvo (Chafariz == "x")
spp_list <- data_raw %>%
  filter(Chafariz == "x") %>%
  transmute(species = `Nome válido`) %>%
  distinct() %>%
  pull(species)

# 3) Identificar colunas de classe (as que têm algum dígito no nome)
#    (Isso pega "3 Forest", "15 Pasture", "20, 35, 39..." etc.)
class_cols <- names(data_raw)[str_detect(names(data_raw), "\\d")]

# 4) Normalizar presença (X / x) e "explodir" os IDs numéricos contidos no nome da coluna
allowed_tbl <- data_raw %>%
  transmute(species = `Nome válido`, across(all_of(class_cols))) %>%
  filter(species %in% spp_list) %>%
  pivot_longer(cols = -species, names_to = "lulc_col", values_to = "val") %>%
  mutate(
    present = !is.na(val) & str_to_upper(as.character(val)) == "X",
    class_id = str_extract_all(lulc_col, "\\d+")
  ) %>%
  filter(present) %>%
  unnest(class_id) %>%
  mutate(class_id = as.integer(class_id)) %>%
  distinct(species, class_id) %>%
  arrange(species, class_id)

# 5) Resumo por espécie (lista de IDs permitidos)
allowed_list <- allowed_tbl %>%
  group_by(species) %>%
  summarise(
    n_classes = n_distinct(class_id),
    allowed_ids = list(sort(unique(class_id))),
    .groups = "drop"
  )

# 6) Sanity checks úteis
cat("Espécies com classes mapeadas:", nrow(allowed_list), "de", length(spp_list), "\n")

no_classes <- setdiff(spp_list, allowed_list$species)
if (length(no_classes) > 0) {
  cat("ATENÇÃO: espécies sem nenhuma classe marcada (X):\n")
  print(no_classes)
}

# 7) Salvar produtos intermediários (recomendado)
dir.create("resultados/chafariz/lulc_filter", recursive = TRUE, showWarnings = FALSE)
write_csv(allowed_tbl,  "resultados/chafariz/lulc_filter/allowed_tbl.csv")
write_csv(allowed_list, "resultados/chafariz/lulc_filter/allowed_list.csv")
saveRDS(allowed_list,   "resultados/chafariz/lulc_filter/allowed_list.rds")

