library(readr)
library(dplyr)
library(stringr)

# Lê a tabela consolidada de ocorrências (já com speciesLink + GBIF padronizados)
occ_all <- read_csv("dados/tabelas/ocorrencias_brutas_todas.csv")

# Dá uma “limpada” básica em species:
# - remove espaços em branco no início/fim
occ_all <- occ_all %>%
  mutate(species = str_trim(species))

#occ_all$species %>% unique() %>% sort() %>% as_tibble() %>% rename(original=value) %>% write_csv("dados/tabelas/sinonimos_species.csv")

# Lê a tabela de sinônimos/simplificações de nomes de espécies.
# Essa tabela deve ter, no mínimo, as colunas:
#   - original: como o nome aparece em occ_all$species
#   - aceito  : como você quer que ele fique na versão consolidada
spp_syn <- read_csv("dados/tabelas/sinonimos_species.csv",
                    show_col_types = FALSE) %>% 
  # normaliza nomes de colunas (minúsculas, sem acento, sem espaço, etc.)
  janitor::clean_names() %>% 
  # garante que as strings também estejam “limpas”
  mutate(
    original = str_trim(original),
    aceito   = str_trim(aceito)
  )

# Conferência rápida dos nomes “aceitos” que você definiu no dicionário
sort(unique(spp_syn$aceito))

# Aplica o dicionário de sinônimos:
# - faz um left_join ligando occ_all$species ao spp_syn$original
# - se existir um “aceito”, substitui o species por ele
# - se não existir, mantém o species original
occ_all <- occ_all %>%
  left_join(spp_syn, by = c("species" = "original")) %>%
  mutate(
    species = dplyr::coalesce(aceito, species)  # se tiver sinônimo, usa; senão mantém
  ) %>%
  filter(aceito != 'remover') %>% 
  select(-aceito)  # remove a coluna auxiliar, já incorporada em species



# Checa a lista final de nomes de espécies consolidados
sort(unique(occ_all$species))

# Checa especificamente o grupo de interesse (aqui, Cereus ... jamacaru)
# para ver se todas as variantes foram colapsadas corretamente
sort(unique(occ_all$species[grepl("^Cereus.*jamacaru", occ_all$species)]))

write_csv(occ_all, "dados/tabelas/ocorrencias_limpas.csv")
