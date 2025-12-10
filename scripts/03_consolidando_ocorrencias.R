# Carregando os pacotes ---------------------------------------------------
library(readr)
library(dplyr)
library(readxl)

# Importando os pontos de ocorrência --------------------------------------
occ_raw_splink <- read_csv(
  "dados/tabelas/ocorrencias_splink.csv"
)

occ_raw_gbif <- read_csv(
  "dados/tabelas/ocorrencias_gbif.csv",
  show_col_types = FALSE  # silencia mensagens do readr
)

occ_raw_gbif$institutionCode %>% table %>% sort(d=T) %>% head(20)
occ_raw_gbif <- occ_raw_gbif %>% filter(!institutionCode %in% "iNaturalist")

# Conferência rápida (opcional) -------------------------------------------
# occ_raw_splink # scientificname = nome da espécie sem autor
# occ_raw_gbif   # species        = nome da espécie sem autor

# 1. Padroniza speciesLink -----------------------------------------------
# - Aqui: 
#   * speciesLink$scientificname = nome da espécie sem autor
#   * speciesLink$scientificnameauthor = autor
#   * speciesLink$species = epíteto específico (não precisamos na saída)

occ_splink_std <- occ_raw_splink %>%
  # Primeiro, criar internamente as versões que vamos usar
  mutate(
    species_no_author   = scientificname  # nome sem autor (canônico)
  ) %>%
  transmute(
    source               = "speciesLink",
    country              = country,
    stateprovince        = stateprovince,
    institutioncode      = institutioncode,
    collectioncode       = collectioncode,
    kingdom              = kingdom,
    phylum               = phylum,
    order                = NA,
    family               = family,
    genus                = genus,
    # species canônico: nome da espécie SEM autor (vai casar com GBIF$species)
    species              = species_no_author,
    year                 = as.integer(yearcollected),
    month                = NA_integer_,
    decimallongitude     = as.numeric(decimallongitude),
    decimallatitude      = as.numeric(decimallatitude)
  )

# 2. Padroniza GBIF -------------------------------------------------------
# - Aqui:
#   * GBIF$species       = nome da espécie sem autor
#   * GBIF$scientificName = nome com autor (podemos descartar)
#   -> species deve casar com occ_splink_std$species

occ_gbif_std <- occ_raw_gbif %>%
  transmute(
    source               = "GBIF",
    country              = country,
    stateprovince        = stateProvince,
    institutioncode      = institutionCode,
    collectioncode       = collectionCode,
    kingdom              = kingdom,
    phylum               = phylum,
    order                = order,
    family               = family,
    genus                = genus,
    # species: já é o nome SEM autor, compatível com speciesLink
    species              = species,
    year                 = as.integer(year),
    month                = as.integer(month),
    collector            = identifiedBy,
    decimallongitude     = decimalLongitude,
    decimallatitude      = decimalLatitude
  )

# 3. Empilha tudo em uma tabela única -------------------------------------
occ_all <- bind_rows(occ_splink_std, occ_gbif_std)

write_csv(occ_all, "dados/tabelas/ocorrencias_brutas_todas.csv")

# 4. Remover duplicatas (mesmo ponto presente nas duas fontes) ------------
# - Chave de unicidade: species (SEM autor) + coordenadas + ano
occ_all_nodup <- occ_all %>%
  distinct(species, decimallongitude, decimallatitude, year, .keep_all = TRUE)

write_csv(occ_all_nodup, "dados/tabelas/ocorrencias_brutas_todas_nodup.csv")
