# Carregando os pacotes ---------------------------------------------------
library(readr)
library(dplyr)
library(readxl)

# Importando os pontos de ocorrência --------------------------------------
occ_raw_splink <- read_excel(
  "dados/tabelas/speciesLink-20251201154658-0010982_occ_all_selected_species.xlsx"
)

occ_raw_gbif <- read_csv(
  "dados/tabelas/ocorrencias_gbif.csv",
  show_col_types = FALSE  # silencia mensagens do readr
)

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
    catalognumber        = catalognumber,
    kingdom              = kingdom,
    phylum               = phylum,
    class                = taxonclass,
    order                = ordem,
    family               = family,
    genus                = genus,
    # species canônico: nome da espécie SEM autor (vai casar com GBIF$species)
    species              = species_no_author,
    typestatus           = typestatus,
    year                 = as.integer(yearcollected),
    month                = NA_integer_,
    collector            = collector,
    decimallongitude     = as.numeric(longitude),
    decimallatitude      = as.numeric(latitude)
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
    catalognumber        = NA_character_,
    kingdom              = kingdom,
    phylum               = phylum,
    class                = NA_character_,
    order                = order,
    family               = family,
    genus                = genus,
    # species: já é o nome SEM autor, compatível com speciesLink
    species              = species,
    typestatus           = NA_character_,
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
