# Pacotes ----------------------------------------------------------------------
library(readr)
library(dplyr)
library(readxl)

# Entradas ---------------------------------------------------------------------
# Ocorrências consolidadas por fonte
arq_splink <- "dados/tabelas/ocorrencias_splink.csv"
arq_gbif   <- "dados/tabelas/ocorrencias_gbif.csv"

# Ocorrências novas (tabela manual)
arq_new    <- "dados/tabelas/Espécies Modelagem BEI.xlsx - Occs_novas_Chafariz.csv"

# Saídas -----------------------------------------------------------------------
out_all    <- "dados/tabelas/ocorrencias_brutas_todas.csv"
out_nodup  <- "dados/tabelas/ocorrencias_brutas_todas_nodup.csv"

# Importando os pontos de ocorrência ------------------------------------------
occ_raw_splink <- read_csv(arq_splink,
                           show_col_types = FALSE  # silencia mensagens do readr
)

occ_raw_gbif <- read_csv(arq_gbif,
                         show_col_types = FALSE  # silencia mensagens do readr
)

# Diagnóstico rápido: principais institutionCode (GBIF) -------------------------
occ_raw_gbif$institutionCode %>% table %>% sort(d = T) %>% head(20)

# Remoção direta: registros provenientes do iNaturalist (GBIF) -----------------
occ_raw_gbif <- occ_raw_gbif %>%
  filter(!institutionCode %in% "iNaturalist")

# Conferência rápida (opcional) -----------------------------------------------
# occ_raw_splink # scientificname = nome da espécie sem autor
# occ_raw_gbif   # species        = nome da espécie sem autor

# 1) Padroniza speciesLink -----------------------------------------------------
# Convenção adotada:
# - speciesLink$scientificname = nome SEM autor (canônico)
# - speciesLink$scientificnameauthor = autor (não usado aqui)
# - speciesLink$species = epíteto específico (não usado na saída)
occ_splink_std <- occ_raw_splink %>%
  mutate(
    # Nome canônico (sem autor), usado para casar com GBIF$species
    species_no_author = scientificname
  ) %>%
  transmute(
    source           = "speciesLink",
    country          = country,
    stateprovince    = stateprovince,
    institutioncode  = institutioncode,
    collectioncode   = collectioncode,
    kingdom          = kingdom,
    phylum           = phylum,
    order            = NA,
    family           = family,
    genus            = genus,
    species          = species_no_author,
    year             = as.integer(yearcollected),
    month            = NA_integer_,
    decimallongitude = as.numeric(decimallongitude),
    decimallatitude  = as.numeric(decimallatitude),
    searched         = searched
  )

# 2) Padroniza GBIF ------------------------------------------------------------
# Convenção adotada:
# - GBIF$species        = nome SEM autor (canônico)
# - GBIF$scientificName = nome COM autor (descartado aqui)
occ_gbif_std <- occ_raw_gbif %>%
  transmute(
    source           = "GBIF",
    country          = country,
    stateprovince    = stateProvince,
    institutioncode  = institutionCode,
    collectioncode   = collectionCode,
    kingdom          = kingdom,
    phylum           = phylum,
    order            = order,
    family           = family,
    genus            = genus,
    species          = species,
    year             = as.integer(year),
    month            = as.integer(month),
    collector        = identifiedBy,
    decimallongitude = decimalLongitude,
    decimallatitude  = decimalLatitude,
    searched         = searched
  )

# 3) Empilha fontes em uma tabela única ----------------------------------------
occ_all <- bind_rows(occ_splink_std, occ_gbif_std)

# Importando ocorrências novas (tabela manual) ---------------------------------
# Observação: locale + col_types garantem leitura correta de decimais
occ_new_raw <- read_csv(
  arq_new,
  locale = locale(decimal_mark = ",", grouping_mark = " "),
  col_types = cols(
    latitude  = col_double(),
    longitude = col_double(),
    .default  = col_guess()
  )
) %>%
  # Mantém apenas colunas relevantes (inclui duplicatas/alternativas se existirem)
  select(Especie, Grupo, `Unidade do registro`, latitude, longitude, Unidade, Lat, Long) %>%
  # Padroniza nomes para casar com o esquema de occ_all
  rename(
    species          = Especie,
    decimallongitude = longitude,
    decimallatitude  = latitude
  ) %>%
  # Marca a fonte como "tabelas"
  mutate(source = "tabelas")

# Acrescenta ocorrências novas à base consolidada ------------------------------
occ_all <- bind_rows(occ_all, occ_new_raw)

# Exporta tabela bruta consolidada ---------------------------------------------
write_csv(occ_all, out_all)

# 4) Remover duplicatas --------------------------------------------------------
# Critério: mesmo ponto + mesma espécie (sem autor)
occ_all_nodup <- occ_all %>%
  distinct(species, decimallongitude, decimallatitude, .keep_all = TRUE)

# Exporta tabela consolidada sem duplicatas ------------------------------------
write_csv(occ_all_nodup, out_nodup)
