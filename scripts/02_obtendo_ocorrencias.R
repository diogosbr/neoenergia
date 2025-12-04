# =====================================================================
# Funções auxiliares
# =====================================================================

# Formata tempos tipo "2 min 30 sec" ou "1 h 05 min"
format_time_hms <- function(x) {
  total_sec <- as.numeric(x, units = "secs")
  if (is.na(total_sec)) return("NA")
  
  total_sec <- round(total_sec)
  hours <- total_sec %/% 3600
  rem   <- total_sec %% 3600
  mins  <- rem %/% 60
  secs  <- rem %% 60
  
  parts <- character(0)
  if (hours > 0) parts <- c(parts, sprintf("%d h", hours))
  if (mins  > 0) parts <- c(parts, sprintf("%d min", mins))
  if (secs  > 0 || length(parts) == 0) parts <- c(parts, sprintf("%d sec", secs))
  
  paste(parts, collapse = " ")
}

# Barra de progresso simples, baseada em número de iterações
progress_bar <- function(i, total_iterations, start_time, bar_width = 30) {
  
  if (total_iterations <= 0L || i <= 0L) {
    return(invisible(NULL))
  }
  
  elapsed_time <- Sys.time() - start_time
  estimated_time_left <- (elapsed_time / i) * (total_iterations - i)
  
  pct    <- i / total_iterations
  filled <- round(bar_width * pct)
  
  bar <- paste0(
    "[",
    paste(rep("=", filled), collapse = ""),
    paste(rep(" ", max(bar_width - filled, 0L)), collapse = ""),
    "]"
  )
  
  line <- sprintf(
    "%s %3d%% | elapsed: %s | ETA: %s",
    bar,
    floor(pct * 100),
    format_time_hms(elapsed_time),
    format_time_hms(estimated_time_left)
  )
  
  cat("\r", line, "        ")
  
  if (i == total_iterations) cat("\n")
  
  invisible(NULL)
}

# Gera um nome de arquivo "seguro" a partir do nome da espécie
sanitize_species_name <- function(x) {
  x <- trimws(x)
  # Substitui tudo que não for letra, número ou "_" por "_"
  x <- gsub("[^[:alnum:]_]+", "_", x)
  if (nchar(x) == 0) x <- "sem_nome"
  x
}

# Cria uma linha vazia padrão para GBIF, preservando tipos
make_empty_gbif_row <- function(scientific_name) {
  tibble::tibble(
    kingdom          = NA_character_,
    phylum           = NA_character_,
    order            = NA_character_,
    family           = NA_character_,
    genus            = NA_character_,
    year             = NA_integer_,
    month            = NA_integer_,
    institutionCode  = NA_character_,
    collectionCode   = NA_character_,
    identifiedBy     = NA_character_,
    species          = NA_character_,
    scientificName   = scientific_name,
    decimalLongitude = NA_real_,
    decimalLatitude  = NA_real_,
    country          = NA_character_,
    stateProvince    = NA_character_
  )
}

# =====================================================================
# Pacotes e configurações
# =====================================================================

library(rgbif)
library(dplyr)
library(readr)

devtools::load_all("../splink/")

api_key <- Sys.getenv("splink_api_key")
if (identical(api_key, "") || is.na(api_key)) {
  stop("Defina a variável de ambiente 'splink_api_key' antes de rodar o script.")
}

# Garante que as pastas de saída existam (não falha se já existirem)
dir.create("dados/tabelas/gbif",   recursive = TRUE, showWarnings = FALSE)
dir.create("dados/tabelas/splink", recursive = TRUE, showWarnings = FALSE)

# Limites de download (ajuste se quiser ser mais/menos conservador)
gbif_limit   <- 50000L
splink_limit <- 50000L

# =====================================================================
# Lista de espécies
# =====================================================================

spp_list <- read_csv(
  "dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv",
  show_col_types = FALSE
)

spp <- spp_list$`Nome válido` %>%
  unique() %>%
  na.omit() %>%
  trimws() %>%
  .[. != ""]

n_spp <- length(spp)

if (n_spp == 0L) {
  stop("Nenhuma espécie válida encontrada na coluna 'Nome válido'.")
}

# Colunas desejadas (para GBIF) – esquema fixo
cols_to_keep <- c(
  "kingdom", "phylum", "order", "family", "genus",
  "year", "month",
  "institutionCode",
  "collectionCode",
  "identifiedBy",
  "species",
  "scientificName",
  "decimalLongitude",
  "decimalLatitude",
  "country", "stateProvince"
)

# Tipos esperados por coluna (para as colunas que faltarem)
char_cols <- c(
  "kingdom", "phylum", "order", "family", "genus",
  "institutionCode", "collectionCode", "identifiedBy",
  "species", "scientificName",
  "country", "stateProvince"
)
int_cols <- c("year", "month")
num_cols <- c("decimalLongitude", "decimalLatitude")

# =====================================================================
# Loop principal sobre as espécies
# =====================================================================

start_time <- Sys.time()

for (i in seq_along(spp)) {
  
  sp <- spp[i]
  
  # Atualiza barra de progresso
  progress_bar(i, n_spp, start_time)
  
  # Segurança extra contra entradas vazias
  if (is.na(sp) || !nzchar(sp)) next
  
  # Nome "seguro" para o arquivo (evita problemas com espaços, acentos etc.)
  sp_safe <- sanitize_species_name(sp)
  
  # -------------------------------------------------------------------
  # 1) Obtendo os pontos de ocorrência do GBIF
  # -------------------------------------------------------------------
  occ_raw <- tryCatch(
    occ_search(
      scientificName = sp,
      hasCoordinate  = TRUE,
      limit          = gbif_limit
    ),
    error = function(e) {
      warning(sprintf("Erro em occ_search() para '%s': %s", sp, conditionMessage(e)))
      # Mantém a estrutura esperada
      list(data = NULL)
    }
  )
  
  # Caso sem dados (data NULL ou sem linhas)
  if (is.null(occ_raw$data) || nrow(occ_raw$data) == 0) {
    
    occ <- make_empty_gbif_row(sp)
    
  } else {
    
    # Seleciona apenas as colunas que existem
    occ <- occ_raw$data %>%
      select(any_of(cols_to_keep))
    
    # Garante que todas as colunas de interesse existam,
    # criando as que faltarem com o tipo adequado
    missing_cols <- setdiff(cols_to_keep, names(occ))
    
    if (length(missing_cols) > 0) {
      for (mc in missing_cols) {
        if (mc %in% char_cols) {
          occ[[mc]] <- NA_character_
        } else if (mc %in% int_cols) {
          occ[[mc]] <- NA_integer_
        } else if (mc %in% num_cols) {
          occ[[mc]] <- NA_real_
        } else {
          occ[[mc]] <- NA
        }
      }
    }
    
    # Reordena as colunas na ordem padrão
    occ <- occ[, cols_to_keep]
  }
  
  # Salvando a tabela do GBIF no disco rígido
  write_csv(
    occ,
    file = file.path("dados", "tabelas", "gbif", paste0("ocorrencias_", sp_safe, "_gbif.csv"))
  )
  
  # -------------------------------------------------------------------
  # 2) Obtendo os pontos de ocorrência do speciesLink (splink)
  # -------------------------------------------------------------------
  resultados <- tryCatch(
    get_data(
      list_data = list(
        scientificName = sp,
        limit          = splink_limit,
        synonyms       = c("dsmz", "moure", "flora2020", "gbif")
      ),
      apikey = api_key
    ),
    error = function(e) {
      warning(sprintf("Erro em get_data() para '%s': %s", sp, conditionMessage(e)))
      NULL
    }
  )
  
  # Se não vier nada do speciesLink, salva um registro mínimo
  if (is.null(resultados) || nrow(resultados) == 0) {
    resultados <- tibble::tibble(
      scientificName = sp,
      .source        = "splink",
      .obs           = 0L
    )
  }
  
  write_csv(
    resultados,
    file = file.path("dados", "tabelas", "splink", paste0("ocorrencias_", sp_safe, "_splink.csv"))
  )
}

cat("\nDownload individual por espécie concluído.\n")

# =====================================================================
# Consolidação dos arquivos em tabelas únicas (GBIF + speciesLink)
# =====================================================================

# GBIF ------------------------------------------------------------------

gbif_files <- list.files(
  "dados/tabelas/gbif",
  pattern    = "\\.csv$",
  full.names = TRUE
)

if (length(gbif_files) > 0) {
  occ_raw_gbif <- gbif_files %>%
    lapply(read_csv, show_col_types = FALSE) %>%
    bind_rows()
  
  write_csv(occ_raw_gbif, "dados/tabelas/ocorrencias_gbif.csv")
}

# speciesLink -----------------------------------------------------------

splink_files <- list.files(
  "dados/tabelas/splink",
  pattern    = "\\.csv$",
  full.names = TRUE
)

if (length(splink_files) > 0) {
  occ_raw_splink <- splink_files %>%
    lapply(read_csv, show_col_types = FALSE) %>%
    bind_rows()
  
  write_csv(occ_raw_splink, "dados/tabelas/ocorrencias_splink.csv")
}

cat(
  "Arquivos consolidados salvos em:\n",
  "- dados/tabelas/ocorrencias_gbif.csv\n",
  "- dados/tabelas/ocorrencias_splink.csv\n"
)
