# Função de barra de progresso simples ------------------------------------
progress_bar <- function(i, total_iterations, start_time, bar_width = 30) {
  
  elapsed_time <- Sys.time() - start_time
  estimated_time_left <- (elapsed_time / i) * (total_iterations - i)
  
  pct <- i / total_iterations
  filled <- round(bar_width * pct)
  
  bar <- paste0(
    "[",
    paste0(rep("=", filled), collapse = ""),
    paste0(rep(" ", bar_width - filled), collapse = ""),
    "]"
  )
  
  line <- sprintf(
    "%s %3d%% | elapsed: %s | ETA: %s",
    bar,
    floor(pct * 100),
    format(elapsed_time, digits = 3),
    format(estimated_time_left, digits = 3)
  )
  
  # \r volta para o início da linha e reescreve tudo
  cat("\r", line, "        ")
  
  if (i == total_iterations) cat("\n")
}


# Carregando os pacotes ---------------------------------------------------
library(rgbif)
library(dplyr)
library(readr)


# Lista de espécies -------------------------------------------------------

spp_list <- read_csv("dados/tabelas/spp_chafariz_consolidada.csv")
spp <- spp_list$Espécie %>% unique()

# Colunas desejadas (para garantir sempre o mesmo esquema)
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

start_time <- Sys.time()
n_spp <- length(spp)

for (i in seq_along(spp)) {
  
    sp <- spp[i]
  
  progress_bar(i, n_spp, start_time)
  
  # Obtendo os pontos de ocorrência ---------------------------------------
  occ_raw <- occ_search(scientificName = sp, hasCoordinate = TRUE)
  
  # Caso sem dados (data NULL ou sem linhas) ------------------------------
  if (is.null(occ_raw$data) || nrow(occ_raw$data) == 0) {
    
    occ <- tibble(
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
      scientificName   = sp,
      decimalLongitude = NA_real_,
      decimalLatitude  = NA_real_,
      country          = NA_character_,
      stateProvince    = NA_character_
    )
    
  } else {
    
    # Seleciona apenas as colunas que existem -----------------------------
    occ <- occ_raw$data %>%
      select(any_of(cols_to_keep))
    
    # Garante que todas as colunas de interesse existam -------------------
    missing_cols <- setdiff(cols_to_keep, names(occ))
    if (length(missing_cols) > 0) {
      for (mc in missing_cols) {
        occ[[mc]] <- NA
      }
    }
    
    # Reordena as colunas na ordem padrão
    occ <- occ[, cols_to_keep]
  }
  
  # Salvando a tabela no disco rígido -------------------------------------
  write.csv(
    occ,
    paste0("dados/tabelas/gbif/ocorrencias_", sp, "_gbif.csv"),
    row.names = FALSE
  )
}

close(pb)

gbif_files <- list.files("dados/tabelas/gbif/", pattern = "csv$", full.names = T)
occ_raw_gbif <- lapply(gbif_files, read_csv) %>% bind_rows()
write_csv(occ_raw_gbif, "dados/tabelas/ocorrencias_gbif.csv")
