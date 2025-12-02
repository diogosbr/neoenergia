# Função para adicionar barra de progresso com tempo passado e estimativa de tempo restante
progress_bar <- function(i, total_iterations, start_time) {
  
  # Criar a barra de progresso
  pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
  
  # Calcular o tempo passado
  elapsed_time <- Sys.time() - start_time
  
  # Estimar o tempo restante
  estimated_time_left <- (elapsed_time / i) * (total_iterations - i)
  
  # Exibir o progresso com tempo passado e tempo estimado restante
  time_passed_str <- format(elapsed_time, digits = 3)
  time_left_str <- format(estimated_time_left, digits = 3)
  
  # Atualizar a barra de progresso
  #setTxtProgressBar(pb, i)
  cat("Time elapsed: ", time_passed_str, " | Estimated time left: ", time_left_str, "     \r")
}


# Carregando os pacotes ---------------------------------------------------
library(rgbif)
library(dplyr)
library(readr)


# Lista de espécies -------------------------------------------------------

spp_list <- read_csv("dados/tabelas/spp_chafariz_consolidada.csv")
spp <- spp_list$Espécie %>% unique()

start_time <- Sys.time()

#for(sp in spp){
for(i in seq_along(spp)){
  
  progress_bar(i, length(spp), start_time)
  
  # Obtendo os pontos de ocorrência -----------------------------------------
  occ_raw <- occ_search(scientificName = spp[i], hasCoordinate = TRUE)
  
  if(is.null(occ_raw$data)){
    occ <- tibble(kingdom = NA, 
                  phylum = NA, 
                  order = NA,  
                  family = NA, 
                  genus = NA,
                  year = NA, 
                  month = NA,
                  institutionCode = NA,
                  collectionCode = NA,
                  identifiedBy = NA,
                  species = NA,
                  scientificName = spp[i],
                  decimalLongitude = NA,
                  decimalLatitude = NA,
                  country = NA, 
                  stateProvince = NA)
    write.csv(occ, paste0("dados/tabelas/gbif/ocorrencias_", spp[i],"_gbif.csv"), row.names = FALSE)
    next
  }
  

  # Selecionado os dados de interesse
  occ <- occ_raw$data %>% select(kingdom, phylum, order,  family, genus,
                                 year, month,
                                 institutionCode,
                                 collectionCode,
                                 identifiedBy,
                                 species,
                                 scientificName,
                                 decimalLongitude,
                                 decimalLatitude,
                                 country, stateProvince)

  # Salvando a tabela no disco rígido
  write.csv(occ, paste0("dados/tabelas/gbif/ocorrencias_", spp[i],"_gbif.csv"), row.names = FALSE)
}
