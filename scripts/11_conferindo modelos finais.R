# -------------------------------------------------------------------
# Pacotes
# -------------------------------------------------------------------
library(dplyr)
library(readr)
library(stringr)
library(terra)

# -------------------------------------------------------------------
# Espécies alvo (a partir da tabela)
# -------------------------------------------------------------------
spp_list <- read_csv(
  "dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv",
  show_col_types = FALSE
) %>%
  filter(Chafariz == "x") %>%
  select(`Nome válido`) %>%
  distinct() %>%
  pull()

# -------------------------------------------------------------------
# Listar rasters e filtrar pelos ensembles binários
# -------------------------------------------------------------------

# lista_ensemble_bin <- list.files("resultados/chafariz/ensemble_v03/bin//", recursive = TRUE, full.names = TRUE)
# 
# # Observação: mantém seu padrão de filtro via regex com paste(collapse="|")
# lista_ensemble_bin <- lista_ensemble_bin[
#   grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)
# ]

spp_list <- 
  read_csv("dados/tabelas/Espécies Modelagem BEI.xlsx - Chafariz_Luzia.csv", show_col_types = FALSE) %>% 
  filter(Chafariz == "x") %>% select(`Nome válido`) %>% distinct() %>% pull()

lista_geral <- list.files("resultados/chafariz/v06/", recursive = TRUE, full.names = TRUE)

lista_ensemble_bin <- lista_geral[grepl("models_ensemble/caatinga/bin_", lista_geral)]

lista_ensemble_bin <- lista_ensemble_bin[grepl(spp_list %>% paste(collapse = "|"), lista_ensemble_bin)]


# -------------------------------------------------------------------
# Ocorrências
# -------------------------------------------------------------------
pts <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")

# -------------------------------------------------------------------
# Extrair nome da espécie do arquivo:
# "bin_Varronia leucocephala.tif" -> "Varronia leucocephala"
# -------------------------------------------------------------------
spp <- lista_ensemble_bin %>%
  basename() %>%
  str_remove("^bin_") %>%
  str_remove("\\.tif(f)?$")


# -------------------------------------------------------------------
# Plot
# -------------------------------------------------------------------
for (i in seq_along(lista_ensemble_bin)) {
  
  sp <- spp[i]
  
  occ <- pts[pts$species %in% sp, -1]
  
  r <- rast(lista_ensemble_bin[i])
  
  pres_predita <- na.omit(extract(r,occ, ID = F))[,1]
  
  acc <- round(mean(pres_predita),2)
  
  #sum(pres_predita)/length(pres_predita)
  
  plot(r, col = c("gray", "darkgreen"), 
       main = paste(sp, ' | ', nrow(occ), "pontos  |  sensibilidade:", acc))
  points(occ, pch = 16, cex = 0.7)
  
  
  
  Sys.sleep(0.9)
}


sp_v03 <- rast("resultados/chafariz/v03//Tropidurus semitaeniatus/models_ensemble/caatinga/bin_Tropidurus semitaeniatus.tif")

sp_v04 <- rast("resultados/chafariz/v04//Tropidurus semitaeniatus/models_ensemble/caatinga/bin_Tropidurus semitaeniatus.tif")

pres_predita_v03 <- na.omit(extract(sp_v03,occ, ID = F))[,1]
pres_predita_v04 <- na.omit(extract(sp_v04,occ, ID = F))[,1]

acc_v03 <- round(mean(pres_predita_v03),2)
acc_v04 <- round(mean(pres_predita_v04),2)

plot(c(sp_v03, sp_v04))

par(mfrow = c(1,2))
plot(sp_v03, col = c("gray", "darkgreen"), main = paste(sp, ' | ', nrow(occ), "pontos  |  sensibilidade:", acc_v03))
points(occ, pch = 16, cex = 0.7)
plot(sp_v04, col = c("gray", "darkgreen"), main = paste(sp, ' | ', nrow(occ), "pontos  |  sensibilidade:", acc_v04),)
points(occ, pch = 16, cex = 0.7)


