library(readr)
library(dplyr)

output <- "resultados/chafariz/v01/"

# Ocorrências ----
occ_all <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")

# Lista de espécies a modelar
spp <- unique(occ_all$species)

eval_list <- paste0(output, spp, '/models_ensemble/evaluate_', spp, ".csv")[2] %>% 
  lapply(read_csv) %>% 
  bind_rows()

write_csv(eval_list, paste0(output,"evaluate_all_species.csv"))

boxplot(desempenho$auc ~ desempenho$algorithm)
boxplot(desempenho$TSSmax ~ desempenho$algorithm)
boxplot(desempenho$sensitivity ~ desempenho$algorithm)
boxplot(desempenho$specificity ~ desempenho$algorithm)
boxplot(desempenho$accuracy ~ desempenho$algorithm)
boxplot(desempenho$comission ~ desempenho$algorithm)
boxplot(desempenho$omission ~ desempenho$algorithm)
