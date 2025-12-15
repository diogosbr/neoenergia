library(readr)
library(dplyr)

output <- "resultados/chafariz/v03//"

# Ocorrências ----
occ_all <- read_csv("dados/tabelas/ocorrencias_modelagem.csv")

# Lista de espécies a modelar
spp <- unique(occ_all$species)
spp <- list.dirs(output, recursive = F, full.names = F)

eval_list <- paste0(output, spp, '/models_ensemble/evaluate_', spp, ".csv") %>% 
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

desempenho %>% 
  group_by(algorithm) %>% 
  summarise(
    min = mean(sensitivity),
    mean = mean(sensitivity),
    median = mean(sensitivity),
    max = mean(sensitivity))

desempenho %>% 
  group_by(algorithm) %>% 
  summarise(
    min = mean(specificity),
    mean = mean(specificity),
    median = mean(specificity),
    max = mean(specificity))
