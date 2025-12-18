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

boxplot(eval_list$auc ~ eval_list$algorithm)
boxplot(eval_list$TSSmax ~ eval_list$algorithm)
boxplot(eval_list$sensitivity ~ eval_list$algorithm)
boxplot(eval_list$specificity ~ eval_list$algorithm)
boxplot(eval_list$accuracy ~ eval_list$algorithm)
boxplot(eval_list$comission ~ eval_list$algorithm)
boxplot(eval_list$omission ~ eval_list$algorithm)

eval_list %>% 
  group_by(algorithm) %>% 
  summarise(
    min = min(sensitivity),
    mean = mean(sensitivity),
    median = median(sensitivity),
    max = max(sensitivity),
    sd = sd(sensitivity))

eval_list %>% 
  group_by(algorithm) %>% 
  summarise(
    min = min(specificity),
    mean = mean(specificity),
    median = median(specificity),
    max = max(specificity),
    sd = sd(specificity))


eval_list %>%
  group_by(algorithm, partiton, species) %>%
  summarise(
    sens = sensitivity,
    spec = specificity,
    .groups = "drop"
  ) %>%
  arrange(spec)
