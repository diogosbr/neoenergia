library(terra)

# ============================================================
# 0) PARÂMETROS
# ============================================================
n_perm <- 1e5
q_top  <- 0.95

# Versão conservadora (redução de autocorrelação): fator de agregação
# (ex.: fact = 5 => blocos de 5x5 pixels)
fact   <- 5

# Saídas
out_dir <- "resultados/testes_riqueza"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1) LEITURA DOS RASTERS
# ============================================================
# riqueza_caat   <- rast("resultados/riqueza_caatinga.tif")
# riqueza_bho    <- rast("resultados/riqueza_bho.tif")
# riqueza_buffer <- rast("resultados/riqueza_buffer.tif")

riqueza_caat   <- rast("resultados/riqueza_caat_ameacadas.tif")
riqueza_buffer <- rast("resultados/riqueza_buffer_ameacadas.tif")

riqueza_caat   <- rast("resultados/riqueza_caat_endemicas.tif")
riqueza_buffer <- rast("resultados/riqueza_buffer_endemicas.tif")

riqueza_caat   <- rast("resultados/riqueza_caat_")
riqueza_buffer <- rast("resultados/riqueza_buffer_migratorias.tif")

# # Checagem conservadora: exige mesma geometria (evita comparação incorreta)
# if (!compareGeom(riqueza_caat, riqueza_bho, stopOnError = FALSE)) {
#   stop("Geometria diferente entre riqueza_caat e riqueza_bho (crs/res/extent/origem). Ajuste antes de seguir.")
# }
# if (!compareGeom(riqueza_caat, riqueza_buffer, stopOnError = FALSE)) {
#   stop("Geometria diferente entre riqueza_caat e riqueza_buffer (crs/res/extent/origem). Ajuste antes de seguir.")
# }

# ============================================================
# 2) EXTRAÇÃO DOS VALORES (SEM NA)
# ============================================================
v_caat <- values(riqueza_caat,   mat = FALSE, na.rm = TRUE)
v_bho  <- values(riqueza_bho,    mat = FALSE, na.rm = TRUE)
v_buf  <- values(riqueza_buffer, mat = FALSE, na.rm = TRUE)

# Garantia de tipo numérico (conservador)
v_caat <- as.numeric(v_caat)
v_bho  <- as.numeric(v_bho)
v_buf  <- as.numeric(v_buf)

# Checagens rápidas
summary(v_caat)
summary(v_bho)
summary(v_buf)

# ============================================================
# 3) FUNÇÕES DE TESTE
# ============================================================
perm_test <- function(v_sub, v_ref, stat = mean, n_perm = 9999, seed = 1) {
  v_sub <- v_sub[!is.na(v_sub)]
  v_ref <- v_ref[!is.na(v_ref)]
  
  n <- length(v_sub)
  if (n == 0) stop("v_sub não tem valores (tudo NA ou vazio).")
  if (n > length(v_ref)) stop("v_sub tem mais amostras do que v_ref; não dá para amostrar sem reposição.")
  
  set.seed(seed)
  
  obs <- stat(v_sub)
  
  null <- replicate(n_perm, stat(sample(v_ref, n, replace = FALSE)))
  
  center <- mean(null)
  p <- (sum(abs(null - center) >= abs(obs - center)) + 1) / (n_perm + 1)
  
  list(
    n_sub = n,
    obs = obs,
    null_mean = center,
    diff = obs - center,
    diff_percent = 100 * (obs - center) / center,
    p_value = p,
    null = null
  )
}

enrichment_test <- function(v_sub, v_ref, q = 0.95, n_perm = 9999, seed = 1) {
  v_sub <- v_sub[!is.na(v_sub)]
  v_ref <- v_ref[!is.na(v_ref)]
  
  n <- length(v_sub)
  if (n == 0) stop("v_sub não tem valores (tudo NA ou vazio).")
  if (n > length(v_ref)) stop("v_sub tem mais amostras do que v_ref; não dá para amostrar sem reposição.")
  
  set.seed(seed)
  
  thr <- as.numeric(quantile(v_ref, q, na.rm = TRUE))
  obs <- mean(v_sub >= thr)
  
  null <- replicate(n_perm, mean(sample(v_ref, n, replace = FALSE) >= thr))
  
  center <- mean(null)
  p <- (sum(abs(null - center) >= abs(obs - center)) + 1) / (n_perm + 1)
  
  # diff_percent aqui é relativo ao null_prop (quando > 0)
  diff_val <- obs - center
  diff_pct <- if (center == 0) NA_real_ else 100 * diff_val / center
  
  list(
    n_sub = n,
    quantile = q,
    threshold = thr,
    obs_prop = obs,
    null_prop = center,
    diff = diff_val,
    diff_percent = diff_pct,
    p_value = p,
    null = null
  )
}

# ============================================================
# 4) TESTES (PIXEL) — MÉDIA E MEDIANA
# ============================================================
res_bho_mean <- perm_test(v_bho, v_caat, stat = mean,   n_perm = n_perm, seed = 10)
res_buf_mean <- perm_test(v_buf, v_caat, stat = mean,   n_perm = n_perm, seed = 11)
res_buf_bho_mean <- perm_test(v_buf, v_bho, stat = mean,   n_perm = n_perm, seed = 11)

res_bho_med  <- perm_test(v_bho, v_caat, stat = median, n_perm = n_perm, seed = 12)
res_buf_med  <- perm_test(v_buf, v_caat, stat = median, n_perm = n_perm, seed = 13)

# ============================================================
# 5) PLOTS DIAGNÓSTICOS (OPCIONAL)
# ============================================================
hist(res_buf_mean$null, breaks = 50, main = "Null - Buffer (média, pixel)",
     xlab = "Estatística (média da riqueza)",
     xlim = (range(res_buf_mean$null) + c(-1, +4)))
abline(v = res_buf_mean$obs, lwd = 2)

hist(res_buf_med$null, breaks = 50, main = "Null - Buffer (mediana, pixel)",
     xlab = "Estatística (mediana da riqueza)",
     xlim = (range(res_buf_med$null) + c(-1, +4)))
abline(v = res_buf_med$obs, lwd = 2)

par(mfrow = c(1,3))
hist(v_caat, breaks = 50, main = "Caatinga (pixel)", xlab = "Riqueza potencial")

abline(v = min(v_buf), lwd = 2, col = 'red')
abline(v = max(v_buf), lwd = 2, col = 'red')

# abline(v = min(v_bho), lwd = 2, col = 'blue')
# abline(v = max(v_bho), lwd = 2, col = 'blue')

hist(v_bho,  breaks = 50, main = "5 bacias (pixel)", xlab = "Riqueza potencial")
hist(v_buf,  breaks = 50, main = "Buffer (pixel)",   xlab = "Riqueza potencial")
par(mfrow = c(1,1))

# ============================================================
# 6) VERSÃO CONSERVADORA — AGREGAÇÃO EM BLOCOS (fact x fact)
# ============================================================
agg_caat <- aggregate(riqueza_caat,   fact = fact, fun = mean, na.rm = TRUE)
agg_bho  <- aggregate(riqueza_bho,    fact = fact, fun = mean, na.rm = TRUE)
agg_buf  <- aggregate(riqueza_buffer, fact = fact, fun = mean, na.rm = TRUE)

va_caat <- values(agg_caat, mat = FALSE); va_caat <- va_caat[!is.na(va_caat)]
va_bho  <- values(agg_bho,  mat = FALSE); va_bho  <- va_bho[!is.na(va_bho)]
va_buf  <- values(agg_buf,  mat = FALSE); va_buf  <- va_buf[!is.na(va_buf)]

va_caat <- as.numeric(va_caat)
va_bho  <- as.numeric(va_bho)
va_buf  <- as.numeric(va_buf)

res_bho_mean_blk <- perm_test(va_bho, va_caat, stat = mean, n_perm = n_perm, seed = 20)
res_buf_mean_blk <- perm_test(va_buf, va_caat, stat = mean, n_perm = n_perm, seed = 21)

hist(res_buf_mean_blk$null, breaks = 50, main = paste0("Null - Buffer (média, blocos fact=", fact, ")"),
     xlab = "Estatística (média da riqueza por bloco)",
     xlim = (range(res_buf_mean_blk$null) + c(-1, +4)))
abline(v = res_buf_mean_blk$obs, lwd = 2)

# ============================================================
# 7) ENRIQUECIMENTO NO TOPO (PIXEL) — q = 0.95 (ajustável)
# ============================================================
enr_bho <- enrichment_test(v_bho, v_caat, q = q_top, n_perm = n_perm, seed = 30)
enr_buf <- enrichment_test(v_buf, v_caat, q = q_top, n_perm = n_perm, seed = 31)

# ============================================================
# 8) TABELA SÍNTESE (TUDO)
#    Colunas pedidas:
#    - quantile, threshold, obs_prop, null_prop, diff, p_value
#    - n_sub, obs, null_mean, diff, diff_percent, p_value
#    (unificadas em uma tabela; onde não se aplica fica NA)
# ============================================================
row_from_perm <- function(id, x) {
  data.frame(
    id = id,
    quantile = NA_real_,
    threshold = NA_real_,
    obs_prop = NA_real_,
    null_prop = NA_real_,
    n_sub = x$n_sub,
    obs = x$obs,
    null_mean = x$null_mean,
    diff = x$diff,
    diff_percent = x$diff_percent,
    p_value = x$p_value,
    stringsAsFactors = FALSE
  )
}

row_from_enr <- function(id, x) {
  data.frame(
    id = id,
    quantile = x$quantile,
    threshold = x$threshold,
    obs_prop = x$obs_prop,
    null_prop = x$null_prop,
    n_sub = x$n_sub,
    obs = NA_real_,
    null_mean = NA_real_,
    diff = x$diff,
    diff_percent = x$diff_percent,
    p_value = x$p_value,
    stringsAsFactors = FALSE
  )
}

tab_sintese <- rbind(
  row_from_perm("bho_mean_pixel",        res_bho_mean),
  row_from_perm("buffer_mean_pixel",     res_buf_mean),
  row_from_perm("bho_median_pixel",      res_bho_med),
  row_from_perm("buffer_median_pixel",   res_buf_med),
  row_from_perm(paste0("bho_mean_block_fact", fact),    res_bho_mean_blk),
  row_from_perm(paste0("buffer_mean_block_fact", fact), res_buf_mean_blk),
  row_from_enr(paste0("bho_enrich_q", q_top, "_pixel"),  enr_bho),
  row_from_enr(paste0("buffer_enrich_q", q_top, "_pixel"), enr_buf)
)

print(tab_sintese)

# (Opcional, mas recomendado) salvar a tabela síntese também
write.csv(tab_sintese, file.path(out_dir, "sintese_testes_riqueza.csv"), row.names = FALSE)

# ============================================================
# 9) METADADOS (CSV) — SIGNIFICADO DE CADA COLUNA DA TABELA SÍNTESE
# ============================================================
meta_cols <- data.frame(
  coluna = names(tab_sintese),
  significado = c(
    "Identificador do teste (área + métrica + escala).",
    "Quantil (q) usado no teste de enriquecimento (proporção acima do topo da Caatinga). NA nos testes de permutação de estatística.",
    "Valor de riqueza no quantil 'quantile' calculado na referência (Caatinga). NA nos testes de permutação de estatística.",
    "Proporção observada de células na subárea com riqueza >= threshold (apenas enriquecimento). NA nos testes de permutação de estatística.",
    "Média da proporção sob a hipótese nula (amostragem aleatória da Caatinga) para enriquecimento. NA nos testes de permutação de estatística.",
    "Tamanho amostral da subárea (número de células/blocos usados no teste).",
    "Estatística observada na subárea (ex.: média/mediana da riqueza). NA nos testes de enriquecimento.",
    "Média da distribuição nula da estatística (permutações). NA nos testes de enriquecimento.",
    "Diferença entre observado e nulo. Para permutação: obs - null_mean. Para enriquecimento: obs_prop - null_prop.",
    "Diferença percentual em relação ao nulo. Para permutação: 100*(obs-null_mean)/null_mean. Para enriquecimento: 100*(obs_prop-null_prop)/null_prop (NA se null_prop=0).",
    "p-valor bicaudal por permutação, comparando o afastamento do observado em relação ao centro da distribuição nula."
  ),
  stringsAsFactors = FALSE
)

write.csv(meta_cols, file.path(out_dir, "sintese_testes_riqueza__metadados_colunas.csv"), row.names = FALSE)
