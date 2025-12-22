library(terra)

riqueza_caat   <- rast("resultados/riqueza_caatinga.tif")
riqueza_bho    <- rast("resultados/riqueza_bho.tif")
riqueza_buffer <- rast("resultados/riqueza_buffer.tif")

# Extrair valores (removendo NA)
v_caat <- values(riqueza_caat, mat = FALSE, na.rm = T)
v_bho  <- values(riqueza_bho,  mat = FALSE, na.rm = T)
v_buf  <- values(riqueza_buffer, mat = FALSE, na.rm = T)

# Checagens rápidas
summary(v_caat)
summary(v_bho)
summary(v_buf)


perm_test <- function(v_sub, v_ref, stat = mean, n_perm = 9999, seed = 1) {
  set.seed(seed)
  n <- length(v_sub)
  obs <- stat(v_sub)
  
  # Null: sorteia n valores da Caatinga e calcula a estatística
  null <- replicate(n_perm, stat(sample(v_ref, n, replace = FALSE)))
  
  # p-valor bicaudal (comparando afastamento em relação ao centro da null)
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

# Média
res_bho_mean <- perm_test(v_bho, v_caat, stat = mean, n_perm = 9999, seed = 10)
res_buf_mean <- perm_test(v_buf, v_caat, stat = mean, n_perm = 9999, seed = 11)

# Mediana (mais robusta a caudas)
res_bho_med <- perm_test(v_bho, v_caat, stat = median, n_perm = 9999, seed = 12)
res_buf_med <- perm_test(v_buf, v_caat, stat = median, n_perm = 9999, seed = 13)

res_bho_mean
res_buf_mean
res_bho_med
res_buf_med

# Null vs observado (ex.: média do buffer)
hist(res_buf_mean$null, breaks = 50, main = "Null - Buffer", xlab = "Média da riqueza", xlim = (range(res_buf_mean$null) + c(-1,+4)))
abline(v = res_buf_mean$obs, lwd = 2)

hist(res_buf_med$null, breaks = 50, main = "Null - Buffer", xlab = "Média da riqueza", xlim = (range(res_buf_med$null) + c(-1,+4)))
abline(v = res_buf_med$obs, lwd = 2)



# Versão mais conservadora (recomendado): reduzir autocorrelação espacial por blocos ----
fact <- 5  # ajuste (ex.: 5, 10). Depende do seu pixel e da escala ecológica

agg_caat <- aggregate(riqueza_caat, fact = fact, fun = mean, na.rm = TRUE)
agg_bho  <- aggregate(riqueza_bho,  fact = fact, fun = mean, na.rm = TRUE)
agg_buf  <- aggregate(riqueza_buffer, fact = fact, fun = mean, na.rm = TRUE)

va_caat <- values(agg_caat, mat = FALSE); va_caat <- va_caat[!is.na(va_caat)]
va_bho  <- values(agg_bho,  mat = FALSE); va_bho  <- va_bho[!is.na(va_bho)]
va_buf  <- values(agg_buf,  mat = FALSE); va_buf  <- va_buf[!is.na(va_buf)]

res_bho_mean_blk <- perm_test(va_bho, va_caat, stat = mean, n_perm = 9999, seed = 20)
res_buf_mean_blk <- perm_test(va_buf, va_caat, stat = mean, n_perm = 9999, seed = 21)

res_bho_mean_blk
res_buf_mean_blk


par(mfrow = c(1,3))
hist(v_caat, breaks = 50, main = "Caatinga", xlab = "Riqueza potencial")
hist(v_bho,  breaks = 50, main = "5 bacias (nv5)", xlab = "Riqueza potencial")
hist(v_buf,  breaks = 50, main = "Buffer", xlab = "Riqueza potencial")
par(mfrow = c(1,1))

# Null vs observado (ex.: média do buffer)
hist(res_buf_mean_blk$null, breaks = 50, main = "Null (média por bloco) - Buffer", xlab = "Média da riqueza", xlim = (range(res_buf_mean_blk$null) + c(-1,+4)))
abline(v = res_buf_mean_blk$obs, lwd = 2)





# Complemento “habitat crítico”: enriquecimento de pixels no topo da riqueza da Caatinga ----

enrichment_test <- function(v_sub, v_ref, q = 0.95, n_perm = 9999, seed = 1) {
  set.seed(seed)
  thr <- as.numeric(quantile(v_ref, q, na.rm = TRUE))
  obs <- mean(v_sub >= thr)
  
  n <- length(v_sub)
  null <- replicate(n_perm, mean(sample(v_ref, n, replace = FALSE) >= thr))
  
  center <- mean(null)
  p <- (sum(abs(null - center) >= abs(obs - center)) + 1) / (n_perm + 1)
  
  list(
    quantile = q,
    threshold = thr,
    obs_prop = obs,
    null_prop = center,
    diff = obs - center,
    p_value = p,
    null = null
  )
}

enr_bho <- enrichment_test(v_bho, v_caat, q = 0.95, n_perm = 9999, seed = 30)
enr_buf <- enrichment_test(v_buf, v_caat, q = 0.95, n_perm = 9999, seed = 31)

enr_bho
enr_buf
