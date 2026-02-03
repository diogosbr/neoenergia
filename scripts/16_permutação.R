library(terra)

# ============================================================
# 0) PARÂMETROS
# ============================================================
n_perm <- 1e5
q_top  <- 0.95
fact   <- 5

base_dir <- "/home/smaug/home/diogo/Documentos/github/neoenergia/resultados"
out_dir  <- file.path(base_dir, "testes_riqueza")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# 1) CHECAGEM GEOM (EXATAMENTE COMO VOCÊ PEDIU)
# ============================================================
assert_same_geom <- function(r_ref, r_sub, label) {
  ok <- compareGeom(r_ref, r_sub, stopOnError = FALSE)
  if (!isTRUE(ok)) warning(paste0("Geometria diferente entre referência e ", label,
                                  " (crs/res/extent/origem). Corrija antes de seguir."))
  invisible(TRUE)
  return(NULL)
}

# ============================================================
# 2) FUNÇÕES DE TESTE (CONSERVADORAS)
# ============================================================
perm_test <- function(v_sub, v_ref, stat = mean, n_perm = 9999, seed = 1, keep_null = FALSE) {
  v_sub <- as.numeric(v_sub); v_sub <- v_sub[!is.na(v_sub)]
  v_ref <- as.numeric(v_ref); v_ref <- v_ref[!is.na(v_ref)]
  
  n <- length(v_sub)
  if (n == 0) stop("perm_test: v_sub não tem valores válidos (tudo NA ou vazio).")
  if (n > length(v_ref)) stop("perm_test: v_sub tem mais amostras do que v_ref; não dá para amostrar sem reposição.")
  
  set.seed(seed)
  
  obs  <- stat(v_sub)
  null <- replicate(n_perm, stat(sample(v_ref, n, replace = FALSE)))
  
  center <- mean(null)
  p <- (sum(abs(null - center) >= abs(obs - center)) + 1) / (n_perm + 1)
  
  out <- list(
    n_sub = n,
    obs = obs,
    null_mean = center,
    diff = obs - center,
    diff_percent = if (center == 0) NA_real_ else 100 * (obs - center) / center,
    p_value = p
  )
  if (keep_null) out$null <- null
  out
}

enrichment_test <- function(v_sub, v_ref, q = 0.95, n_perm = 9999, seed = 1, keep_null = FALSE) {
  v_sub <- as.numeric(v_sub); v_sub <- v_sub[!is.na(v_sub)]
  v_ref <- as.numeric(v_ref); v_ref <- v_ref[!is.na(v_ref)]
  
  n <- length(v_sub)
  if (n == 0) stop("enrichment_test: v_sub não tem valores válidos (tudo NA ou vazio).")
  if (n > length(v_ref)) stop("enrichment_test: v_sub tem mais amostras do que v_ref; não dá para amostrar sem reposição.")
  
  set.seed(seed)
  
  thr <- as.numeric(quantile(v_ref, q, na.rm = TRUE))
  obs <- mean(v_sub >= thr)
  
  null <- replicate(n_perm, mean(sample(v_ref, n, replace = FALSE) >= thr))
  
  center <- mean(null)
  p <- (sum(abs(null - center) >= abs(obs - center)) + 1) / (n_perm + 1)
  
  diff_val <- obs - center
  diff_pct <- if (center == 0) NA_real_ else 100 * diff_val / center
  
  out <- list(
    n_sub = n,
    quantile = q,
    threshold = thr,
    obs_prop = obs,
    null_prop = center,
    diff = diff_val,
    diff_percent = diff_pct,
    p_value = p
  )
  if (keep_null) out$null <- null
  out
}

# ============================================================
# 3) UTILITÁRIOS
# ============================================================
safe_rast <- function(path) {
  if (!file.exists(path)) stop(paste0("Arquivo não encontrado: ", path))
  rast(path)
}

get_vals <- function(r) {
  v <- values(r, mat = FALSE)
  v <- as.numeric(v)
  v[!is.na(v)]
}

# Checagem conservadora adequada ao seu caso:
# - NÃO exige mesmo extent/nrow/ncol (buffer é recorte)
# - EXIGE CRS, resolução e origem compatíveis (evita erro)
is_compatible_sampling <- function(r_ref, r_obs, tol = 1e-9) {
  ok_crs <- same.crs(r_ref, r_obs)
  ok_res <- isTRUE(all.equal(res(r_ref), res(r_obs), tolerance = tol))
  ok_org <- isTRUE(all.equal(origin(r_ref), origin(r_obs), tolerance = tol))
  ok_crs && ok_res && ok_org
}

geom_brief <- function(r) {
  list(
    crs = crs(r),
    res = res(r),
    origin = origin(r),
    ext = ext(r)
  )
}

# ============================================================
# 4) CENÁRIOS (APENAS OS QUE VOCÊ PEDIU)
# ============================================================
scenarios <- list(
  list(id = "buffer_vs_bho__todas",            obs = "riqueza_buffer.tif",             ref = "riqueza_bho.tif"),
  list(id = "buffer_vs_caatinga__todas",       obs = "riqueza_buffer.tif",             ref = "riqueza_caatinga.tif"),
  
  list(id = "buffer_vs_bho__endemicas",        obs = "riqueza_buffer_endemicas.tif",   ref = "riqueza_bho_endemicas.tif"),
  list(id = "buffer_vs_caatinga__endemicas",   obs = "riqueza_buffer_endemicas.tif",   ref = "riqueza_caatinga_endemicas.tif"),
  
  list(id = "buffer_vs_bho__ameacadas",        obs = "riqueza_buffer_ameacadas.tif",   ref = "riqueza_bho_ameacadas.tif"),
  list(id = "buffer_vs_caatinga__ameacadas",   obs = "riqueza_buffer_ameacadas.tif",   ref = "riqueza_caatinga_ameacadas.tif"),
  
  list(id = "buffer_vs_bho__migratorias",      obs = "riqueza_buffer_migratorias.tif", ref = "riqueza_bho_migratorias.tif"),
  list(id = "buffer_vs_caatinga__migratorias", obs = "riqueza_buffer_migratorias.tif", ref = "riqueza_caatinga_migratorias.tif")
)

# ============================================================
# 5) FORMATO DA TABELA E FUNÇÕES DE LINHA
# ============================================================
expected_cols <- c(
  "scenario", "scale", "test", "metric",
  "quantile", "threshold", "obs_prop", "null_prop",
  "n_sub", "obs", "null_mean", "diff", "diff_percent", "p_value"
)

row_perm <- function(scenario_id, scale, metric, x) {
  data.frame(
    scenario = scenario_id,
    scale = scale,
    test = "perm",
    metric = metric,
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

row_enr <- function(scenario_id, scale, x) {
  data.frame(
    scenario = scenario_id,
    scale = scale,
    test = "enrichment",
    metric = NA_character_,
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

safe_rbind_tabs <- function(tabs, expected_cols) {
  tabs2 <- tabs[!vapply(tabs, is.null, logical(1))]
  
  if (length(tabs2) == 0) {
    out <- as.data.frame(matrix(nrow = 0, ncol = length(expected_cols)))
    names(out) <- expected_cols
    return(out)
  }
  
  out <- do.call(rbind, tabs2)
  
  missing <- setdiff(expected_cols, names(out))
  if (length(missing) > 0) {
    for (m in missing) out[[m]] <- NA
  }
  
  out <- out[, expected_cols, drop = FALSE]
  out
}

# ============================================================
# 6) FUNÇÃO PRINCIPAL (ESTAVA FALTANDO NO SEU SCRIPT)
# ============================================================
run_scenario <- function(sc, base_dir, out_dir, n_perm, q_top, fact, seed_base = 1000) {
  obs_path <- file.path(base_dir, sc$obs)
  ref_path <- file.path(base_dir, sc$ref)
  
  if (!file.exists(obs_path)) {
    warning(paste0("[SKIP] Obs não existe: ", obs_path))
    return(NULL)
  }
  if (!file.exists(ref_path)) {
    warning(paste0("[SKIP] Ref não existe: ", ref_path))
    return(NULL)
  }
  
  message("Rodando: ", sc$id)
  
  r_obs <- rast(obs_path)
  r_ref <- rast(ref_path)
  
  # Mantém o seu warning "completo"
  assert_same_geom(r_ref, r_obs, label = sc$id)
  
  # Conservador do jeito certo: ignora extent, mas exige CRS/res/origin
  if (!is_compatible_sampling(r_ref, r_obs)) {
    warning(paste0(
      "[SKIP] Incompatível para amostragem (CRS/res/origin) no cenário: ", sc$id, "\n",
      "  REF: ", sc$ref, " | res=", paste(res(r_ref), collapse="x"),
      " origin=", paste(origin(r_ref), collapse=", "), "\n",
      "  OBS: ", sc$obs, " | res=", paste(res(r_obs), collapse="x"),
      " origin=", paste(origin(r_obs), collapse=", ")
    ))
    return(NULL)
  }
  
  v_ref <- get_vals(r_ref)
  v_obs <- get_vals(r_obs)
  
  sid <- match(sc$id, vapply(scenarios, `[[`, character(1), "id"))
  if (is.na(sid)) sid <- 1L
  s0 <- seed_base + 100 * sid
  
  # ===== TESTES (PIXEL) =====
  res_mean <- perm_test(v_obs, v_ref, stat = mean,   n_perm = n_perm, seed = s0 + 11, keep_null = TRUE)
  res_med  <- perm_test(v_obs, v_ref, stat = median, n_perm = n_perm, seed = s0 + 13, keep_null = TRUE)
  enr      <- enrichment_test(v_obs, v_ref, q = q_top, n_perm = n_perm, seed = s0 + 31, keep_null = TRUE)
  
  # ===== VERSÃO CONSERVADORA (BLOCOS) =====
  agg_ref <- aggregate(r_ref, fact = fact, fun = mean, na.rm = TRUE)
  agg_obs <- aggregate(r_obs, fact = fact, fun = mean, na.rm = TRUE)
  
  assert_same_geom(agg_ref, agg_obs, label = paste0(sc$id, "__blocks"))
  
  res_blk <- NULL
  if (is_compatible_sampling(agg_ref, agg_obs)) {
    va_ref <- get_vals(agg_ref)
    va_obs <- get_vals(agg_obs)
    res_blk <- perm_test(va_obs, va_ref, stat = mean, n_perm = n_perm, seed = s0 + 21, keep_null = TRUE)
  } else {
    warning(paste0("[SKIP-BLOCK] Incompatível (CRS/res/origin) após agregação: ", sc$id))
  }
  
  # ===== GRÁFICOS (1 PDF por cenário) =====
  plot_file <- file.path(out_dir, paste0("diagnosticos__", sc$id, ".pdf"))
  pdf(plot_file, width = 10, height = 8)
  
  hist(res_mean$null, breaks = 50,
       main = paste0("Null (perm) — ", sc$id, " — média (pixel)"),
       xlab = "Estatística (média da riqueza)")
  abline(v = res_mean$obs, lwd = 2)
  
  hist(res_med$null, breaks = 50,
       main = paste0("Null (perm) — ", sc$id, " — mediana (pixel)"),
       xlab = "Estatística (mediana da riqueza)")
  abline(v = res_med$obs, lwd = 2)
  
  par(mfrow = c(1, 2))
  hist(v_ref, breaks = 50, main = paste0("Referência — ", basename(sc$ref)), xlab = "Riqueza (pixel)")
  abline(v = enr$threshold, lwd = 2)
  hist(v_obs, breaks = 50, main = paste0("Observado — ", basename(sc$obs)), xlab = "Riqueza (pixel)")
  abline(v = enr$threshold, lwd = 2)
  par(mfrow = c(1, 1))
  
  if (!is.null(res_blk)) {
    hist(res_blk$null, breaks = 50,
         main = paste0("Null (perm) — ", sc$id, " — média (blocos fact=", fact, ")"),
         xlab = "Estatística (média da riqueza por bloco)")
    abline(v = res_blk$obs, lwd = 2)
  } else {
    plot.new()
    title(main = paste0("Blocos (fact=", fact, "): pulado"))
  }
  
  hist(enr$null, breaks = 50,
       main = paste0("Null (enrichment) — ", sc$id, " — proporção >= q", q_top, " (pixel)"),
       xlab = "Proporção de pixels acima do limiar")
  abline(v = enr$obs_prop, lwd = 2)
  
  dev.off()
  
  # ===== TABELA =====
  tab <- rbind(
    row_perm(sc$id, "pixel", "mean",   res_mean),
    row_perm(sc$id, "pixel", "median", res_med),
    row_enr (sc$id, "pixel", enr)
  )
  if (!is.null(res_blk)) {
    tab <- rbind(tab, row_perm(sc$id, paste0("block_fact", fact), "mean", res_blk))
  }
  
  write.csv(tab, file.path(out_dir, paste0("sintese__", sc$id, ".csv")), row.names = FALSE)
  
  rm(res_mean, res_med, enr, res_blk); gc()
  tab
}


# ============================================================
# 7) RODAR E CONSOLIDAR
# ============================================================
tabs <- lapply(
  scenarios, run_scenario,
  base_dir = base_dir,
  out_dir  = out_dir,
  n_perm   = n_perm,
  q_top    = q_top,
  fact     = fact,
  seed_base = 1000
)

tab_all <- safe_rbind_tabs(tabs, expected_cols)

if (nrow(tab_all) > 0) {
  write.csv(tab_all, file.path(out_dir, "sintese__todos_cenarios.csv"), row.names = FALSE)
  print(tab_all)
} else {
  warning("Nenhum cenário gerou resultados (todos SKIP por arquivos faltando ou geometrias incompatíveis).")
}

# ============================================================
# 8) METADADOS (CSV) — SEM DEPENDER DE tab_all TER COLUNAS
# ============================================================
meta_significado <- c(
  "Identificador do cenário (obs vs ref).",
  "Escala de análise: pixel ou bloco (agregado).",
  "Tipo de teste: perm (estatística) ou enrichment (topo).",
  "Métrica usada no teste perm: mean/median (NA no enrichment).",
  "Quantil (q) usado no enrichment (NA no perm).",
  "Limiar (threshold) calculado na referência no quantil q (NA no perm).",
  "Proporção observada no obs com valores >= threshold (apenas enrichment).",
  "Proporção média sob H0 para enrichment (apenas enrichment).",
  "Tamanho amostral do obs (número de células/blocos válidos).",
  "Estatística observada (perm) (NA no enrichment).",
  "Média da distribuição nula da estatística (perm) (NA no enrichment).",
  "Diferença entre observado e nulo (perm: obs-null_mean; enrichment: obs_prop-null_prop).",
  "Diferença percentual em relação ao nulo (NA se centro nulo = 0).",
  "p-valor bicaudal por permutação."
)

meta_cols <- data.frame(
  coluna = expected_cols,
  significado = meta_significado,
  stringsAsFactors = FALSE
)

write.csv(meta_cols, file.path(out_dir, "sintese__metadados_colunas.csv"), row.names = FALSE)
