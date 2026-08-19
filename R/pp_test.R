pp_test <- function(y, lags = "short") {
  # Verifica se o pacote 'urca' está instalado
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("O pacote 'urca' é necessário. Instale com: install.packages('urca')")
  }
  
  # Tipos de especificação e de ajuste de lags no PP
  especificacoes <- c("constant", "trend")
  
  # Mapeamento do nome dos modelos para exibição
  model_names <- c("constant" = "drift", "trend" = "trend")
  
  resultados <- list()
  
  for (espec in especificacoes) {
    
    # Executa o teste Phillips-Perron via urca::ur.pp
    # model: "constant" (com intercepto/drift) ou "trend" (com tendência)
    # type: "Z-tau" ou "Z-alpha" (Z-tau é o mais usual e diretamente comparável ao ADF)
    teste <- urca::ur.pp(y, type = "Z-tau", model = espec, lags = lags)
    
    # Extrai a estatística de teste e os valores críticos
    est_stat <- teste@teststat[1]
    cv_1pct  <- teste@cval[1, "1pct"]
    cv_5pct  <- teste@cval[1, "5pct"]
    cv_10pct <- teste@cval[1, "10pct"]
    
    # Número de lags efetivamente usados na correção de autocorrelação (truncation lag)
    lags_usados <- NA
    
    # Regressão auxiliar usada no PP para extrair parâmetros
    coef_mat    <- teste@testreg$coefficients
    
    # Extrai estatísticas da tendência (tt)
    if (espec == "trend" && "trend" %in% rownames(coef_mat)) {
      tt_coef   <- round(coef_mat["trend", "Estimate"], 4)
      tt_stat   <- round(coef_mat["trend", "t value"], 4)
      tt_pvalue <- round(coef_mat["trend", "Pr(>|t|)"], 4)
    } else {
      tt_coef   <- NA
      tt_stat   <- NA
      tt_pvalue <- NA
    }
    
    # Extrai estatísticas do intercepto
    if ("(Intercept)" %in% rownames(coef_mat)) {
      intercept_coef   <- round(coef_mat["(Intercept)", "Estimate"], 4)
      intercept_stat   <- round(coef_mat["(Intercept)", "t value"], 4)
      intercept_pvalue <- round(coef_mat["(Intercept)", "Pr(>|t|)"], 4)
    } else {
      intercept_coef   <- NA
      intercept_stat   <- NA
      intercept_pvalue <- NA
    }
    
    # Monta a linha do resultado
    resultados[[length(resultados) + 1]] <- data.frame(
      especif           = model_names[espec],
      lag_option        = lags,
      bandwidth_lags    = lags_usados,
      test_stat         = round(est_stat, 4),
      cv_1pct           = round(cv_1pct, 4),
      cv_5pct           = round(cv_5pct, 4),
      cv_10pct          = round(cv_10pct, 4),
      tt_coef           = tt_coef,
      tt_stat           = tt_stat,
      tt_pvalue         = tt_pvalue,
      intercept_coef    = intercept_coef,
      intercept_stat    = intercept_stat,
      intercept_pvalue  = intercept_pvalue,
      stringsAsFactors  = FALSE
    )
  }
  
  # Agrupa os resultados em uma única tabela
  tabela_final <- do.call(rbind, resultados)
  
  return(tabela_final)
}