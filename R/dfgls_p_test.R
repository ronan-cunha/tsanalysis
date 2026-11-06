#' Título da função dfgls_p_test
#'
#' @description Descrição da função dfgls_p_test.
#'
#' @export

dfgls_p_test <- function(y, max_lags = 4, crit = 'AIC', type_test = "P-test") {
  
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("O pacote 'urca' é necessário. Instale com: install.packages('urca')")
  }
  
  especificacoes <- c("constant", "trend")
  resultados <- list()
  
  for (espec in especificacoes) {
    
    # Executa o teste ERS (P-test ou DF-GLS) via urca::ur.ers
    teste <- urca::ur.ers(
      y          = y, 
      type       = type_test, 
      model      = espec, 
      lag.max    = max_lags)
    
    # Extrai a estatística de teste e os valores críticos
    est_stat <- as.numeric(teste@teststat)
    cv_1pct  <- teste@cval[1, "1pct"]
    cv_5pct  <- teste@cval[1, "5pct"]
    cv_10pct <- teste@cval[1, "10pct"]
    
    # Número de lags efetivamente selecionados pela função ur.ers
    lags_usados <- teste@lag
    
    # Extrai valor do critério de informação
    crit_val <- extract_crit_info_p(teste)[[crit]]
    
    # Monta a linha do resultado
    resultados[[length(resultados) + 1]] <- data.frame(
      test_type          = type_test,
      especif            = espec,
      info_criterion     = crit,
      info_crit_val      = round(crit_val, 4),
      selected_lags      = lags_usados,
      test_stat          = round(est_stat, 6),
      cv_1pct            = round(cv_1pct, 6),
      cv_5pct            = round(cv_5pct, 6),
      cv_10pct           = round(cv_10pct, 6),
      stringsAsFactors   = FALSE
    )
  }
  
  # Agrupa os resultados em uma tabela final
  tabela_final <- do.call(rbind, resultados)
  rownames(tabela_final) <- NULL
  return(tabela_final)
}

extract_crit_info_p <- function(testreg) {

  # Extrai resíduos e dimensão dos parâmetros
  res <- testreg@y-testreg@yd
  n   <- length(res)
  p   <- 1
  
  # Cálculo da Log-Likelihood Gaussiana
  log_lik <- -n/2 * (log(2 * pi) + log(sum(res^2) / n) + 1)
  
  # Critérios de informação
  aic_val <- -2 * log_lik + 2 * (p + 1)
  bic_val <- -2 * log_lik + log(n) * (p + 1)
  
  return(c(AIC = aic_val, BIC = bic_val))
}
