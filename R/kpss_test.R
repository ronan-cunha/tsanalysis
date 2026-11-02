kpss_test <- function(y, lags = "short") {
  
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("O pacote 'urca' é necessário. Instale com: install.packages('urca')")
  }
  
  # Especificações: 'mu' (estacionariedade em nível) e 'tau' (estacionariedade em tendência)
  especificacoes <- c("mu", "tau")
  resultados <- list()
  
  for (espec in especificacoes) {
    
    # Executa o teste KPSS via urca::ur.kpss
    teste <- urca::ur.kpss(
      y    = y, 
      type = espec, 
      lags = lags
    )
    
    # Extrai estatística de teste e valores críticos
    est_stat  <- as.numeric(teste@teststat)
    cv_10pct  <- teste@cval[1, "10pct"]
    cv_5pct   <- teste@cval[1, "5pct"]
    cv_2_5pct <- teste@cval[1, "2.5pct"]
    cv_1pct   <- teste@cval[1, "1pct"]
    
    espec_desc <- switch(espec, "mu" = "constant", "tau" = "trend")
    
    # Monta a linha do resultado
    resultados[[length(resultados) + 1]] <- data.frame(
      test_type        = "KPSS",
      especif          = espec_desc,
      lags_setting     = as.character(lags),
      selected_lags    = teste@lag,
      test_stat        = round(est_stat, 6),
      cv_10pct         = round(cv_10pct, 4),
      cv_5pct          = round(cv_5pct, 4),
      cv_2.5pct        = round(cv_2_5pct, 4),
      cv_1pct          = round(cv_1pct, 4),
      stringsAsFactors = FALSE
    )
  }
  
  tabela_final <- do.call(rbind, resultados)
  rownames(tabela_final) <- NULL
  return(tabela_final)
}

