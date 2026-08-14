library(urca)

adf_test <- function(y, max_lags = 10, criterion = 'BIC') {
  # Verifica se o pacote 'urca' está instalado
  if (!requireNamespace("urca", quietly = TRUE)) {
    stop("O pacote 'urca' é necessário. Instale com: install.packages('urca')")
  }
  
  # Tipos de especificação do teste e critérios de seleção de defasagens
  especificacoes <- c("none", "drift", "trend")
  criterios <- c("AIC", "BIC")
  
  resultados <- list()
  
  for (espec in especificacoes) {
    for (crit in criterios) {
      
      # Executa o teste ADF via urca::ur.df
      teste <- urca::ur.df(y, type = espec, lags = max_lags, selectlags = crit)
      
      # Identifica o nome do parâmetro tau correto conforme o modelo
      tau_nome <- switch(espec,
                         "none"  = "tau1",
                         "drift" = "tau2",
                         "trend" = "tau3")
      
      # Extrai a estatística de teste e os valores críticos
      est_stat <- teste@teststat[1, tau_nome]
      cv_1pct  <- teste@cval[tau_nome, "1pct"]
      cv_5pct  <- teste@cval[tau_nome, "5pct"]
      cv_10pct <- teste@cval[tau_nome, "10pct"]
      
      # Número de lags efetivamente escolhido
      lags_usados <- sum(grepl("z.diff.lag", names(teste@testreg[["aliased"]])))
      
      # Extrai estatísticas da tendência (tt) se a especificação for 'trend'
      if (espec == "trend" && "tt" %in% rownames(teste@testreg[["coefficients"]])) {
        coef_mat <- teste@testreg[["coefficients"]]
        tt_coef   <- round(coef_mat["tt", "Estimate"], 4)
        tt_stat   <- round(coef_mat["tt", "t value"], 4)
        tt_pvalue <- round(coef_mat["tt", "Pr(>|t|)"], 4)
      } else {
        tt_coef   <- NA
        tt_stat   <- NA
        tt_pvalue <- NA
      }
      
      # Extrai estatísticas da tendência (tt) se a especificação for 'trend'
      if (espec == "drift" && "(Intercept)" %in% rownames(teste@testreg[["coefficients"]])) {
        coef_mat <- teste@testreg[["coefficients"]]
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
        especif            = espec,
        info_criterion     = crit,
        info_crit_val      = extract_crit_info(teste)[[crit]],
        selected_lags      = lags_usados,
        test_stat          = round(est_stat, 4),
        cv_1pct            = round(cv_1pct, 4),
        cv_5pct            = round(cv_5pct, 4),
        cv_10pct           = round(cv_10pct, 4),
        tt_coef            = tt_coef,
        tt_stat            = tt_stat,
        tt_pvalue          = tt_pvalue,
        intercept_coef     = intercept_coef,
        intercept_stat     = intercept_stat,
        intercept_pvalue   = intercept_pvalue,
        stringsAsFactors   = FALSE
      )
    }
  }
  
  # Agrupa os resultados em uma única tabela
  tabela_final <- do.call(rbind, resultados)
  tabela_final <- tabela_final[tabela_final['info_criterion'] == criterion, ]
  return(tabela_final)
}
extract_crit_info <- function(teste_urdf) {
  
  # Extrai a regressão interna (summary.lm)
  testreg <- if (inherits(teste_urdf, "ur.df")) teste_urdf@testreg else teste_urdf
  
  # Extrai resíduos e dimensão dos parâmetros
  res <- testreg$residuals
  n   <- length(res)
  p   <- nrow(testreg$coefficients) # Número de coeficientes estimados
  
  # Cálculo direto da Log-Likelihood Gaussiana
  log_lik <- -n/2 * (log(2 * pi) + log(sum(res^2) / n) + 1)
  
  # Cálculo dos critérios de informação (incluindo o termo da variância nos parâmetros)
  aic_val <- -2 * log_lik + 2 * (p + 1)
  bic_val <- -2 * log_lik + log(n) * (p + 1)
  
  return(c(AIC = aic_val, BIC = bic_val))
}
