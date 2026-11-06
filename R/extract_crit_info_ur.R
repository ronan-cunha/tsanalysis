#' Título da função extract_crit_info_ur
#'
#' @description Descrição da função extract_crit_info_ur.
#'
#' @export

extract_crit_info_ur <- function(teste_ur) {
  # Extrai a regressão interna
  testreg <- if (inherits(teste_ur, "ur.ers") || 
                 inherits(teste_ur, "ur.df") || 
                          inherits(teste_ur, "ur.kpss")) teste_ur@testreg else teste_ur
  
  # Extrai resíduos e dimensão dos parâmetros
  res <- testreg$residuals
  n   <- length(res)
  p   <- nrow(testreg$coefficients)
  
  # Cálculo da Log-Likelihood Gaussiana
  log_lik <- -n/2 * (log(2 * pi) + log(sum(res^2) / n) + 1)
  
  # Critérios de informação
  aic_val <- -2 * log_lik + 2 * (p + 1)
  bic_val <- -2 * log_lik + log(n) * (p + 1)
  
  return(c(AIC = aic_val, BIC = bic_val))
}
