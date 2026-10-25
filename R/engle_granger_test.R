engle_granger_test <- function(y, x, ic = c("AIC", "BIC"), max_lag = 12) {
  ic <- match.arg(ic)
  
  # 1. Alinhamento dos dados e regressão de longo prazo (MQO)
  df <- na.omit(data.frame(y = as.numeric(y), x = as.numeric(x)))
  T_obs <- nrow(df)
  
  if (T_obs < 20) {
    stop("Número insuficiente de observações para o teste de cointegração.")
  }
  
  fit_ols <- lm(y ~ x, data = df)
  e_hat <- residuals(fit_ols)
  
  df_tst <- urca::ur.df(e_hat, 
                        type = 'none', 
                        lags = max_lag,
                        selectlags = ic)
  
  # 3. Valores Críticos de MacKinnon (2010) para N = 2 com Constante
  # Parâmetros: c(phi_inf, phi_1, phi_2)
  cv_1pct  <- -3.9001 + (-10.534 / T_obs) + (-30.30 / (T_obs^2))
  cv_5pct  <- -3.3377 + (-5.967  / T_obs) + (-8.98  / (T_obs^2))
  cv_10pct <- -3.0462 + (-4.383  / T_obs) + (-6.00  / (T_obs^2))
  
  # 4. Saída estruturada
  data.frame(
    t_stat           = as.numeric(df_tst@teststat),
    cv_1pct          = cv_1pct,
    cv_5pct          = cv_5pct,
    cv_10pct         = cv_10pct,
    cointegrated_5pct = as.numeric(df_tst@teststat) < cv_5pct,
    n_obs            = T_obs,
    beta_x           = as.numeric(coef(fit_ols)["x"]),
    alpha_const      = as.numeric(coef(fit_ols)["(Intercept)"])
  )
}
