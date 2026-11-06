#' Título da função half_lives_ar
#'
#' @description Descrição da função half_lives_ar.
#'
#' @export

half_lives_ar <- function(ar_model,  eta = 0.05){
  k = length(ar_model$coef)
  
  # Vetor de estimativas pontuais (phi_hat) e Matriz de Covariância
  phi_hat <- as.numeric(ar_model$coef[1:k])
  V_phi   <- vcov(ar_model)[1:k, 1:k]
  
  # 3. Estimativa Pontual
  hl_point_lp <- calc_hl_longo_ar(phi_hat)
  hl_point_cp <- calc_hl_curto_ar(phi_hat)
  if(length(phi_hat)==1){
    hl_point_raw = log(0.5)/log(abs(phi_hat))
  } else {
    hl_point_raw = NA
    }
  
  # 4. Amostragem do Elipsoide de Confiança A (Nível 95%)
  cutoff_chi2 <- qchisq(1 - eta, df = k)
  
  set.seed(123)
  n_sims <- 1000
  
  # Decomposição de Cholesky para mapear a elipse
  L <- t(chol(V_phi))
  
  # Gerar pontos uniformemente distribuídos no interior da hiperesfera de raio sqrt(chi2)
  points_sphere <- matrix(rnorm(n_sims * k), ncol = k)
  radii <- runif(n_sims)^(1 / k) * sqrt(cutoff_chi2)
  norms <- sqrt(rowSums(points_sphere^2))
  points_scaled <- (points_sphere / norms) * radii
  
  # Mapear os pontos para o elipsoide dos parâmetros phi em R^k
  phi_samples <- t(apply(points_scaled, 1, function(pt) phi_hat + L %*% pt))
  
  # Short-run
  # 5. Avaliação da Meia-Vida em todo o Conjunto de Confiança A
  hl_sims_cp <- apply(phi_samples, 2, calc_hl_curto_ar)
  hl_validos_cp <- hl_sims_cp[!is.na(hl_sims_cp)]
  
  # Limites conservadores do intervalo [l1, l2] (Proposição 2)
  l1_min_cp <- min(hl_validos_cp)
  l2_max_cp <- max(hl_validos_cp)
  
  # Long run
  # 5. Avaliação da Meia-Vida em todo o Conjunto de Confiança A
  hl_sims_lp <- apply(phi_samples, 2, calc_hl_longo_ar)
  hl_validos_lp <- hl_sims_lp[!is.na(hl_sims_lp)]
  
  # Limites conservadores do intervalo [l1, l2] (Proposição 2)
  l1_min_lp <- min(hl_validos_lp)
  l2_max_lp <- max(hl_validos_lp)
  
  return(dfout <- data.frame('ar' = k,
                             'rho' = phi_hat[1],
                             'meia_vida_ponto_raw' = hl_point_raw,
                             'meia_vida_ponto_cp' = hl_point_cp,
                             'li_cp' = l1_min_cp,
                             'ls_cp' = l2_max_cp,
                             'meia_vida_ponto_lp' = hl_point_lp,
                             'li_lp' = l1_min_lp,
                             'ls_lp' = l2_max_lp))
  
}  
