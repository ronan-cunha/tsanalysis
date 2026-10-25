half_lives_ar <- function(ar_model,  eta = 0.05, verbose = FALSE){
  k = length(ar_model$coef)
  
  # Vetor de estimativas pontuais (phi_hat) e Matriz de Covariância
  phi_hat <- as.numeric(ar_model$coef[1:k])
  V_phi   <- vcov(ar_model)[1:k, 1:k]
  
  # 3. Estimativa Pontual
  hl_point <- calc_hl_longo_ar(phi_hat)
  
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
  
  # 5. Avaliação da Meia-Vida em todo o Conjunto de Confiança A
  hl_sims <- apply(phi_samples, 1, calc_hl_longo_ar)
  hl_validos <- hl_sims[!is.na(hl_sims)]
  
  # Limites conservadores do intervalo [l1, l2] (Proposição 2)
  l1_min <- min(hl_validos)
  l2_max <- max(hl_validos)
  
  if(verbose){
    # 6. Exibição dos Resultados
    cat("\n=========================================================\n")
    cat("  RESULTADOS DO AR(", k, ")", " COM IC 95%\n", sep = "")
    cat("=========================================================\n")
    cat("Estimativa Pontual de Phi:                  ", round(phi_hat, 4), "\n")
    cat("Meia-vida de Longo Prazo (Estimativa Pontual): ", hl_point, " meses\n", sep = "")
    cat("Intervalo de Confiança a 95% [l1 - l2]:       [", l1_min, " - ", l2_max, "] meses\n", sep = "")
  }
  return(dfout <- data.frame('ar' = k,
                             'rho' = phi_hat[1],
                             'meia_vida_ponto' = hl_point,
                             'li' = l1_min,
                             'ls' = l2_max))
  
}  
