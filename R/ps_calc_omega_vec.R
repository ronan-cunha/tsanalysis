#' Cálculo iterativo e otimizado da variância Ômega (Pesaran & Smith)
#'
#' @description Calcula a evolução de omega_sq_h de forma cumulativa O(H) em vez de O(H^3).
#' @noRd
#' @keywords internal
ps_calc_omega_vec <- function(phis, sigma_eps, s, H, n_vars) {
  omega_sq_vec <- numeric(H)
  A_k <- matrix(0, nrow = n_vars, ncol = n_vars)
  sum_V <- 0

  # A variância omega_sq para h é a média da soma de k=0 até h-1 de (s' * A_k * Sigma * A_k' * s)
  for (k in 1:H) {
    # No R, phis[,,k] corresponde ao Phi_{k-1} da literatura
    A_k <- A_k + phis[, , k]

    # Matriz de variância do passo k
    V_k <- A_k %*% sigma_eps %*% t(A_k)

    # Extrai o escalar de interesse e acumula
    termo_escalar <- as.numeric(t(s) %*% V_k %*% s)
    sum_V <- sum_V + termo_escalar

    # Salva a variância normalizada pelo horizonte atual (k)
    omega_sq_vec[k] <- sum_V / k
  }

  return(omega_sq_vec)
}
