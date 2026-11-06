#' Título da função calc_hl_curto_ar
#'
#' @description Descrição da função calc_hl_curto_ar.
#'
#' @export

calc_hl_curto_ar <- function(phi_vec, max_l = 120, pi_val = 0.5) {
  k_dim <- length(phi_vec)
  
  # Matriz Companheira A (k x k)
  A <- matrix(0, nrow = k_dim, ncol = k_dim)
  A[1, ] <- phi_vec
  if (k_dim > 1) A[2:k_dim, 1:(k_dim - 1)] <- diag(k_dim - 1)
  
  # Verificar estabilidade do sistema (|lambda_i| < 1)
  if (any(abs(eigen(A)$values) >= 1)) return(NA)
  
  # Trajetória da Resposta ao Impulso simples IR(l) = (A^l)_{1,1}
  IR <- numeric(max_l)
  A_power <- A
  for (l in 1:max_l) {
    IR[l] <- A_power[1, 1]
    A_power <- A_power %*% A
  }
  
  # Escala de curto prazo em AR univariado (s = 1) e valor terminal c_infty = 0
  s_curto <- 1
  em_banda <- abs(IR - 0) <= (pi_val * s_curto)
  
  # Identificação da entrada definitiva na banda [-pi*s, pi*s]
  fora_banda <- which(!em_banda)
  N_curto <- if (length(fora_banda) == 0) 1 else max(fora_banda) + 1
  return(N_curto)
}
