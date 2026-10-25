calc_hl_longo_ar <- function(phi_vec, max_l = 120, pi_val = 0.5) {
  k_dim <- length(phi_vec)
  
  # Matriz Companheira A (k x k)
  A <- matrix(0, nrow = k_dim, ncol = k_dim)
  A[1, ] <- phi_vec
  if (k_dim > 1) A[2:k_dim, 1:(k_dim - 1)] <- diag(k_dim - 1)
  
  # Verificar se o sistema é estável (|lambda_i| < 1)
  if (any(abs(eigen(A)$values) >= 1)) return(NA)
  
  # Multiplicador Total de Longo Prazo F_total = [A(I - A)^(-1)]_{1,1}
  I_k <- diag(k_dim)
  F_mat <- A %*% solve(I_k - A)
  F_total <- F_mat[1, 1]
  if (abs(F_total) < 1e-8) return(0)
  
  # Trajetória da Resposta Acumulada CIR(l)
  CIR <- numeric(max_l)
  A_power <- A
  for (l in 1:max_l) {
    IR_l <- A_power[1, 1]
    CIR[l] <- if (l == 1) IR_l else CIR[l - 1] + IR_l
    A_power <- A_power %*% A
  }
  
  # Teste de entrada e permanência na banda [-pi*s, pi*s], com s = |F_total|
  s_longo <- abs(F_total)
  em_banda <- abs(CIR - F_total) <= (pi_val * s_longo)
  
  fora_banda <- which(!em_banda)
  N_longo <- if (length(fora_banda) == 0) 1 else max(fora_banda) + 1
  return(N_longo)
}
