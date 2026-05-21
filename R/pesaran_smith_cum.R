#' Teste de Significância para Impacto Médio Cumulativo (Pesaran & Smith)
#'
#' @param model Objeto da classe 'varest'.
#' @param target_var String com o nome da variável de interesse.
#' @param y_hat Vetor completo de previsões.
#' @param y Vetor completo de dados reais.
#'
#' @return Um data.frame contendo o impacto, estatística T e p-valor para cada passo h.
#' @export
pesaran_smith_cum <- function(model, target_var, y_hat, y) {

  setup <- ps_setup(model, target_var, y_hat, y)

  # 1. Cálculo Otimizado das Variâncias para todo o horizonte H
  omega_sq_vec <- ps_calc_omega_vec(setup$phis, setup$sigma_eps, setup$s, setup$H, setup$n_vars)

  # 2. Cálculo Vetorizado do Impacto Médio (dhat_h) lidando com NAs
  d_na_rm <- ifelse(is.na(setup$d_total), 0, setup$d_total)
  n_valid <- cumsum(!is.na(setup$d_total))
  dhat_vec <- cumsum(d_na_rm) / ifelse(n_valid == 0, 1, n_valid)

  # 3. Estatísticas Vetorizadas
  h_seq <- 1:setup$H
  stat_t_vec <- (sqrt(h_seq) * dhat_vec) / sqrt(omega_sq_vec)
  p_val_vec <- 2 * (1 - pnorm(abs(stat_t_vec)))

  # Trata possíveis casos onde omega_sq é 0 ou negativo (instabilidade numérica)
  invalido <- omega_sq_vec <= 0
  stat_t_vec[invalido] <- NA
  p_val_vec[invalido] <- NA

  return(data.frame(
    h = h_seq,
    impacto_medio = dhat_vec,
    stat_t = stat_t_vec,
    p_valor = p_val_vec
  ))
}
