#' Teste de Significância para Impacto Médio (Pesaran & Smith) - Horizonte Fixo
#'
#' @param model Objeto da classe 'varest'.
#' @param target_var String com o nome da variável de interesse.
#' @param y_hat Objeto com as previsões.
#' @param y Objeto com os dados reais.
#'
#' @return Lista com impacto, estatística T e p-valor.
#' @export
pesaran_smith <- function(model, target_var, y_hat, y) {

  setup <- ps_setup(model, target_var, y_hat, y)

  # Para o cálculo pontual no horizonte H, usamos as otimizações vetoriais
  # e selecionamos apenas a última posição (passo H)
  omega_sq_vec <- ps_calc_omega_vec(setup$phis, setup$sigma_eps, setup$s, setup$H, setup$n_vars)
  omega_sq_H <- omega_sq_vec[setup$H]

  dhat <- mean(setup$d_total, na.rm = TRUE)

  if (omega_sq_H > 0) {
    stat_T <- (sqrt(setup$H) * dhat) / sqrt(omega_sq_H)
    p_val <- 2 * (1 - pnorm(abs(stat_T)))
  } else {
    stat_T <- NA
    p_val <- NA
  }

  return(list(
    target = target_var,
    impacto_medio = dhat,
    stat_t = stat_T,
    p_valor = p_val
  ))
}
