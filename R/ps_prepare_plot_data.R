#' Prepara dados de Pesaran & Smith para Plotagem
#'
#' @description Adiciona colunas de erro padrão e intervalos de confiança (95%).
#' @param df Dataframe retornado por calc_pesaran_smith_cum.
#' @noRd
ps_prepare_plot_data <- function(df) {
  # Recuperamos o erro padrão implícito: SE = impacto / stat_t
  # Se stat_t for 0 ou NA, o SE é tratado como NA
  df$se <- abs(df$impacto_medio / df$stat_t)

  # Intervalo de Confiança de 95% (1.96 * SE)
  df$lwr <- df$impacto_medio - (1.96 * df$se)
  df$upr <- df$impacto_medio + (1.96 * df$se)

  # Flag de significância para cores
  df$significant <- ifelse(df$p_valor < 0.05, "Sim", "Não")

  return(df)
}
