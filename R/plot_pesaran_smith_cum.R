#' Plot do Impacto Médio Cumulativo
#'
#' @param df_cum Dataframe retornado por calc_pesaran_smith_cum.
#' @param title Título opcional para o gráfico.
#' @return Um objeto ggplot2.
#' @export
plot_pesaran_smith_cum <- function(df_cum, title = "Impacto Médio Cumulativo (Pesaran & Smith)") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("O pacote 'ggplot2' é necessário para esta função.")
  }

  # Prepara os dados internamente
  plot_data <- ps_prepare_plot_data(df_cum)

  ggplot2::ggplot(plot_data, ggplot2::aes(x = h, y = impacto_medio)) +
    # Faixa de Confiança (Sombreado)
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr),
                         fill = "steelblue", alpha = 0.15) +
    # Linha de referência (Zero)
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    # Linha do Impacto
    ggplot2::geom_line(ggplot2::aes(color = "Impacto Médio"), size = 1) +
    # Pontos coloridos por significância
    ggplot2::geom_point(ggplot2::aes(color = significant), size = 2) +
    # Customização de Cores
    ggplot2::scale_color_manual(
      values = c("Sim" = "firebrick", "Não" = "gray60", "Impacto Médio" = "steelblue"),
      name = "Significância (5%)"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Bandas de confiança de 95% baseadas na estatística T do teste",
      x = "Horizonte de Previsão (h)",
      y = "Impacto (Log-diff)",
      caption = "Nota: Pontos vermelhos indicam p-valor < 0.05"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}
