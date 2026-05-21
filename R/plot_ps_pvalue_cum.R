#' @param df_cum Dataframe retornado por calc_pesaran_smith_cum.
#' @param log_scale Booleano. Se TRUE, utiliza escala log10 para o p-valor.
#' @return Um objeto ggplot2.
#' @export
plot_ps_pvalue_cum <- function(df_cum, log_scale = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("O pacote 'ggplot2' é necessário para esta função.")
  }
  
  # Criar coluna de categoria para destacar o que está abaixo de 5%
  df_cum$status <- ifelse(df_cum$p_valor <= 0.05, "Significante", "Não Significante")
  
  p <- ggplot2::ggplot(df_cum, ggplot2::aes(x = h, y = p_valor)) +
    # Área sombreada para a região de significância (0 a 0.05)
    ggplot2::annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.05, 
                      fill = "green", alpha = 0.05) +
    # Linha de corte de 5%
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", alpha = 0.6) +
    # Linha e pontos do p-valor
    ggplot2::geom_line(color = "gray30", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(color = status), size = 2.5) +
    # Escalas e Cores
    ggplot2::scale_color_manual(
      values = c("Significante" = "firebrick", "Não Significante" = "gray70"),
      name = "Critério (p < 0.05)"
    ) +
    ggplot2::labs(
      title = "Evolução da Significância Estatística",
      subtitle = "P-valor cumulativo ao longo do horizonte h",
      x = "Horizonte de Previsão (h)",
      y = "P-valor (Teste de Cauda Dupla)",
      caption = "A área verde indica a zona de rejeição da hipótese nula (significância)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  
  # Aplicar escala logarítmica se solicitado
  if (log_scale) {
    p <- p + ggplot2::scale_y_log10(breaks = c(0.001, 0.01, 0.05, 0.1, 0.5, 1))
  } else {
    p <- p + ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1))
  }
  
  return(p)
}