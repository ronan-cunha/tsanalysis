#' Preparação de dados para o teste de Pesaran & Smith
#'
#' @description Realiza validações e cálculos base comuns aos testes pontual e cumulativo.
#' @noRd
#' @keywords internal
ps_setup <- function(model, target_var, y_hat, y) {
  if (!inherits(model, "varest")) stop("O modelo deve ser da classe 'varest'.")
  if (length(y_hat) != length(y)) stop("y_hat e y devem ter o mesmo comprimento.")

  variaveis_modelo <- colnames(model[['y']])
  posicao <- which(variaveis_modelo == target_var)

  if (length(posicao) == 0) stop("Variável não encontrada no modelo.")

  s <- rep(0, length(variaveis_modelo))
  s[posicao] <- 1

  H <- length(y)
  resid <- residuals(model)
  sigma_eps <- (t(resid) %*% resid) / nrow(resid)

  d_total <- log(y_hat) - log(y)
  phis <- vars::Phi(model, nstep = H)

  return(list(
    H = H,
    s = s,
    sigma_eps = sigma_eps,
    d_total = d_total,
    phis = phis,
    n_vars = ncol(sigma_eps)
  ))
}

