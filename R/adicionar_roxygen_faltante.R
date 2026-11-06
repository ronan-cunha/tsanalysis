#' Título da função adicionar_roxygen_faltante
#'
#' @description Descrição da função adicionar_roxygen_faltante.
#'
#' @export

adicionar_roxygen_faltante <- function(dir_path = "R") {
  # Lista todos os arquivos .R ou .r no diretório informado
  arquivos <- list.files(dir_path, pattern = "\\.[Rr]$", full.names = TRUE)

  if (length(arquivos) == 0) {
    stop("Nenhum arquivo .R encontrado no diretório: ", dir_path)
  }

  for (arq in arquivos) {
    # Lê as linhas do arquivo mantendo a codificação UTF-8
    linhas <- readLines(arq, encoding = "UTF-8")

    # Verifica se o arquivo já possui um cabeçalho roxygen nas primeiras linhas
    tem_roxygen <- any(grepl("^\\s*#+'", head(linhas, 10)))

    if (!tem_roxygen) {
      # Tenta identificar o nome da primeira função definida no arquivo
      # Procura por padrões como: nome_funcao <- function( ou nome_funcao = function(
    match_func <- grep("^[a-zA-Z0-9_.]+\\s*(<-|=)\\s*function", linhas)

    if (length(match_func) > 0) {
      # Extrai o nome da função da linha encontrada
      primeira_def <- linhas[match_func[1]]
      nome_func <- sub("\\s*(<-|=)\\s*function.*", "", primeira_def)
      nome_func <- trimws(nome_func)

      # Monta um cabeçalho roxygen básico obrigatório (evita o erro de falta de nome/título)
      cabecalho <- c(
        paste0("#' Título da função ", nome_func),
        "#'",
        paste0("#' @description Descrição da função ", nome_func, "."),
        "#'",
        "#' @export",
        ""
      )

      # Insere o cabeçalho no topo do arquivo
      novas_linhas <- c(cabecalho, linhas)

      # Sobrescreve o arquivo original com o cabeçalho adicionado
      writeLines(novas_linhas, arq)
      message(sprintf("[ATUALIZADO] Cabeçalho adicionado em: %s (Função: %s)", basename(arq), nome_func))

    } else {
      message(sprintf("[IGNORADO] Nenhuma atribuição de função encontrada em: %s", basename(arq)))
    }

    } else {
      message(sprintf("[OK] O arquivo %s já possui cabeçalho roxygen.", basename(arq)))
    }
  }
  message("Agora rode: roxygen2::roxygenise()")
}
