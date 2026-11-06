#' Título da função dfgls_test
#'
#' @description Descrição da função dfgls_test.
#'
#' @export

dfgls_test <- function(y, max_lags = 4, crit = 'AIC') {

  # DF-GLS possui especificações de constante ('mu') e tendência ('tau')
  especificacoes <- c("constant", "trend")
  #type_test = c("DF-GLS", "P-test")
  
  resultados <- list()
  
  for (espec in especificacoes) {

      # Executa o teste DF-GLS via urca::ur.ers
      teste <- urca::ur.ers(
        y = y, 
        type = 'DF-GLS', 
        model = espec, 
        lag.max = max_lags
        )
      
      # Extrai a estatística de teste e os valores críticos
      est_stat <- as.numeric(teste@teststat)
      cv_1pct  <- teste@cval[1, "1pct"]
      cv_5pct  <- teste@cval[1, "5pct"]
      cv_10pct <- teste@cval[1, "10pct"]
      
      # Número de lags efetivamente utilizados na regressão auxilar
      lags_usados <- sum(grepl("diff", names(teste@testreg[["aliased"]])))
      
      # Extrai valor do critério de informação
      crit_val <- extract_crit_info_ur(teste)[[crit]]
      
      # Monta a linha do resultado
      resultados[[length(resultados) + 1]] <- data.frame(
        especif            = espec,
        info_criterion     = crit,
        info_crit_val      = round(crit_val, 4),
        selected_lags      = lags_usados,
        test_stat          = round(est_stat, 6),
        cv_1pct            = round(cv_1pct, 6),
        cv_5pct            = round(cv_5pct, 6),
        cv_10pct           = round(cv_10pct, 6),
        stringsAsFactors   = FALSE
      )
  }
  
  # Agrupa os resultados e filtra pelo critério selecionado
  tabela_final <- do.call(rbind, resultados)
  tabela_final <- tabela_final[tabela_final['info_criterion'] == crit, ]
  rownames(tabela_final) <- NULL
  return(tabela_final)
}
