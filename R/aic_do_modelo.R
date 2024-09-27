#ATENÇÃO ARRUMAR PROBLEMA PARA VARIAVEIS CATEGORICAS

aic_do_modelo <- function(dados,
                          variavel_resposta,
                          variaveis_preditoras,
                          penalizacao = 2,
                          bayesiano = FALSE) {
  n_ <- nrow(dados)

  if (bayesiano) {
    penalizacao <- log(n_)
  }

  return(
    n_*log( (n_ -(1+length(variaveis_preditoras)))*qme(dados,
                                                       variavel_resposta,
                                                       variaveis_preditoras) - n_*log(n_) + penalizacao*(1+length(variaveis_preditoras)))
  )
}
