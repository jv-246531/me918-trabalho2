predicao <- function(observacao, dados, variavel_resposta, variaveis_preditoras) {
  return(c(1,observacao) %*% betas(dados, variavel_resposta, variaveis_preditoras))
}
