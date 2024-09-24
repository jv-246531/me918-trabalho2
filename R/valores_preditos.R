valores_preditos <- function(dados, variavel_resposta, variaveis_preditoras) {
  x_ <- matriz_delineamento(dados, variaveis_preditoras)
  betas_ <- betas(dados, variavel_resposta, variaveis_preditoras)
  return((x_ %*% betas_))
}
