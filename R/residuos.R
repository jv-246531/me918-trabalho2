#'@title Resíduos obtidos
#'@description
#'Encontra os resíduos na regressão
#'linear múltipla após obter o modelo,
#'\deqn{e=Y-X\hat{\beta}}
#'Temos, para a equação acima,
#'- \eqn{e} é o resíduo obtido;
#'- \eqn{Y} é a vetor variável resposta observado;
#'- \eqn{X} é a matriz de delineamento;
#'- \eqn{\hat{\beta}} é o vetor contendo coeficientes estimados.
#'
#'@details
#'É necessário que a matriz de delineamento tenha posto completo.
#'
#'Verifique se os argumentos da função estão corretamente inseridos. É necessário que as variáveis estejam entre "aspas" (ou seja, char com nome da variável).
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição.
#'
#'@return
#'Retorna-se a relação dos resíduos na mesma ordem que estão as observações.
#'
#'@examples
#'residuos(regressaolinear::dados, "y", c("x1", "x2"))
#'
#'@export

residuos <- function(dados, variavel_resposta, variaveis_preditoras) {
  x_ <- matriz_delineamento(dados, variaveis_preditoras)
  betas_ <- betas(dados, variavel_resposta, variaveis_preditoras)
  return(as.double(dados[[variavel_resposta]] - (x_ %*% betas_)))
}
