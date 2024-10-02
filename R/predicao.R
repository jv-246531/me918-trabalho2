#'@title Predição através do modelo de regressão
#'@description
#'Dada uma nova observação, usa o modelo de regressão
#'estabelecido para predizer o valor da variável resposta,
#'\deqn{\hat{Y}_{novo}=X_{novo}\hat{\beta}}
#'Temos, para a equação acima,
#'- \eqn{\hat{Y}_{novo}} é o valor predito para os novos dados com base no modelo;
#'- \eqn{X_{novo}} é o novo dado inserido;
#'- \eqn{\hat{\beta}} é o vetor contendo coeficientes estimados.
#'
#'@details
#'Verifique se os novos dados estão na ordem em que foram colocados os betas da predição, no argumento variavel_preditoras.
#'
#'É necessário que a matriz de delineamento tenha posto completo.
#'
#'Verifique se os argumentos da função estão corretamente inseridos. É necessário que as variáveis estejam entre "aspas" (ou seja, char com nome da variável).
#'
#'@param observacao Novos valores para variáveis preditoras, dos quais queremos obter predição;
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição.
#'
#'@return
#'Retorna-se a relação dos valores preditos na mesma ordem que estão as observações.
#'
#'#'@examples
#'predicao(proj2::dados, "y", c("x1", "x2"))
#'
#'@export

predicao <- function(observacao, dados, variavel_resposta, variaveis_preditoras) {
  return(c(1,observacao) %*% betas(dados, variavel_resposta, variaveis_preditoras))
}
