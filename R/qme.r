#'@title Quadrado Médio do Erro
#'@description
#'Encontra o Quadrado Médio do Erro (QME)
#'\deqn{QME = \dfrac{1}{n-p}\sum_{i=1}^{n}{(\hat{y}_i - y_i)^2}}
#'Temos, para a equação acima,
#'- \eqn{\hat{y}_i} é a estimativa da variável resposta na observação i;
#'- \eqn{y_i} é o valor da observação i da variável resposta;
#'- \eqn{n} é o número de observações do conjunto de dados;
#'- \eqn{p} é a quantidade de preditoras utilizadas na regressão, incluindo intercepto.
#'
#'@details
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
#'Retorna-se o valor do Quadrado Médio do Erro.
#'
#'@examples
#'qme(proj2::dados, "y", c("x1", "x2"))
#'
#'@export

#ATENÇÃO ARRUMAR PROBLEMA PARA VARIAVEIS CATEGORICAS

qme <- function(dados,
                variavel_resposta,
                variaveis_preditoras) {

  p <- ncol(matriz_delineamento(dados,
                                variaveis_preditoras))

  return(
    sum(
      (residuos(dados,
               variavel_resposta,
               variaveis_preditoras))^2
    )/(nrow(dados)-(p))
  )
}
