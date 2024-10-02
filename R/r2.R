#'@title Coeficiente de determinação (\eqn{R^2})
#'@description
#'Calcula o coeficiente de determinação do modelo com base na variável resposta e nas variáveis preditoras.
#'\deqn{R^2 = 1 - \dfrac{SQ_{res}}{SQ_{tot}}}
#'Temos, para a equação acima,
#'- \eqn{SQ_{res}} é a soma dos quadrados dos resíduos do modelo;
#'- \eqn{SQ_{tot}} é a soma dos quadrados total do modelo, relativa à variável resposta.
#'@details
#'Verifique se os argumentos da função estão corretamente inseridos. É necessário que as variáveis estejam entre "aspas" (ou seja, char com nome da variável).
#'
#'
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição.
#'@return
#'Retorna-se o coeficiente de determinação (\eqn{R^2}) com base na variável resposta e variáveis preditoras do modelo.
#'
#'@examples
#'r2(proj2::dados, "y", c("x1", "x2"))
#'
#'@export

r2 <- function(dados,
               variavel_resposta,
               variaveis_preditoras) {


  return(
    1 - (sum(residuos(dados,
                      variavel_resposta,
                      variaveis_preditoras)^2)/((nrow(dados)-1)*var(dados[[variavel_resposta]])))
  )
}
