#'@title Coeficiente de determinação ajustado (\eqn{R_{aj}^2})
#'@description
#'Calcula o coeficiente de determinação ajustado do modelo com base na variável resposta e nas variáveis preditoras.
#'\deqn{R_{aj}^2 = 1 - \dfrac{n - 1}{n - p}\dfrac{SQ_{res}}{SQ_{tot}}}
#'Temos, para a equação acima,
#'- \eqn{n} é quantidade de observações no conjunto de dados;
#'- \eqn{p} é a quantidade de variáveis explicativas (preditoras) usadas no modelo, mais o intercepto;
#'- \eqn{SQE} é soma quadrática dos resíduos;
#'- \eqn{SQT} é soma quadrática total, relativa à variável resposta.
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
#'Retorna-se o coeficiente de determinação ajustado (\eqn{R_{aj}^2}) com base na variável resposta e variáveis preditoras do modelo.
#'
#'@examples
#'r2_ajustado(regressaolinear::dados, "y", c("x1", "x2"))
#'
#'@export

r2_ajustado <- function(dados,
                        variavel_resposta,
                        variaveis_preditoras) {
  return(
    1 - (qme(dados,
             variavel_resposta,
             variaveis_preditoras)/(var(dados[[variavel_resposta]])))
  )
}
