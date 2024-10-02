#'@title Resíduos Studentizados
#'@description
#'Calcula os resíduos studentizados de um modelo de regressão com base na variável resposta e nas variáveis preditoras.
#'A fórmula utilizada é:
#'\deqn{r_i^* = \dfrac{e_i}{\sqrt{QME \cdot (1 - h_i)}}}
#'onde:
#'- \eqn{e_i} é o resíduo da observação \eqn{i};
#'- \eqn{QME} é o quadrado médio do erro do modelo;
#'- \eqn{h_i} é a alavancagem da observação \eqn{i}.
#'@details
#'Certifique-se de que as variáveis estão corretamente especificadas e que os dados não contêm valores ausentes.
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos analisar os resíduos;
#'@param variaveis_preditoras Vetor composto pelas variáveis que foram utilizadas na predição.
#'@return
#'Retorna um vetor contendo os resíduos studentizados.
#'
#'@examples
#'studentizado(proj2::dados, "y", c("x1", "x2"))
#'
#'@export

studentizado <- function(dados,
                         variavel_resposta,
                         variaveis_preditoras) {
  return(
    residuos(dados,
             variavel_resposta,
             variaveis_preditoras)/sqrt(qme(dados,
                                            variavel_resposta,
                                            variaveis_preditoras)*(1-alavanca(dados,
                                                                              variaveis_preditoras)))
  )
}
