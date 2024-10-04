#'@title Criterio de Informação de Akaike (AIC)
#'@description
#'Calcula o Critério de Informação de Akaike (AIC) para um modelo estatístico com base na variável resposta e nas variáveis preditoras.
#'\deqn{AIC = -2*\text{log }L(\hat{\beta}|\textbf{X}) + k\cdot p}
#'onde:
#'- \eqn{p} é a quantidade de variáveis explicativas (preditoras) usadas no modelo;
#'- \eqn{k} é a penalização no cálculo do Critério;
#'- \eqn{\hat{L}} é o valor máximo da função de verossimilhança, obtido pelo EMV \eqn{\hat{\beta}}.
#'@details
#'A função oferece a opção de usar o Critério de Informação Bayesiano (BIC), ajustando o valor de penalização com base no logaritmo do número de observações.
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis que serão usadas na predição;
#'@param penalizacao Valor de penalização a ser aplicado no AIC (padrão é 2);
#'@param bayesiano Lógico que indica se deve ser usada a penalização do BIC (padrão é FALSE).
#'@return
#'Retorna o valor do Critério de Informação de Akaike (AIC) com base na variável resposta e nas variáveis preditoras do modelo.
#'
#'@examples
#'aic_do_modelo(proj2::dados, "y", c("x1", "x2"))
#'aic_do_modelo(proj2::dados, "y", c("x1", "x2"), bayesiano = TRUE)
#'
#'@export


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

  p <- ncol(matriz_delineamento(dados,
                                variaveis_preditoras))

  return(
    n_*log( (n_ -(p))*qme(dados,
                          variavel_resposta,
                          variaveis_preditoras) - n_*log(n_) + penalizacao*(p))
  )
}
