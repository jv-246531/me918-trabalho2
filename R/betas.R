#'@title Coeficientes da Regressão
#'@description
#'Encontra os coeficientes na regressão
#'linear múltipla,
#'\deqn{Y = \beta_{0}+\beta_{1}X_{1}+...+\beta_{p-1}X_{p-1}+\epsilon}.
#'
#'@details
#'É necessário que a matriz de delineamento tenha posto completo.
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição.
#'
#'@return
#'Relação dos coeficientes e suas estimativas.
#'
#'@export


betas <- function(dados, variavel_resposta, variaveis_preditoras) {
  x_ <- matriz_delineamento(dados = dados,
                            variaveis = variaveis_preditoras)
  if (det(t(x_) %*% x_) == 0) {
    stop("Matriz não tem posto completo. Retire algumas das variáveis e tente novamente.")
  }
  if (!is.numeric(dados[[variavel_resposta]])) {
    stop("1) Variável resposta precisa ser numérica. Reveja sua entrada.
2) Verifique se a variável resposta está no banco de dados, e se está escrita corretamente (entre \"aspas\").")
  }
  betas_ <- solve(t(x_) %*% x_,
                  t(x_) %*% dados[[variavel_resposta]])
  return(betas_)
}
