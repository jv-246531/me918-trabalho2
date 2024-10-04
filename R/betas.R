#'@title Coeficientes da Regressão
#'@description
#'Encontra os coeficientes na regressão
#'linear múltipla,
#'\deqn{Y = \beta_{0}+\beta_{1}X_{1}+...+\beta_{p-1}X_{p-1}+\epsilon.}
#'Temos, para a equação acima,
#'- \eqn{Y} é a variável resposta;
#'- \eqn{X_{1},...,X_{p-1}} são as variáveis preditoras;
#'- \eqn{\beta_{0},\beta_{1},...,\beta_{n}} são os coeficientes da regressão; \eqn{\beta_{0}} é o intercepto;
#'- \eqn{\epsilon\sim\mathcal{N}(0,\sigma^{2})} é o erro aleatório independente e identicamente distribuído para toda observação.
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
#'Retorna-se a relação dos coeficientes e suas estimativas.
#'
#'@examples
#'betas(proj2::dados, "y", c("x1", "x2"))
#'
#'@export


betas <- function(dados, variavel_resposta, variaveis_preditoras) {

  if (any(!complete.cases(dados[,c(variavel_resposta, variaveis_preditoras)]))) {
    stop("Variáveis escolhidas do conjunto de dados contêm entradas NA. Para usar a função, é necessário removê-las.")
  }

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

  colnames(betas_) <- "Estimativa"
  return(betas_)
}
