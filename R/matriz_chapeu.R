matriz_chapeu <- function(dados,
                          variaveis_preditoras) {

  x_ <- matriz_delineamento(dados = dados,
                            variaveis = variaveis_preditoras)

  if (det(t(x_) %*% x_) == 0) {
    stop("Matriz não tem posto completo. Retire algumas das variáveis e tente novamente.")
  }

  return(
    x_ %*% solve(t(x_) %*% x_) %*% t(x_)
  )
}
