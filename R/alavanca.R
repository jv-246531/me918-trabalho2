alavanca <- function(dados,
                     variaveis_preditoras) {
  return(
    diag(
      matriz_chapeu(dados,
                    variaveis_preditoras)
    )
  )
}
