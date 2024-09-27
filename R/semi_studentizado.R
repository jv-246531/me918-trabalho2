semi_studentizado <- function(dados,
                              variavel_resposta,
                              variaveis_preditoras) {
  return(
    residuos(dados,
                variavel_resposta,
                variaveis_preditoras)/sqrt(qme(dados,
                                                    variavel_resposta,
                                                    variaveis_preditoras))
  )
}
