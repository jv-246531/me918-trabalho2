r2_ajustado <- function(dados,
                        variavel_resposta,
                        variaveis_preditoras) {
  return(
    1 - (qme(dados,
             variavel_resposta,
             variaveis_preditoras)/(var(dados[[variavel_resposta]])))
  )
}
