r2 <- function(dados,
               variavel_resposta,
               variaveis_preditoras) {
  return(
    1 - (qme(dados,
             variavel_resposta,
             variaveis_preditoras)*(nrow(dados)-(1+length(variaveis_preditoras)))/((nrow(dados)-1)*var(dados[[variavel_resposta]])))
  )
}
