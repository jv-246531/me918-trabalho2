#ATENÇÃO ARRUMAR PROBLEMA PARA VARIAVEIS CATEGORICAS

qme <- function(dados,
                variavel_resposta,
                variaveis_preditoras) {
  return(
    sum(
      (residuos(dados,
               variavel_resposta,
               variaveis_preditoras))^2
    )/(nrow(dados)-(1+length(variaveis_preditoras)))
  )
}
