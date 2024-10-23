#'@title Gráfico: Valores Preditos v.s. Valores Observados
#'@description
#'Com ajuda do pacote _ggplot2_, cria um gráfico de dispersão da variável resposta predita v.s. observada com a reta identidade para comparar os valores.
#'
#'@details
#'Verifique se os argumentos da função estão corretamente inseridos. É necessário que as variáveis estejam entre "aspas" (ou seja, char com nome da variável).
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição;
#'@param reta Lógico que adiciona a reta identidade y = x (padrão é TRUE).
#'@return
#'Retorna-se um gráfico de dispersão das variáveis respostas preditas no modelo v.s. observadas.
#'
#'@examples
#'predito_observado(regressaolinear::dados, "y", c("x1", "x2"))
#'
#'@export


predito_observado <- function(dados,
                              variavel_resposta,
                              variaveis_preditoras,
                              reta = TRUE) {

  suppressWarnings( library(ggplot2) )

  preditos <- valores_preditos(dados, variavel_resposta, variaveis_preditoras)
  valores <- data.frame(observados = dados[[variavel_resposta]],
                        preditos = preditos)
  grafico_ <- ggplot(valores) +
    geom_point(aes(x = preditos,
                   y = observados)) +
    labs(title = "Valores preditos x Valores observados",
         x = "Valores preditos",
         y = "Valores observados") +
    theme_classic()

  if (reta) {
    grafico_ <- grafico_ +
    geom_abline(slope = 1, intercept = 0)
  }

  return(grafico_)
}
