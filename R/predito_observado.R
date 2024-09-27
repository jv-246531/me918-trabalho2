predito_observado <- function(dados,
                              variavel_resposta,
                              variaveis_preditoras,
                              reta = TRUE) {

  library(ggplot2)

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
