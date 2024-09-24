grafico <- function(dados, variavel_resposta, variaveis_preditoras) {
  preditos <- valores_preditos(dados, variavel_resposta, variaveis_preditoras)
  valores <- data.frame(observados = dados[[variavel_resposta]],
                        preditos = preditos)
  ggplot(valores) +
    geom_point(aes(x = preditos,
                   y = observados)) +
    labs(title = "Valores preditos x Valores observados",
         x = "Valores preditos",
         y = "Valores observados") +
    theme_classic()
}
