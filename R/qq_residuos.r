qq_residuos <- function(dados,
                        variavel_resposta,
                        variaveis_preditoras,
                        reta = TRUE) {

  library(ggplot2)

  residuos_ <- data.frame(residuo = studentizado(dados, variavel_resposta, variaveis_preditoras))

  grafico_ <- ggplot(residuos_) +
    stat_qq(aes(sample = residuo)) +
    labs(title = "Comparação de quantis com a distribuição normal padrão\n(QQ-Norm)",
         x = "Quantis teóricos",
         y = "Resíduos studentizados") +
    theme_classic()

  if (reta) {
    grafico_ <- grafico_ +
      stat_qq_line(aes(sample = residuo))
  }

  return(grafico_)
}
