#'@title QQ-Plot dos resíduos
#'@description
#'Com ajuda do pacote _ggplot2_, cria um gráfico QQ-Plot dos resíduos do modelo de regressão.
#'
#'@details
#'Verifique se os argumentos da função estão corretamente inseridos. É necessário que as variáveis estejam entre "aspas" (ou seja, char com nome da variável).
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição;
#'@param reta Lógico que adiciona a reta do esperado sob distribuição normal (padrão é TRUE).
#'@return
#'Retorna-se um gráfico QQ-plot dos resíduos do modelo de regressão.
#'
#'@examples
#'qq_residuos(regressaolinear::dados, "y", c("x1", "x2"))
#'
#'@export

qq_residuos <- function(dados,
                        variavel_resposta,
                        variaveis_preditoras,
                        reta = TRUE) {

  suppressWarnings( library(ggplot2) )

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
