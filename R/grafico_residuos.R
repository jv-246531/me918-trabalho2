#'@title Gráfico: Valores Preditos v.s. Resíduos
#'@description
#'Com ajuda do pacote _ggplot2_, cria um valores preditos para a variável resposta v.s. resíduos, podendo ou não ser studentizados.
#'
#'@details
#'Verifique se os argumentos da função estão corretamente inseridos. É necessário que as variáveis estejam entre "aspas" (ou seja, char com nome da variável).
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição;
#'@param studentizado Parâmetro para indicar se os resíduos são calculados sem normalização (argumento "nao"),
#'se serão semi-studentizados (argumento "semi") ou studentizados (argumento "sim").
#'@return
#'Retorna-se um gráfico de dispersão das variáveis preditas v.s. observadas.
#'
#'@examples
#'grafico_residuos(regressaolinear::dados, "y", c("x1", "x2"), studentizado = "sim")
#'
#'@export


grafico_residuos <- function(dados,
                             variavel_resposta,
                             variaveis_preditoras,
                             studentizado = "nao") {

  suppressWarnings( library(ggplot2) )

  preditos <- valores_preditos(dados, variavel_resposta, variaveis_preditoras)
  if(studentizado == "nao") {
    residuos_ <- residuos(dados, variavel_resposta, variaveis_preditoras)
  }
  else{
    if(studentizado == "semi") {
      residuos_ <- semi_studentizado(dados, variavel_resposta, variaveis_preditoras)
    }
    else{
      if(studentizado == "sim") {
        residuos_ <- studentizado(dados, variavel_resposta, variaveis_preditoras)
      }
      else{
        stop("Argumento \'studentizado\' com entrada inválida. Insira um argumento possível (\'nao\',\'semi\' ou \'sim\')")
      }
    }
  }

  valores <- data.frame(residuos_ = residuos_,
                        preditos = preditos)
  grafico_ <- ggplot(valores) +
    geom_point(aes(x = preditos,
                   y = residuos_))  +
    geom_abline(slope = 0, intercept = 0) +
    theme_classic()

  if(studentizado == "nao") {
    grafico_ <- grafico_ +
      labs(title = "Valores preditos x Resíduos",
           x = "Valores preditos",
           y = "Resíduos")
  }
  else{
    if(studentizado == "semi") {
      grafico_ <- grafico_ +
        labs(title = "Valores preditos x Resíduos",
             x = "Valores preditos",
             y = "Resíduos semi-studentizados")
    }
    else{
      grafico_ <- grafico_ +
        labs(title = "Valores preditos x Resíduos",
             x = "Valores preditos",
             y = "Resíduos studentizados")
    }
  }

  return(grafico_)
}
