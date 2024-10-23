#'@title Regressão Linear Múltipla
#'@description
#'Retorna uma lista com diversas informações relacionadas a um modelo de
#'regressão linear múltipla, construído com base em um banco de dados
#'e suas variáveis. O modelo é o que segue.
#'
#'\deqn{Y=\beta_{0}+\beta_{1}X_{1}+\cdots+\beta_{p-1}X_{p-1}+\epsilon}
#'
#'Temos, para a equação acima,
#'- \eqn{Y} é a variável resposta;
#'- \eqn{X_{1},...,X_{p-1}} são as variáveis preditoras;
#'- \eqn{\beta_{0},\beta_{1},...,\beta_{n}} são os coeficientes da regressão; \eqn{\beta_{0}} é o intercepto;
#'- \eqn{\epsilon\sim\mathcal{N}(0,\sigma^{2})} é o erro aleatório independente e identicamente distribuído para toda observação.
#'
#'Além disso, permite utilização de uma série de métodos:
#'- print;
#'- plot;
#'- confint;
#'- summary;
#'- predict.
#'
#'@details
#'É necessário que a matriz de delineamento tenha posto completo.
#'
#'Verifique se os argumentos da função estão corretamente inseridos. É
#'necessário que as variáveis estejam entre "aspas" (ou seja, char com
#'nome da variável).
#'
#'Para uso do módulo _predict_, é preciso especificar, em valores numéricos,
#'os valores da variáveis preditoras na ordem em que aparecem na equação de regressão.
#'Deste modo, por exemplo, deve ter a inserção correta de 0s e 1s nas variáveis dummy.
#'
#'@param dados Banco de dados para análise;
#'@param variavel_resposta Variável resposta, da qual queremos
#'fazer a predição;
#'@param variaveis_preditoras Vetor composto pelas variáveis
#'que serão usadas na predição.
#'
#'@return
#'Retorna-se um objeto do tipo "nosso_lm", contendo uma lista de informações
#'que podem ser obtidas ao estabeler o modelo. Nesta lista, encontra-se:
#'- .$betas: Relação com estimativas para os coeficientes da regressão;
#'- .$qme: Quadrado médio do erro, estimador não viesado para variância do erro normal;
#'- .$r2_ajustado: Coeficiente de determinação ajustado pela quantidade de preditoras;
#'- .$r2: Coeficiente de determinação sem ajuste;
#'- .$residuos: Resíduos obtidos pela diferença entre o que foi observado e o que foi predito no modelo;
#'- .$semi_studentizado: Resíduos semi_studentizados;
#'- .$studentizado: Resíduos studentizados;
#'- .$valores_preditos: Predição dos valores da variável resposta em relação ao modelo construído;
#'- .$aic_do_modelo: Critério de informação de Akaike para o modelo, com \eqn{k=2};
#'- .$matriz_delineamento: Matriz com informações sobre as variáveis preditoras;
#'- .$variavel_resposta: Lista contendo nome da variável resposta e seus valores.
#'
#'@examples
#'regressao <- nosso_lm(regressaolinear::dados, "y", c("x1", "x2"))
#'plot(regressao)
#'confint(regressao)
#'summary(regressao)
#'predict(regressao, observacao = c(13.82, 62.17))
#'
#'regressao <- nosso_lm(regressaolinear::categoricas, "y", c("fator1", "fator2", "numerica"))
#'plot(regressao)
#'confint(regressao)
#'summary(regressao)
#'predict(regressao, observacao = c("b", "c", 5)) # vai dar errado
#'predict(regressao, observacao = c(1,0,0,0,1,0,5)) # vai dar certo: segue a relação dos betas
#'
#'@export

nosso_lm <- function(dados,
                     variavel_resposta,
                     variaveis_preditoras) {

  objeto <- structure(list(betas = betas(dados,
                                         variavel_resposta,
                                         variaveis_preditoras),
                           qme = qme(dados,
                                     variavel_resposta,
                                     variaveis_preditoras),
                           r2_ajustado = r2_ajustado(dados,
                                                     variavel_resposta,
                                                     variaveis_preditoras),
                           r2 = r2(dados,
                                   variavel_resposta,
                                   variaveis_preditoras),
                           residuos = residuos(dados,
                                               variavel_resposta,
                                               variaveis_preditoras),
                           semi_studentizado = semi_studentizado(dados,
                                                                 variavel_resposta,
                                                                 variaveis_preditoras),
                           studentizado = studentizado(dados,
                                                       variavel_resposta,
                                                       variaveis_preditoras),
                           valores_preditos = valores_preditos(dados,
                                                               variavel_resposta,
                                                               variaveis_preditoras),
                           aic_do_modelo = aic_do_modelo(dados,
                                                         variavel_resposta,
                                                         variaveis_preditoras),
                           matriz_delineamento = matriz_delineamento(dados,
                                                                     variaveis_preditoras),
                           variavel_resposta = list(nome = variavel_resposta,
                                                    valores = dados[[variavel_resposta]])),
                      class = "nosso_lm")

  return(objeto)
}

#'@export
print.nosso_lm <- function(x, ...) {
  cat("Regressão linear múltipla realizada para\n")
  cat(paste0(x$variavel_resposta[["nome"]], " ~ ", paste(rownames(x$betas)[-1], collapse = " + ")), "\n")
}

#'@export
plot.nosso_lm <- function(x, ...) {

  library(ggplot2)

  valores <- data.frame(preditos = x$valores_preditos,
                        observados = x$valores_preditos + x$residuos,
                        residuos = x$studentizado)

  grafico_ <- list(

    ggplot(valores) +
    geom_point(aes(x = preditos,
                   y = observados)) +
    geom_abline(slope = 1, intercept = 0) +
    labs(title = "Valores preditos x Valores observados",
         x = "Valor predito",
         y = "Valor observado") +
    theme_classic(),

    ggplot(valores) +
      geom_point(aes(x = preditos,
                     y = residuos)) +
      geom_abline(slope = 0, intercept = 0) +
      labs(title = "Valores preditos x Resíduos",
           x = "Valor predito",
           y = "Resíduo studentizado") +
      theme_classic(),

    ggplot(valores) +
      stat_qq(aes(sample = residuos)) +
      stat_qq_line(aes(sample = residuos)) +
      labs(title = "Comparação de quantis com a distribuição normal padrão\n(QQ-Norm)",
           x = "Quantis teóricos",
           y = "Resíduos studentizados") +
      theme_classic()
  )
  for(plot in grafico_) {
    readline(prompt = "Aperte \"Enter\" para ver o próximo gráfico:")
    print(plot)
  }
}

#'@export
confint.nosso_lm <- function(x, level = .95, ...) {
  return(
    data.frame(Estimativa = x$betas,
               limite_inferior = as.double(x$betas - qt(1-((1-level)/2), df = nrow(x$matriz_delineamento)-ncol(x$matriz_delineamento))*sqrt(x$qme * diag(solve(t(x$matriz_delineamento) %*% x$matriz_delineamento)))),
               limite_superior = as.double(x$betas + qt(1-((1-level)/2), df = nrow(x$matriz_delineamento)-ncol(x$matriz_delineamento))*sqrt(x$qme * diag(solve(t(x$matriz_delineamento) %*% x$matriz_delineamento))))
               )
  )
}

#'@export
summary.nosso_lm <- function(x, ...) {
  return(
    data.frame(
      Estimado = x$betas,
      erro_padrão = as.double(sqrt(x$qme * diag(solve(t(x$matriz_delineamento) %*% x$matriz_delineamento)))),
      estatistica_t = as.double(x$betas/sqrt(x$qme * diag(solve(t(x$matriz_delineamento) %*% x$matriz_delineamento)))),
      p_valor = 2*pt(as.double(abs(x$betas/sqrt(x$qme * diag(solve(t(x$matriz_delineamento) %*% x$matriz_delineamento))))),
                     lower.tail = FALSE, df = nrow(x$matriz_delineamento)-ncol(x$matriz_delineamento))
      )
    )
}

#'@export
predict.nosso_lm <- function(x, observacao, ...) {
    return(as.double(c(1,observacao) %*% x$betas))
}
