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
                           matriz_delineamento = matriz_delineamento(dados,
                                                                     variaveis_preditoras),
                           variavel_resposta = variavel_resposta,
                           variaveis_preditoras = variaveis_preditoras),
                      class = "nosso_lm")

  return(objeto)
}

#'@export
print.nosso_lm <- function(x, ...) {
  cat("Regressão linear múltipla realizada para\n")
  cat(paste0(x$variavel_resposta, " ~ ", paste(x$variaveis_preditoras, collapse = " + ")), "\n")
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
