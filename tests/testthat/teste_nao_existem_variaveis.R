test_that("Testes relacionados com falta de variáveis nos argumentos da função", {
  expect_error(
    betas(regressaolinear::dados,
          "y",
          "x")) # não existe coluna com nome x no conjunto de dados

  expect_error(
    residuos(regressaolinear::dados,
             "y",
             c("x1", "x3"))) # nao existe coluna com nome x3 no conjunto de dados

  expect_error(
    valores_preditos(regressaolinear::dados,
                     "y",
                     c("x1, x2"))) # escrita errada do vetor das variaveis preditoras

  expect_error(
    nosso_lm(regressaolinear::dados,
             "y",
             c("x1", "x2", "x3"))) # colocou-se variaveis que nao existem

  expect_error(
    studentizado(regressaolinear::dados,
                 "rato",
                 c("x1", "x2"))) # nao existe coluna com nome rato no conjunto de dados

  expect_error(
    semi_studentizado(regressaolinear::dados_,
                      "y",
                      c("x1", "x2"))) # nao existe conjunto dados_ no pacote

  expect_error(
    aic_do_modelo(dados = regressaolinear::dados,
                  variaveis_preditoras = c("x1", "x2"))) # nao especificou variavel resposta

  expect_error(
    r2(dados = regressaolinear::dados,
       variavel_resposta = "y")) # nao especificou variaveis preditoras

  expect_error(
    r2_ajustado(variavel_resposta = "y",
                variaveis_preditoras = c("x1", "x3"))) # nao especificou conjunto de dados

  expect_error(
    betas(data.frame(y = c(1,2,3),
                     x = c(3,NA,7)),
          "y",
          "x")) # contém NA na variável preditora escolhida

  expect_error(
    betas(data.frame(y = c(1,NA,3),
                     x = c(3,5,7)),
          "y",
          "x")) # contém NA na variável resposta escolhida

  expect_error(
    nosso_lm(regressaolinear::dados,
             y,
             c("x1", "x2"))) # variavel resposta nao está entre aspas

  expect_error(
    nosso_lm(regressaolinear::dados,
             "y",
             c(x1, x2))) # variaveis preditoras nao estão entre aspas
}
)
