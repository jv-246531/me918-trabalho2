test_that("Testes relacionados a propriedades matriciais e modelos no geral", {
  expect_error(
    betas(data.frame(y = c(1,2,3),
                     x = c(1,1,1)),
          "y",
          "x")) # coluna constante: confunde com intercepto

  expect_error(
    valores_preditos(data.frame(y = c(1,2,3),
                                x1 = c(1,3,5),
                                x2 = c(2,6,10)),
                     "y",
                     c("x1", "x2"))) # uma coluna é o dobro da outra: posto não completo da matriz de delineamento

  expect_error(
    nosso_lm(data.frame(y = c(1,2,3),
                        x1 = c(1,0,0),
                        x2 = c(0,1,0),
                        x3 = c(0,0,1)),
             "y",
             c("x1", "x2", "x3"))) # mais variaveis preditoras do que observacoes: posto não completo da matriz de delineamento

  expect_equal(
    r2(data.frame(y = c(1,2,3),
                  x = c(3,5,7)),
       "y",
       "x"), 1) # resposta é combinação linear + translanção da preditora: espera-se descrição perfeita

  expect_equal(
    qme(data.frame(y = c(1,2,3),
                  x1 = c(1,1,2),
                  x2 = c(0,1,1)),
       "y",
       c("x1", "x2")), 0) # resposta é combinação linear perfeita das preditoras: espera-se descrição perfeita


}
)
