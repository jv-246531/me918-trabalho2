test_that("Testes relacionados as classes dos objetos", {
  expect_type(
    betas(regressaolinear::dados,
          "y",
          c("x1", "x2")),
    "double"
  )

  expect_type(
    aic_do_modelo(regressaolinear::dados,
                  "y",
                  c("x1", "x2")),
    "double"
  )

  expect_s3_class(
    grafico_residuos(regressaolinear::dados,
                     "y",
                     c("x1", "x2")),
    "ggplot"
  )

  expect_s3_class(
    nosso_lm(regressaolinear::dados,
             "y",
             c("x1", "x2")),
    "nosso_lm"
  )

  expect_type(
    predicao(regressaolinear::dados,
             "y",
             c("x1", "x2"),
             observacao = c(13.82, 62.17)),
    "double"
  )

  expect_s3_class(
    predito_observado(regressaolinear::dados,
                      "y",
                      c("x1", "x2")),
    "ggplot"
  )

  expect_type(
    qme(regressaolinear::dados,
        "y",
        c("x1", "x2")),
    "double"
  )

  expect_s3_class(
    qq_residuos(regressaolinear::dados,
                "y",
                c("x1", "x2")),
    "ggplot"
  )

  expect_type(
    r2_ajustado(regressaolinear::dados,
                "y",
                c("x1", "x2")),
    "double"
  )

  expect_type(
    r2(regressaolinear::dados,
       "y",
       c("x1", "x2")),
    "double"
  )

  expect_type(
    residuos(regressaolinear::dados,
             "y",
             c("x1", "x2")),
    "double"
  )

  expect_type(
    semi_studentizado(regressaolinear::dados,
                      "y",
                      c("x1", "x2")),
    "double"
  )

  expect_type(
    studentizado(regressaolinear::dados,
                 "y",
                 c("x1", "x2")),
    "double"
  )

  expect_type(
    valores_preditos(regressaolinear::dados,
                     "y",
                     c("x1", "x2")),
    "double"
  )


}
)
