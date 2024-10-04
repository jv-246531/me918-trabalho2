test_that("Testes relacionados as classes dos objetos", {
  expect_type(
    betas(proj2::dados,
          "y",
          c("x1", "x2")),
    "double"
  )

  expect_type(
    aic_do_modelo(proj2::dados,
                  "y",
                  c("x1", "x2")),
    "double"
  )

  expect_s3_class(
    grafico_residuos(proj2::dados,
                     "y",
                     c("x1", "x2")),
    "ggplot"
  )

  expect_s3_class(
    nosso_lm(proj2::dados,
             "y",
             c("x1", "x2")),
    "nosso_lm"
  )

  expect_type(
    predicao(proj2::dados,
             "y",
             c("x1", "x2"),
             observacao = c(13.82, 62.17)),
    "double"
  )

  expect_s3_class(
    predito_observado(proj2::dados,
                      "y",
                      c("x1", "x2")),
    "ggplot"
  )

  expect_type(
    qme(proj2::dados,
        "y",
        c("x1", "x2")),
    "double"
  )

  expect_s3_class(
    qq_residuos(proj2::dados,
                "y",
                c("x1", "x2")),
    "ggplot"
  )

  expect_type(
    r2_ajustado(proj2::dados,
                "y",
                c("x1", "x2")),
    "double"
  )

  expect_type(
    r2(proj2::dados,
       "y",
       c("x1", "x2")),
    "double"
  )

  expect_type(
    residuos(proj2::dados,
             "y",
             c("x1", "x2")),
    "double"
  )

  expect_type(
    semi_studentizado(proj2::dados,
                      "y",
                      c("x1", "x2")),
    "double"
  )

  expect_type(
    studentizado(proj2::dados,
                 "y",
                 c("x1", "x2")),
    "double"
  )

  expect_type(
    valores_preditos(proj2::dados,
                     "y",
                     c("x1", "x2")),
    "double"
  )


}
)
