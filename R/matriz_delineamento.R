matriz_delineamento <- function(dados, variaveis) {

  if (length(setdiff(variaveis,
                     colnames(dados)
                     )
             ) > 0) {
    stop(paste0("As seguintes variáveis não estão presentes no banco de dados:\n",
               paste(setdiff(variaveis, colnames(dados)),
                     collapse = "\n"),
               "\nVerifique se estão no banco de dados e escritas corretamente."))
  }

  formula_ <- as.formula(paste0("~",
                                paste(variaveis, collapse = "+")))

  matriz_ <- model.matrix(formula_,
                          data = dados)

  return(matriz_)
}
