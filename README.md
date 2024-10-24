
# regressaolinear

## 

<!-- badges: start -->
<!-- badges: end -->

O objetivo do pacote *regressaolinear* é ajustar um modelo de regressão
linear, realizar predições e análises residuais, com estimativas dos
coeficientes (betas); efetuar comparações entre os valores preditos e
observados e inserir ferramentes que permitam a análise detalhada dos
resíduos do modelo ajustado, incluindo, por exemplo, resíduos
studentizados e gráficos que permitem verificar normaldiade dos
resíduos.

Funções disponíveis (utilize `help(`nome da função`)` para verificar
como funciona e o que obtém-se como resutlado):

- **aic_do_modelo**: Calcula o AIC para avaliar a qualidade do modelo.
- **betas**: Estima os coeficientes do modelo de regressão.
- **grafico_residuos**: Gera gráfico de resíduos versus valores
  preditos.
- **nosso_lm**: Ajusta um modelo de regressão linear através de uma
  função diferente do `lm` original.
- **predicao**: Realiza predições com base no modelo ajustado.
- **predito_observado**: Gera gráfico que compara valores preditos e
  observados.
- **qme**: Calcula o quadrado médio do erro (QME).
- **qq_residuos**: Gera gráfico QQ para verificar normalidade dos
  resíduos.
- **r2**: Calcula o coeficiente de determinação $R^{2}$.
- **r2_ajustado**: Calcula o coeficiente de determinação ajustado,
  $R^{2}_{adj}$.
- **residuos**: Calcula os resíduos do modelo.
- **semi_studentizado**: Calcula resíduos semi-studentizados.
- **studentizado**: Calcula resíduos studentizados.
- **valores_preditos**: Calcula os valores preditos do modelo.

Conjuntos de dados disponíveis, gerados por simulação:

- **categoricas**: Banco de dados com duas variáveis numéricas, *y* e
  *x*, e duas variáveis categóricas, *fator1* e *fator2*, ambas com
  quatro níveis: “a”, “b”, “c” e “d”.
- **dados**: Banco de dados com três variáveis numéricas, *y*, *x1* e
  *x2*.

Ambos conjuntos de dados foram gerados no R.

## Instalação

Você pode instalar a versão de desenvolvimento do pacote
*regressaolinear* diretamente do GitHub com:

``` r
install.packages("devtools")
devtools::install_github("jv-246531/me918-trabalho2")
```

## Exemplo

Aqui está um exemplo básico de como ajustar um modelo de regressão
linear usando o `regressaolinear`:

``` r
library(regressaolinear)

dados = dados
variavel_resposta = "y"
variaveis_preditoras = c("x1", "x2")

# Valores preditos para a variável resposta
valores_preditos <- regressaolinear::valores_preditos(dados,
                                                       variavel_resposta,
                                                       variaveis_preditoras)

# Resíduos obtidos
residuos_obtidos <- regressaolinear::residuos(dados,
                                               variavel_resposta,
                                               variaveis_preditoras)
```
