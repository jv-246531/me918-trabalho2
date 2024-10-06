
# proj2

<!-- badges: start -->
<!-- badges: end -->

O objetivo do pacote *proj2* é ajustar um modelo de regressão linear,
realizar predições e análises residuais, com estimativas dos
coeficientes (betas); efetuar comparações entre os valores preditos e
observados e inserir ferramentes que permitam a análise detalhada dos
resíduos do modelo ajustado, incluindo, por exemplo, resíduos
studentizados e gráficos que permitem verificar normaldiade dos
resíduos.

## Instalação

Você pode instalar a versão de desenvolvimento do pacote proj2
diretamente do GitHub com:

``` r
install.packages("devtools")
devtools::install_github("jv-246531/me918-trabalho2")
```

## Exemplo

Aqui está um exemplo básico de como ajustar um modelo de regressão
linear usando o `proj2`:

``` r
library(proj2)

dados = dados
variavel_resposta = "y"
variaveis_preditoras = c("x1", "x2")

# Valores preditos para a variável resposta
valores_preditos <- proj2::valores_preditos(dados, variavel_resposta, variaveis_preditoras)

# Resíduos obtidos
residuos_obtidos <- proj2::residuos(dados, variavel_resposta, variaveis_preditoras)
```
