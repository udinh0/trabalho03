library(plumber)
library(lubridate)

#* @apiTitle API para regressão linear.
#* @apiDescription Essa API tem como objetivo aplicar regressão em um conjunto de dados pré existente.

suppressPackageStartupMessages(library(tidyverse))
library(glue)
dados = read_csv("dados_regressao.csv", show_col_types = FALSE)
fit = lm(y ~ x + grupo, data = dados)

#* Novos registros
#* @param x parametro x
#* @param grupo parametro grupo
#* @param y parametro y
#* @post /registro
function(x, grupo, y) {
  df = data.frame(x = as.double(x), grupo = as.character(grupo), y = as.double(y), momento_registro = now())
  dados = bind_rows(dados, df)
  write_csv(dados, "dados_regressao.csv")
  print("Usuario cadastrado")
}

#* Deletar registros
#* @param l linha a ser deletada
#* @delete /deletar_registro
function(l) {
  l = as.numeric(l)
  if (l > nrow(dados) || l < 1) {
    stop("Número de linha inválido.")
  }
  readr::write_csv(dados[-l,], "dados_regressao.csv")
  print("Registro deletado com sucesso.")
}

#* Modificar registros
#* @param l linha a ser modificada
#* @param x parametro x
#* @param grupo parametro grupo
#* @param y parametro y
#* @post /modificar_registro
function(l, x, grupo, y) {
  dados = read_csv("dados_regressao.csv", show_col_types = FALSE)
  l = as.numeric(l)
  if (l > nrow(dados) || l < 1) {
    stop("Número de linha inválido.")
  }
  dados[l, "x"] = as.double(x)
  dados[l, "grupo"] = as.character(grupo)
  dados[l, "y"] = as.double(y)
  dados[l, "momento_registro"] = lubridate::now(tzone = "America/Sao_Paulo")
  write_csv(dados, "dados_regressao.csv")
  print("Registro modificado com sucesso.")
}

#* Inferências Gráfico
#* @serializer png
#* @get /grafico
function() {
  g1 = dados %>% 
    ggplot(aes(x = x, y = y, col = grupo)) + 
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, formula = "y ~ x")
  plot(g1)
}

#* Resíduos
#* @serializer json
#* @get /residuos
function() {
  fit$residuals
}

#* Gráficos resíduos
#* @serializer png
#* @get /plot_residuos
function() {
  par(mfrow = c(1, 3))
  plot(x = scale(fit$residuals), y = scale(dados$y), pch = 19,
       xlab = "Resíduos padronizados",
       ylab = "Y padronizado",
       main = "Y vs residuos")
  abline(h = 0, col = "red")
  hist(fit$residuals, col = "turquoise3",
       xlab = "Resíduos",
       ylab = "Frequência",
       main = "Histograma dos resíduos")
  qqnorm(fit$residuals, pch = 19, main = "Normal Q-Q Plot resíduos")
  qqline(fit$residuals)
}

#* Significância estatistica
#* @serializer json
#* @get /significancia
function() {
  coef(summary(fit))
}

#* Predição
#* @serializer json
#* @param string Lista em formato JSON para previsão.
#* @get /predicao
function(string) {
  data = jsonlite::fromJSON(string)
  df = data.frame(x = as.double(data$x), grupo = data$grupo)
  pred = predict(fit, df)
  pred
}

