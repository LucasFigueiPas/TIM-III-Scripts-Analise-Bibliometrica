# ========================== Script 05 – Producao anual =========================
# Finalidade: Analisar a produção científica anual da base bibliográfica,
# incluindo total de publicações por ano e taxa média de crescimento, com destaques.
# ------------------------------------------------------------------------------

# -------------------------- 05.1 - Gerar Produção Científica Anual -------------
prod_an <- resumo$AnnualProduction
prod_an$Year <- as.numeric(as.character(prod_an$Year))
# -------------------------- 05.2 - Calcular Taxa de Crescimento ----------------
tx_cresc <- resumo$AnnualGrowthRate

# -------------------------- 05.3 - Exibir Resultados no Console ----------------
print(prod_an)
cat("\n Taxa de crescimento anual:", round(tx_cresc, 2), "%\n")

# -------------------------- 05.4 - Criar Diretório para Gráficos ---------------
pasta_saida <- criar_pasta_resultado("05-Producao anual")
caminho_barra <- file.path(pasta_saida, "05_05-prod_an_barras.png")

# -------------------------- 05.5 - Preparar Dados ------------------------------
colnames(prod_an) <- c("Year", "Articles")
prod_an$Year <- as.numeric(as.character(prod_an$Year))

# -------------------------- 05.6 - Gráfico de Barras com Destaques -------------

# Garantir que todos os anos do intervalo estejam presentes
anos_completos <- data.frame(Year = seq(min(prod_an$Year), max(prod_an$Year), by = 1))
prod_an_completo <- merge(anos_completos, prod_an, by = "Year", all.x = TRUE)
prod_an_completo$Articles[is.na(prod_an_completo$Articles)] <- 0

# Abrir o dispositivo gráfico
png(filename = caminho_barra, width = 1000, height = 600)

# Criar gráfico de barras
bar_positions <- barplot(
  height = prod_an_completo$Articles,
  names.arg = prod_an_completo$Year,
  main = "Produção Científica Anual",
  xlab = "Ano", ylab = "Número de Artigos",
  col = "steelblue",
  las = 2,
  border = NA,
  ylim = c(0, max(prod_an_completo$Articles) + 5)
)

# Adicionar valores acima das barras
text(
  x = bar_positions,
  y = prod_an_completo$Articles,
  labels = prod_an_completo$Articles,
  pos = 3, cex = 0.8
)

# Adicionar os pontos nos marcos históricos
marcos_x <- c(1992, 2012, 2015)
marcos_cor <- c("purple", "orange", "forestgreen")
for (i in seq_along(marcos_x)) {
  idx <- which(prod_an_completo$Year == marcos_x[i])
  if (length(idx) == 1) {
    points(x = bar_positions[idx], y = 0.1, pch = 16, col = marcos_cor[i], cex = 1.5)
  }
}

# Adicionar legenda
legend("topleft",
       legend = c("Rio 92", "Rio+20", "Acordo de Paris / ODS"),
       title = "Legenda:",
       col = marcos_cor,
       pch = 16, bty = "n", cex = 0.9)

# Fechar dispositivo gráfico
dev.off()
