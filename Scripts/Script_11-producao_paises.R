# ========================== Script 11 – Producao paises =======================
# ------------------------------------------------------------------------------
# Finalidade: Analisar a produção científica por país com base nos artigos
# da base bibliográfica tratada.
# ------------------------------------------------------------------------------

# -------------------------- 11.1 - Extração e preparação dos dados ------------
paises <- resumo$MostProdCountries
print(paises)

colnames(paises) <- trimws(colnames(paises))
paises$Articles <- as.numeric(paises$Articles)
paises$Country <- factor(paises$Country, levels = rev(paises$Country))

# -------------------------- 11.2 - Criar diretório de saída -------------------
pasta_base <- file.path(Sys.getenv("USERPROFILE"), "Documents")
pasta_saida <- file.path(Sys.getenv("USERPROFILE"), "Documents", "Resultados", "11-Producao paises")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

caminho_png <- file.path(pasta_saida, "11-producao_por_pais.png")
caminho_csv <- file.path(pasta_saida, "11-producao_por_pais.csv")

# -------------------------- 11.3 - Gráfico da Produção por País ---------------
grafico_paises <- ggplot(paises, aes(x = Country, y = Articles)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Produção Científica por País",
       x = "País", y = "Número de Artigos") +
  theme_minimal()

ggsave(filename = caminho_png, plot = grafico_paises, width = 9, height = 6, dpi = 300)

# -------------------------- 11.4 - Exportar Tabela ----------------------------
write.csv(paises, file = caminho_csv, row.names = FALSE, quote = TRUE)
