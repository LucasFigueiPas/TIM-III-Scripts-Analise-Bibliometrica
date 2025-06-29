# ========================== Script 8 – Periodicos relevantes ==================
# ------------------------------------------------------------------------------
# Finalidade: Identificar os periódicos (sources) mais relevantes com base
# no número de publicações da base tratada.
# ------------------------------------------------------------------------------

# -------------------------- 8.1 - Extrair Periódicos Mais Relevantes ----------
per <- resumo$MostRelSources
per <- head(per, 10)
print(per)

# -------------------------- 8.2 - Preparar Dados para o Gráfico ---------------
colnames(per) <- trimws(colnames(per))
per$Sources <- factor(per$Sources, levels = rev(per$Sources))
per$Articles <- as.numeric(per$Articles)

# -------------------------- 8.3 - Criar Diretório ------------------------------
pasta_saida <- criar_pasta_resultado("08-Periodicos relevantes")
caminho_grafico <- file.path(pasta_saida, "08_08-per_rel.png")
caminho_csv_top <- file.path(pasta_saida, "08_08-periodicos_mais_relevantes_top20.csv")
caminho_csv_todos <- file.path(pasta_saida, "08_08-periodicos_todos.csv")

# -------------------------- 8.4 - Exportar Top 20 Periódicos ------------------
salvar_csv(per, caminho_csv_top)

# -------------------------- 8.5 - Exportar Todos os Periódicos ----------------
periodicos_todos <- dados_final %>%
  filter(!is.na(SO) & SO != "") %>%
  group_by(SO) %>%
  summarise(Numero_Artigos = n(), .groups = "drop") %>%
  arrange(desc(Numero_Artigos)) %>%
  rename(Periodico = SO)

salvar_csv(periodicos_todos, caminho_csv_todos)

# -------------------------- 8.6 - Gerar Gráfico de Barras ---------------------
g_per <- ggplot(per, aes(x = Sources, y = Articles)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "",
       x = "Periódico", y = "Número de Artigos") +
  theme_minimal()

# -------------------------- 8.7 - Salvar Gráfico ------------------------------
ggsave(filename = caminho_grafico, plot = g_per,
       width = 9, height = 6, dpi = 300)
