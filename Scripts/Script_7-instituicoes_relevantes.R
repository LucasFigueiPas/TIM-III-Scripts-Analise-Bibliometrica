# ========================== Script 7 – Instituicoes relevantes ================
# ------------------------------------------------------------------------------
# Finalidade: Identificar instituições mais relevantes com base na produção
# científica (número de publicações) na base analisada.
# ------------------------------------------------------------------------------

# -------------------------- 7.1 - Separar e Contar Instituições ---------------
afil <- dados_final %>%
  select(C1) %>%
  filter(!is.na(C1) & C1 != "") %>%
  mutate(inst = sapply(strsplit(C1, ","), function(x) trimws(x[1]))) %>%
  count(inst, sort = TRUE)

print(head(afil, 20))

# -------------------------- 7.2 - Criar Diretório de Saída --------------------
pasta_saida <- criar_pasta_resultado("07-Instituicoes relevantes")
caminho_grafico <- file.path(pasta_saida, "07_07-inst_rel.png")
caminho_csv_todas <- file.path(pasta_saida, "07_07-instituicoes_todas.csv")
caminho_csv_top20 <- file.path(pasta_saida, "07_07-instituicoes_top20.csv")

# -------------------------- 7.3 - Exportar Tabelas de Instituições ------------
salvar_csv(afil, caminho_csv_todas)

# Top 20
top20_af <- afil %>% head(20)
salvar_csv(top20_af, caminho_csv_top20)

# -------------------------- 7.4 - Gráfico das 10 Principais Instituições ------
top_af <- afil %>% head(10)

grafico_inst <- ggplot(top_af, aes(x = reorder(inst, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Instituições mais Produtivas",
       x = "Instituição", y = "Número de Artigos") +
  theme_minimal()

# -------------------------- 7.5 - Salvar Gráfico ------------------------------
ggsave(
  filename = caminho_grafico,
  plot = grafico_inst,
  width = 9,
  height = 6,
  dpi = 300
)
