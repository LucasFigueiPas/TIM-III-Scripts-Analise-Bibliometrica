# ========================== Script 06 – Autores relevantes ====================
# ------------------------------------------------------------------------------
# Finalidade: Identificar autores mais produtivos e analisar sua evolução
# ao longo do tempo.
# ------------------------------------------------------------------------------

# -------------------------- 06.1 - Listar Autores Mais Produtivos --------------
aut_prod <- resumo$MostProdAuthors
print(aut_prod)

# -------------------------- 06.2 - Produção ao Longo do Tempo ------------------
aut_t <- authorProdOverTime(dados_final, k = 10)
print(aut_t)

# -------------------------- 06.3 - Criar Diretório de Saída --------------------
pasta_saida <- criar_pasta_resultado("06-Autores relevantes")
caminho_autores_png <- file.path(pasta_saida, "06_06-aut_t.png")
caminho_autores_top_csv <- file.path(pasta_saida, "06_06-autores_mais_produtivos_top20.csv")
caminho_autores_full_csv <- file.path(pasta_saida, "06_06-autores_todos.csv")

# -------------------------- 06.4 - Gráfico da Produção por Autor ---------------
png(filename = caminho_autores_png, width = 1000, height = 700)
authorProdOverTime(dados_final, k = 10)
dev.off()

# -------------------------- 06.5 - Exportar Tabela dos Mais Produtivos ---------
salvar_csv(aut_prod, caminho_autores_top_csv)

# -------------------------- 06.6 - Consolidar Dados de Todos os Autores --------
autores_citacoes <- dados_final %>%
  select(AU, TC) %>%
  mutate(AU = strsplit(AU, ";")) %>%
  unnest(AU) %>%
  mutate(AU = trimws(AU)) %>%
  group_by(AU) %>%
  summarise(
    Numero_Artigos = n(),
    Total_Citacoes = sum(TC, na.rm = TRUE),
    .groups = "drop") %>%
  rename(Autor = AU) %>%
  arrange(desc(Numero_Artigos))

# -------------------------- 06.7 - Exportar Tabela Completa --------------------
salvar_csv(autores_citacoes, caminho_autores_full_csv)
