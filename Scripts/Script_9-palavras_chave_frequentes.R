# ========================== Script 9 – Palavras chave frequentes ==============
# ------------------------------------------------------------------------------
# Finalidade: Identificar as palavras-chave mais frequentes da base, considerando
# tanto Author Keywords (DE) quanto Keywords Plus (ID).
# ------------------------------------------------------------------------------

# -------------------------- 9.1 - Extrair Author Keywords (DE) ----------------
kw_de <- dados_final %>%
  filter(!is.na(DE) & DE != "") %>%
  separate_rows(DE, sep = ";") %>%
  mutate(DE = trimws(DE)) %>%
  count(DE, sort = TRUE) %>%
  rename(Keyword = DE, Frequency = n)

kw_de_top <- head(kw_de, 20)
print(kw_de_top)

# -------------------------- 9.2 - Extrair Keyword Plus (ID) -------------------
kw_id <- dados_final %>%
  filter(!is.na(ID) & ID != "") %>%
  separate_rows(ID, sep = ";") %>%
  mutate(ID = trimws(ID)) %>%
  count(ID, sort = TRUE) %>%
  rename(Keyword = ID, Frequency = n)

kw_id_top <- head(kw_id, 20)
print(kw_id_top)

# -------------------------- 9.3 - Criar Diretório -----------------------------
pasta_saida <- criar_pasta_resultado("09-Palavras chave frequentes")
caminho_png <- file.path(pasta_saida, "09_09-pal_ch.png")
caminho_csv_todas <- file.path(pasta_saida, "09_09-author_keywords_todas.csv")
caminho_csv_top <- file.path(pasta_saida, "09_09-author_keywords_top20.csv")

# -------------------------- 9.4 - Exportar Tabelas ----------------------------
salvar_csv(kw_de, caminho_csv_todas)
salvar_csv(kw_de_top, caminho_csv_top)

# -------------------------- 9.5 - Gerar Gráfico -------------------------------
graf_pl <- ggplot(kw_de_top, aes(x = reorder(Keyword, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Author Keywords mais Frequentes",
    x = "Palavra-chave",
    y = "Frequência"
  ) +
  theme_minimal()

# -------------------------- 9.6 - Salvar Gráfico ------------------------------
ggsave(filename = caminho_png, plot = graf_pl, width = 9, height = 6, dpi = 300)
