# ===== Script 3 – Exclusao artigos fora do tema e Autores Mais Citados ========
# ------------------------------------------------------------------------------
# Finalidade: Eliminar artigos considerados fora do tema, utilizando o
# título como parâmetro, com base em arquivo .txt fornecido pelo usuário.
# ------------------------------------------------------------------------------

# --------------------- 3.1 - Limpeza de Arquivos  -----------------------------
titulos_excluir <- readLines(file.choose(), warn = FALSE)
titulos_excluir <- trimws(titulos_excluir)
titulos_excluir <- titulos_excluir[titulos_excluir != ""]  # Remove linhas vazias

# -------------------------- 3.2 - Padronizar Títulos para Comparar ------------
titulos_excluir_lower <- tolower(titulos_excluir)

dados_final <- dados_final %>%
  mutate(TI_lower = tolower(TI))

# -------------------------- 3.3 - Aplicar Filtro e Remover da Base ------------

n_inicial <- nrow(dados_final)

# Remove títulos considerados fora do tema
dados_final <- dados_final %>%
  mutate(TI_lower = tolower(TI)) %>%
  filter(!(TI_lower %in% titulos_excluir_lower)) %>%
  select(-TI_lower)  # Remove coluna auxiliar

# Manter apenas documentos do tipo ARTICLE
dados_final <- dados_final %>%
  filter(toupper(DT) == "ARTICLE")

n_final <- nrow(dados_final)
n_excluidos <- n_inicial - n_final

# -------------------------- 3.4 - Relatório de Exclusão no Console ------------
pasta_saida <- criar_pasta_resultado("03-Exclusao artigos fora do tema")
caminho_titulos_remov <- file.path(pasta_saida, "03_03-titulos_removidos.csv")
caminho_html_titulos <- file.path(pasta_saida, "03_03-titulos_removidos.html")

# Salvar os títulos excluídos
titulos_removidos <- data.frame(Titulo = titulos_excluir)
salvar_csv(titulos_removidos, caminho_titulos_remov)

# Exportar HTML
dat_tit <- datatable(titulos_removidos, options = list(pageLength = 10, scrollX = TRUE),
                     caption = "Títulos de Artigos Removidos por Estarem Fora do Tema")
saveWidget(dat_tit, file = caminho_html_titulos)

# Relatório no console
cat("Exclusão concluída!\n")
cat("Artigos removidos:", n_excluidos, "\n")
cat("Total restante na base:", n_final, "\n")

# ---------------------------- 3.5 - Definir padrão de exclusão e extrair CR --------------------------

padrao_excluir <- "ANONYMOUS|NO[[:space:]]*TITLE[[:space:]]*CAPTURED"

dados_cr <- metaTagExtraction(dados_final, Field = "CR", sep = ";")

# ----------------------- 3.6 - Calcular frequência por autor ------------------

tabela_citacoes_autores <- dados_cr %>%
  select("CR") %>%
  filter(!is.na(CR)) %>%
  separate_rows(CR, sep = ";") %>%
  mutate(CR = trimws(CR)) %>%
  filter(!str_detect(toupper(CR), padrao_excluir)) %>%
  count(CR, sort = TRUE)

# ----------------------- 3.7 - Exportar resultado em CSV ---------------------
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)

caminho_csv <- file.path(pasta_saida, "03_autores_mais_citados_CR.csv")
write.csv(tabela_citacoes_autores, file = caminho_csv, row.names = FALSE)
