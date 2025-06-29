# ========================== Script 2 – Duplicatas =============================
# ------------------------------------------------------------------------------
# Finalidade: Eliminar duplicatas de arquivos tipo PlainText exportados da
# base Web of Science por DOI (campo DI) e título (campo TI), apresentando
# os resultados no console, em datatables e arquivos .csv.
# ------------------------------------------------------------------------------

# -------------------------- 2.2 - Importar Base PlainText ---------------------
arquivo <- file.choose()  # Selecionar arquivo .txt da Web of Science
dados <- convert2df(file = arquivo, dbsource = "wos", format = "plaintext")

# -------------------------- 2.3 - Remover Duplicatas por DOI -------------------
duplicados_doi <- dados[duplicated(dados$DI) & !is.na(dados$DI), ]

# Criar diretório de saída
pasta_saida <- criar_pasta_resultado("02-Duplicatas")
caminho_doi <- file.path(pasta_saida, "02_02-duplicatas_por_DOI.csv")
caminho_titulo <- file.path(pasta_saida, "02_02-excluidos_por_titulo.csv")
caminho_html_final <- file.path(pasta_saida, "02_02-artigos_sem_duplicatas.html")
caminho_html_removidos <- file.path(pasta_saida, "02_02-duplicatas_removidas.html")

salvar_csv(duplicados_doi, caminho_doi)

n_total <- nrow(dados)
dados_doi <- dados[!duplicated(dados$DI) | is.na(dados$DI), ]
n_pos_doi <- nrow(dados_doi)

# ---------------- 2.4 - Remover Duplicatas por Título (semelhante) ------------
dados_titulo <- duplicatedMatching(dados_doi, Field = "TI", exact = FALSE, tol = 0.95)

excluidos_titulo <- dados_doi[!rownames(dados_doi) %in% rownames(dados_titulo), ]
salvar_csv(excluidos_titulo, caminho_titulo)

dados_final <- dados_titulo  # Base final limpa
n_final <- nrow(dados_final)

# -------------------------- 2.5 - Relatório no Console ------------------------
cat("Total inicial:", n_total, "registros\n")
cat("Total após remoção por DOI:", n_pos_doi, "\n")
cat("Total após remoção por título semelhante:", n_final, "\n")
cat("Duplicatas removidas:", n_total - n_final, "\n")

# -------------------------- 2.6 - Visualização com DataTables -----------------
# Visualização da base final limpa
dat_final <- datatable(dados_final[, c("TI", "DE", "AB")],
                       options = list(pageLength = 10, scrollX = TRUE),
                       caption = "Artigos sem Duplicatas – Título, Palavras-chave e Resumo")
saveWidget(dat_final, file = caminho_html_final)

# Visualização dos artigos removidos (DOI + título semelhante)
duplicatas_total <- rbind(duplicados_doi, excluidos_titulo)

dat_remov <- datatable(duplicatas_total[, c("TI", "AU", "AF")],
                       options = list(pageLength = 10, scrollX = TRUE),
                       caption = "Artigos Removidos como Duplicatas – Título e Autor")
saveWidget(dat_remov, file = caminho_html_removidos)
