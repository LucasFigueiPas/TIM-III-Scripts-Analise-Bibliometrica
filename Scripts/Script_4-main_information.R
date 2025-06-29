# ========================== Script 04 – Main information =======================
# ------------------------------------------------------------------------------
# Finalidade: Apresentar em formato .csv e datatable as estatísticas iniciais
# do conjunto de artigos filtrados de duplicatas. Também aplica limpeza de
# citações com ANONYMOUS ou NO TITLE CAPTURED, preservando os artigos.
# ------------------------------------------------------------------------------

# --------------------- 04.0 - Limpeza de Citações Problemáticas ----------------
dados_final <- limpar_citacoes(dados_final, campo = "CR")

# --------------------- 04.1 - Gerar Estatísticas com biblioAnalysis ------------
resultados <- biblioAnalysis(dados_final, sep = ";")

# --------------------- 04.2 - Resumo dos Principais Indicadores ---------------
resumo <- summary(object = resultados, k = 29, pause = FALSE)
print(resumo$MainInformationDF)

# --------------------- 04.3 - Visualizar Resumo em DataTable ------------------
caminho_html <- file.path(criar_pasta_resultado("04-Main information"), "04_04-MainInformation.html")
dat_main <- datatable(
  resumo$MainInformationDF,
  options = list(pageLength = 30, scrollX = TRUE),
  caption = "Informações sobre a base bibliográfica")
saveWidget(dat_main, file = caminho_html)

# --------------------- 04.4 - Exportar Resumo em Arquivo CSV ------------------
caminho_csv <- file.path(dirname(caminho_html), "04_04-Resumo_MainInformation.csv")
salvar_csv(resumo$MainInformationDF, caminho_csv)
