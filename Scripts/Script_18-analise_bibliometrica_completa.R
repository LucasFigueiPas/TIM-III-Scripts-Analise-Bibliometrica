# =========== Script 1 – Instalacao pacotes e Definição de Funções =============
# ------------------------------------------------------------------------------
# Finalidade: Instalar e carregar os pacotes necessários para executar os
# scripts de análise bibliométrica desenvolvidos no projeto.
# ------------------------------------------------------------------------------

# -------------------------- 1.0. bibliometrix ---------------------------------
if (!require("bibliometrix")) install.packages("bibliometrix")
library(bibliometrix)

# -------------------------- 1.2. DT (DataTables para visualização HTML) -------
if (!require("DT")) install.packages("DT")
library(DT)

# -------------------------- 1.3. ggplot2 (visualizações estáticas) ------------
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# -------------------------- 1.4. dplyr (manipulação de dados) -----------------
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

# -------------------------- 1.5. tidyr (organização de tabelas) ---------------
if (!require("tidyr")) install.packages("tidyr")
library(tidyr)

# -------------------------- 1.6. igraph (redes e grafos) ----------------------
if (!require("igraph")) install.packages("igraph")
library(igraph)

# -------------------------- 1.7. visNetwork (visualização interativa) ---------
if (!require("visNetwork")) install.packages("visNetwork")
library(visNetwork)

# -------------------------- 1.8. scales (normalização e eixos) ----------------
if (!require("scales")) install.packages("scales")
library(scales)

# -------------------------- 1.9. tcltk (janelas gráficas) ---------------------
if (!require("tcltk")) install.packages("tcltk")
library(tcltk)

#--------------------------- 1.10 - reshape2 -----------------------------------
if (!requireNamespace("reshape2", quietly = TRUE)) {
  install.packages("reshape2")}
library(reshape2)

#--------------------------- 1.11 - stringr ------------------------------------
if (!require("stringr")) install.packages("stringr")
library(stringr)
#--------------------------- 1.12. Funções -------------------------------------

# Funções auxiliares reutilizáveis

solicitar_top_n <- function(titulo = "Definir número de itens", default = 60) {
  janela <- tktoplevel()
  tkwm.title(janela, titulo)
  tkwm.geometry(janela, "500x150")
  entrada <- tclVar("60")  # valor padrão sugerido
  tkgrid(tklabel(janela, text = "Digite o número de nós (NULL = todos):"), padx = 10, pady = 5)
  caixa <- tkentry(janela, textvariable = entrada)
  tkgrid(caixa, padx = 10, pady = 5)
  tkgrid(tkbutton(janela, text = "Confirmar", command = function() tkdestroy(janela)))
  tkwait.window(janela)

entrada_txt <- trimws(tclvalue(entrada))
if (entrada_txt == "" || tolower(entrada_txt) == "null") {
  n_top <- NULL
} else if (suppressWarnings(!is.na(as.numeric(entrada_txt)))) {
  n_top <- as.numeric(entrada_txt)
} else {
  stop("Entrada inválida! Digite um número ou deixe em branco para NULL.")
}}

criar_pasta_resultado <- function(nome_script) {
  pasta_base <- file.path(Sys.getenv("USERPROFILE"), "Documents", "Resultados")
  pasta_saida <- file.path(pasta_base, nome_script)
  dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
  return(pasta_saida)
}

salvar_visnetwork <- function(nodes, edges, caminho_html) {
  vis <- visNetwork(nodes, edges, width = "100%", height = "800px") %>%
    visOptions(
      highlightNearest = list(enabled = TRUE, degree = 1, hover = FALSE, algorithm = "hierarchical"),
      nodesIdSelection = list(enabled = TRUE, useLabels = TRUE)
    ) %>%
    visInteraction(navigationButtons = TRUE, zoomView = TRUE) %>%
    visEdges(arrows = "from") %>%
    visIgraphLayout(layout = "layout_with_fr") %>%
    visLayout(improvedLayout = TRUE) %>%
    visPhysics(enabled = FALSE)
  
  visSave(vis, file = caminho_html, selfcontained = TRUE)
}

salvar_png_networkPlot <- function(matriz, top_n, caminho_png, titulo) {
  png(filename = caminho_png, width = 1200, height = 900)
  networkPlot(
    matriz,
    normalize = "association",
    n = top_n,
    Title = titulo,
    type = "fruchterman",
    size = TRUE,
    labelsize = 1.8,
    cluster = "louvain",
    edgesize = 3.5,
    community.repulsion = 0.1,
    remove.isolates = TRUE
  )
  dev.off()
}

salvar_csv <- function(dados, caminho) {
  tryCatch({
    write.csv(dados, file = caminho, row.names = FALSE, fileEncoding = "UTF-8")
    message(paste("Arquivo salvo com sucesso em:", caminho))
  }, error = function(e) {
    message(paste("Erro ao salvar o arquivo CSV em:", caminho))
    message("Detalhes do erro:", e$message)
  })
}

normalizar_cosseno <- function(mat) {
  mat <- as.matrix(mat)
  n <- nrow(mat)
  sim_cos <- matrix(0, n, n)
  rownames(sim_cos) <- rownames(mat)
  colnames(sim_cos) <- colnames(mat)
  
  for (i in 1:n) {
    for (j in i:n) {
      numerador <- mat[i, j]
      denominador <- sqrt(mat[i, i] * mat[j, j])
      valor <- ifelse(denominador == 0, 0, numerador / denominador)
      sim_cos[i, j] <- valor
      sim_cos[j, i] <- valor
    }
  }
  return(sim_cos)
}

limpar_citacoes <- function(dados, campo = "CR") {
  padrao_excluir <- "ANONYMOUS|NO[[:space:]]*TITLE[[:space:]]*CAPTURED"
  
  if (!campo %in% names(dados)) {
    warning(paste("Campo", campo, "não encontrado na base"))
    return(dados)
  }
  
  dados[[campo]] <- sapply(dados[[campo]], function(refs) {
    if (is.na(refs)) return(NA)
    refs_sep <- unlist(strsplit(refs, ";"))
    refs_filtradas <- refs_sep[!grepl(padrao_excluir, toupper(refs_sep))]
    if (length(refs_filtradas) == 0) return(NA)
    paste(trimws(refs_filtradas), collapse = ";")
  })
  
  return(dados)
}

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

# ========================== Script 10 – Rede coocorrencia keywords ============
# ------------------------------------------------------------------------------
# Finalidade: Gerar rede de coocorrência de palavras-chave (Author Keywords)
# com controle do número de nós. Produz visualizações em PNG e HTML e um
# arquivo CSV com dados da rede.
# ------------------------------------------------------------------------------

# -------------------------- 10.1 - Definir Parâmetros -------------------------
top_n_nodes <- solicitar_top_n("Coocorrência Palavras-Chave")

# -------------------------- 10.2 - Gerar Matriz de Coocorrência --------------
mtx_cooc <- biblioNetwork(dados_final,
                          analysis = "co-occurrences",
                          network = "author_keywords",
                          n = top_n_nodes,
                          sep = ";")

# -------------------------- 10.3 - Criar Grafo --------------------------------
grafo <- graph_from_adjacency_matrix(
  mtx_cooc,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

clus <- cluster_louvain(grafo)
V(grafo)$cluster <- clus$membership
grau <- degree(grafo)

# -------------------------- 10.4 - Criar Diretório de Saída -------------------
pasta_saida <- criar_pasta_resultado("10-Rede coocorrencia keywords")
caminho_png <- file.path(pasta_saida, "10_10-rede_co_kw.png")
caminho_csv <- file.path(pasta_saida, "10_10-coocorrencia_dados.csv")
caminho_html <- file.path(pasta_saida, "10_10-rede_coocorrencia.html")

# -------------------------- 10.5 - Exportar PNG -------------------------------
salvar_png_networkPlot(mtx_cooc, top_n_nodes, caminho_png,
                       paste("Rede de Coocorrência – Top", top_n_nodes, "Author Keywords"))

# -------------------------- 10.6 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo)$name,
  label = V(grafo)$name,
  group = paste("Cluster", V(grafo)$cluster),
  value = grau,
  title = paste0("Palavra-chave: ", V(grafo)$name,
                 "<br>Grau: ", grau,
                 "<br>Cluster: ", V(grafo)$cluster),
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo, what = "edges")
if (ncol(edges) >= 2) colnames(edges)[1:2] <- c("from", "to")

edges <- edges %>%
  mutate(
    shadow = TRUE,
    smooth = TRUE,
    dashes = FALSE,
  )

# -------------------------- 10.7 - Exportar CSV -------------------------------
write.csv(nodes %>%
            select(Keyword = label,
                   Cluster = group,
                   Grau = value),
          file = caminho_csv,
          row.names = FALSE)

# -------------------------- 10.8 - Visualização Interativa --------------------
salvar_visnetwork(nodes, edges, caminho_html)


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

# ========================== Script 12 – Mapa colaboracao paises ===============
# ------------------------------------------------------------------------------
# Finalidade: Gerar o mapa de colaboração científica entre países com base
# em coautorias internacionais.
# ------------------------------------------------------------------------------

# -------------------------- 12.1 - Definir Parâmetros -------------------------
top_n_paises <- solicitar_top_n("Mapa colaboração de países")

# -------------------------- 12.2 - Preparar Matriz de Colaboração -------------
dados_final <- metaTagExtraction(dados_final, Field = "AU_CO", sep = ";")

rede_paises <- biblioNetwork(dados_final,
                             analysis = "collaboration",
                             network = "countries",
                             sep = ";")

# -------------------------- 12.3 - Criar Grafo --------------------------------
if (!is.null(top_n_paises)) {
  grau_total <- rowSums(rede_paises)
  paises_selecionados <- names(sort(grau_total, decreasing = TRUE))[1:top_n_paises]
  rede_paises <- rede_paises[paises_selecionados, paises_selecionados]
}

grafo_colab <- graph_from_adjacency_matrix(
  rede_paises,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)
clus <- cluster_louvain(grafo_colab)
V(grafo_colab)$cluster <- clus$membership
grau <- degree(grafo_colab)

# -------------------------- 12.4 - Criar Diretório de Saída -------------------
pasta_saida <- criar_pasta_resultado("12-Mapa colaboracao paises")
caminho_png <- file.path(pasta_saida, "12-mapa_colaboracao_paises.png")
caminho_csv <- file.path(pasta_saida, "12-colaboracao_paises.csv")
caminho_html <- file.path(pasta_saida, "12-mapa_colaboracao_paises.html")

# -------------------------- 12.5 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo_colab)$name,
  label = V(grafo_colab)$name,
  group = paste("Cluster", V(grafo_colab)$cluster),
  value = grau,
  title = paste0("País: ", V(grafo_colab)$name,
                 "<br>Grau: ", grau,
                 "<br>Cluster: ", V(grafo_colab)$cluster),
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo_colab, what = "edges")
if (ncol(edges) >= 2) colnames(edges)[1:2] <- c("from", "to")

edges <- edges %>%
  mutate(
    shadow = TRUE,
    smooth = TRUE,
    dashes = FALSE,
  )

# -------------------------- 12.6 - Estatísticas e Exportação ------------------
betweenness <- betweenness(grafo_colab, normalized = TRUE)
closeness <- closeness(grafo_colab, normalized = TRUE)

nodes_export <- nodes %>%
  rename(Pais = label, Cluster = group, Grau = value) %>%
  mutate(
    Betweenness = round(betweenness, 4),
    Closeness = round(closeness, 4)
  )

write.csv(nodes_export, file = caminho_csv, row.names = FALSE)

# -------------------------- 12.7 - Visualização Interativa --------------------
salvar_visnetwork(nodes, edges, caminho_html)

# -------------------------- 12.8 - Exportar PNG Estático ----------------------
png(filename = caminho_png, width = 1200, height = 900)
networkPlot(rede_paises,
            n = 20,
            Title = "Mapa de Colaboração entre Países",
            type = "fruchterman",
            size = TRUE,
            labelsize = 2,
            edgesize = 3,
            remove.isolates = TRUE)
dev.off()

# ===================== Script 13 – Rede colaboracao autores leve ==============
# ------------------------------------------------------------------------------
# Finalidade: Gerar a rede de colaboração entre autores com base nas coautorias.
# ------------------------------------------------------------------------------

# -------------------------- 13.1 - Definir Parâmetros -------------------------
top_n_autores <- solicitar_top_n("Rede de colaboração de autores")

# -------------------------- 13.2 - Matriz de Colaboração ----------------------
rede_autores <- biblioNetwork(dados_final,
                              analysis = "collaboration",
                              network = "authors",
                              sep = ";")

# -------------------------- 13.3 - Criar Grafo --------------------------------
grafo_autores_completo <- graph_from_adjacency_matrix(
  rede_autores,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

grau_completo <- degree(grafo_autores_completo)

if (!is.null(top_n_autores) && length(grau_completo) > top_n_autores) {
  autores_filtrados <- names(sort(grau_completo, decreasing = TRUE))[1:top_n_autores]
  grafo_autores <- induced_subgraph(grafo_autores_completo, vids = autores_filtrados)
} else {
  grafo_autores <- grafo_autores_completo
}

# -------------------------- 13.4 - Métricas e Clusters ------------------------
clus <- cluster_louvain(grafo_autores)
V(grafo_autores)$cluster <- clus$membership
grau <- degree(grafo_autores)

# -------------------------- 13.5 - Diretório de Saída -------------------------
pasta_saida <- criar_pasta_resultado("13-Rede colaboracao autores leve")
caminho_png  <- file.path(pasta_saida, "13_rede_colaboracao_autores.png")
caminho_csv  <- file.path(pasta_saida, "13_colaboracao_autores.csv")
caminho_html <- file.path(pasta_saida, "13_rede_colaboracao_autores.html")

# -------------------------- 13.6 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo_autores)$name,
  label = V(grafo_autores)$name,
  group = paste("Cluster", V(grafo_autores)$cluster),
  value = grau,
  title = paste0("Autor: ", V(grafo_autores)$name,
                 "<br>Grau: ", grau,
                 "<br>Cluster: ", V(grafo_autores)$cluster),
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo_autores, what = "edges")
if (ncol(edges) >= 2) colnames(edges)[1:2] <- c("from", "to")

edges <- edges %>%
  mutate(
    shadow = TRUE,
    smooth = TRUE,
    dashes = FALSE,
  )

# -------------------------- 13.7 - Estatísticas e Exportação ------------------
write.csv(nodes %>%
            select(Autor = label,
                   Cluster = group,
                   Grau = value),
          file = caminho_csv,
          row.names = FALSE)

# -------------------------- 13.8 - Visualização Interativa --------------------
salvar_visnetwork(nodes, edges, caminho_html)

# -------------------------- 13.9 - Exportar PNG Estático ----------------------
png(caminho_png, width = 1200, height = 900)
networkPlot(
  as_adjacency_matrix(grafo_autores, sparse = FALSE),
  Title = ifelse(is.null(top_n_autores),
                 "Rede de Colaboração entre Autores",
                 paste("Rede de Colaboração – Top", top_n_autores, "Autores")),
  type = "fruchterman",
  size = TRUE,
  labelsize = 1.5,
  cluster = "louvain",
  edgesize = 3,
  remove.isolates = TRUE
)
dev.off()

# ===================== Script 14 – Rede colaboracao instituicoes leve =========
# ------------------------------------------------------------------------------
# Finalidade: Gerar a rede de colaboração entre instituições com base nas coautorias.
# ------------------------------------------------------------------------------

# -------------------------- 14.1 - Definir Parâmetros -------------------------
top_n_instituicoes <- solicitar_top_n("Rede colaboração de instituições")

# -------------------------- 14.2 - Matriz de Colaboração ----------------------
rede_inst <- biblioNetwork(dados_final,
                           analysis = "collaboration",
                           network = "universities",
                           sep = ";")

# -------------------------- 14.3 - Criar Grafo --------------------------------
grafo_completo <- graph_from_adjacency_matrix(
  rede_inst,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

graus_completo <- degree(grafo_completo)

if (!is.null(top_n_instituicoes) && length(graus_completo) > top_n_instituicoes) {
  inst_selecionadas <- names(sort(graus_completo, decreasing = TRUE))[1:top_n_instituicoes]
  grafo <- induced_subgraph(grafo_completo, vids = inst_selecionadas)
} else {
  grafo <- grafo_completo
}

# -------------------------- 14.4 - Métricas e Clusters ------------------------
graus <- degree(grafo)
clus <- cluster_louvain(grafo)
V(grafo)$cluster <- clus$membership

# -------------------------- 14.5 - Diretório de Saída -------------------------
pasta_saida <- criar_pasta_resultado("15-Rede colaboracao instituicoes leve")
caminho_csv   <- file.path(pasta_saida, "14-colaboracao_instituicoes.csv")
caminho_png   <- file.path(pasta_saida, "14-rede_colaboracao_instituicoes.png")
caminho_html  <- file.path(pasta_saida, "14-rede_colaboracao_instituicoes.html")

# -------------------------- 14.6 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo)$name,
  label = V(grafo)$name,
  group = paste("Cluster", V(grafo)$cluster),
  shape = "dot",
  value = graus,
  title = paste0("Instituição: ", V(grafo)$name,
                 "<br>Grau: ", graus,
                 "<br>Cluster: ", V(grafo)$cluster),
  shadow = TRUE,
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo, what = "edges")
if (ncol(edges) >= 2) colnames(edges)[1:2] <- c("from", "to")

edges <- edges %>%
  mutate(
    shadow = TRUE,
    smooth = TRUE,
    dashes = FALSE,
  )

# -------------------------- 14.7 - Estatísticas e Exportação ------------------
write.csv(
  nodes %>% select(Instituicao = label, Cluster = group, Grau = value),
  file = caminho_csv,
  row.names = FALSE
)

# -------------------------- 14.8 - Visualização Interativa --------------------
salvar_visnetwork(nodes, edges, caminho_html)

# -------------------------- 14.9 - Exportar PNG Estático ----------------------
png(caminho_png, width = 1200, height = 900)
networkPlot(
  as_adjacency_matrix(grafo, sparse = FALSE),
  Title = ifelse(is.null(top_n_instituicoes),
                 "Rede de Colaboração entre Instituições",
                 paste("Rede de Colaboração – Top", top_n_instituicoes, "Instituições")),
  type = "fruchterman",
  size = TRUE,
  labelsize = 1.5,
  cluster = "louvain",
  edgesize = 3,
  remove.isolates = TRUE
)
dev.off()

# ========================== Script 15 – Rede cocitacao autores leve ==========
# ------------------------------------------------------------------------------
# Finalidade: Gerar a rede de cocitação de autores nas formas estática e interativa.
# ------------------------------------------------------------------------------

# -------------------------- 15.1 - Definir Parâmetros -------------------------
top_n_autores <- solicitar_top_n("Rede Cocitação de Autores")
pasta_saida <- criar_pasta_resultado("15-Rede cocitacao autores leve")

# -------------------------- 15.2 - Matriz de Cocitação ------------------------

dados_cr <- limpar_citacoes(dados_final, campo = "CR")
top_n_matriz_autores <- top_n_autores
dados_f <- metaTagExtraction(dados_cr, Field = "CR_AU", sep = ";")
rede_cocit <- biblioNetwork(dados_f,
                            analysis = "co-citation",
                            network = "authors",
                            sep = ";")

# ---------- 15.2.1 - Normalizar matriz com cosseno de Salton ------------------

# Exportar matriz de cocitação bruta
caminho_matriz_frequencia <- file.path(pasta_saida, "16_16_matriz_frequencia_autores.csv")
somas_autores <- Matrix::rowSums(rede_cocit)

if (!is.null(top_n_matriz_autores)) {
  top_autores_matriz <- names(sort(somas_autores, decreasing = TRUE))[1:top_n_matriz_autores]
} else {
  top_autores_matriz <- names(sort(somas_autores, decreasing = TRUE))
}

matriz_filtrada <- rede_cocit[top_autores_matriz, top_autores_matriz]
write.csv(as.matrix(matriz_filtrada), file = caminho_matriz_frequencia, row.names = TRUE, fileEncoding = "UTF-8")
matriz_cosseno <- normalizar_cosseno(matriz_filtrada)

# Exportar matriz normalizada por cosseno
caminho_matriz_cosseno <- file.path(pasta_saida, "15_matriz_cosseno_autores.csv")
write.csv(matriz_cosseno, file = caminho_matriz_cosseno, row.names = TRUE, fileEncoding = "UTF-8")

# -------------------------- 15.3 - Criar Grafo --------------------------------
# Usar rede de cocitação por frequência, com pesos originais
matriz_frequencia <- matriz_filtrada  # pesos = número de cocitações

grafo_completo <- graph_from_adjacency_matrix(
  matriz_frequencia,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

graus_completo <- degree(grafo_completo)

if (!is.null(top_n_autores) && length(graus_completo) > top_n_autores) {
  autores_selecionados <- names(sort(graus_completo, decreasing = TRUE))[1:top_n_autores]
  grafo <- induced_subgraph(grafo_completo, vids = autores_selecionados)
} else {
  grafo <- grafo_completo
}

# -------------------------- 15.4 - Métricas e Clusters ------------------------
grau       <- degree(grafo)
betweenness <- betweenness(grafo, normalized = TRUE)
closeness   <- closeness(grafo, normalized = TRUE)
clus        <- cluster_louvain(grafo)
V(grafo)$cluster <- clus$membership

# -------------------------- 15.5 - Diretório de Saída -------------------------
caminho_csv   <- file.path(pasta_saida, "15_cocitacao_autores.csv")
caminho_png   <- file.path(pasta_saida, "15_rede_cocitacao_autores.png")
caminho_html  <- file.path(pasta_saida, "15_rede_cocitacao_autores.html")

# -------------------------- 15.6 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo)$name,
  label = V(grafo)$name,
  group = paste("Cluster", V(grafo)$cluster),
  shape = "dot",
  value = grau,
  title = paste0("Autor: ", V(grafo)$name,
                 "<br>Grau: ", grau,
                 "<br>Cluster: ", V(grafo)$cluster),
  shadow = TRUE,
  font = list(size = 20, color = "black", face = "arial", vadjust = -10),
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo, what = "edges")
edges <- edges %>%
  rename(value = weight) %>%
  mutate(
    title = paste0("Número de Cocitações: ", value),
    label = value,
    arrows = NA
  )

# -------------------------- 15.7 - Estatísticas e Exportação ------------------
nodes_export <- nodes %>%
  rename(Autor = label, Cluster = group, Grau = value) %>%
  mutate(
    Grau = grau,
    Betweenness = round(betweenness, 4),
    Closeness = round(closeness, 4)
  )

write.csv(nodes_export, file = caminho_csv, row.names = FALSE)

# -------------------------- 15.8 - Visualização Interativa --------------------
visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visEdges(smooth = FALSE, arrows = list(to = list(enabled = FALSE), from = list(enabled = FALSE))) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(
    enabled = TRUE,
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 200,     # Aumenta distância entre nós
      centralGravity = 0.2,   # Reduz força de atração ao centro
      springLength = 200,     # Aumenta "comprimento da mola"
      springConstant = 0.02   # Deixa as ligações mais "moles"
    ),
    stabilization = list(enabled = TRUE, iterations = 200)
  ) %>%
  htmlwidgets::saveWidget(file = caminho_html)

# -------------------------- 15.9 - Exportar PNG Estático ----------------------
png(caminho_png, width = 1200, height = 900)
networkPlot(
  as_adjacency_matrix(grafo, sparse = FALSE),
  Title = ifelse(is.null(top_n_autores),
                 "Rede de Co-citação entre Autores",
                 paste("Rede de Co-citação entre Autores – Top", top_n_autores)),
  type = "fruchterman",
  size = TRUE,
  labelsize = 1.5,
  cluster = "louvain",
  edgesize = 3,
  remove.isolates = TRUE
)
dev.off()

# ========================== Script 16 – Rede cocitacao artigos leve ===========
# ------------------------------------------------------------------------------
# Finalidade: Gerar a rede de cocitação de documentos nas formas estática e interativa.
# ------------------------------------------------------------------------------

# -------------------------- 16.1 - Definir Parâmetros -------------------------
top_n_docs <- solicitar_top_n("Rede Cocitação de Documentos")
top_n_matriz_docs <- top_n_docs  # garantir que a matriz e o grafo tenham os mesmos documentos

# -------------------------- 16.2 - Matriz de Cocitação ------------------------
# Limpeza do campo CR para remover ANONYMOUS e NO TITLE CAPTURED
dados_cr <- limpar_citacoes(dados_final, campo = "CR")
dados_f <- metaTagExtraction(dados_cr, Field = "CR", sep = ";")
rede_docs <- biblioNetwork(dados_f,
                           analysis = "co-citation",
                           network = "references",
                           sep = ";")

# ---------- 16.2.1 - Selecionar documentos e normalizar matriz ----------------
somas_docs <- Matrix::rowSums(rede_docs)

if (!is.null(top_n_matriz_docs)) {
  top_docs_matriz <- names(sort(somas_docs, decreasing = TRUE))[1:top_n_matriz_docs]
} else {
  top_docs_matriz <- names(sort(somas_docs, decreasing = TRUE))
}

matriz_filtrada_docs <- rede_docs[top_docs_matriz, top_docs_matriz]
matriz_cosseno_docs <- normalizar_cosseno(matriz_filtrada_docs)

# -------------------------- 16.2.2 - Exportar Matrizes ------------------------
pasta_saida <- criar_pasta_resultado("16-Rede cocitacao artigos leve")
caminho_cosseno_docs <- file.path(pasta_saida, "16_matriz_cosseno_documentos.csv")
write.csv(as.matrix(matriz_cosseno_docs), file = caminho_cosseno_docs, row.names = TRUE, fileEncoding = "UTF-8")

caminho_frequencia_docs <- file.path(pasta_saida, "16_matriz_frequencia_documentos.csv")
write.csv(as.matrix(matriz_filtrada_docs), file = caminho_frequencia_docs, row.names = TRUE, fileEncoding = "UTF-8")

# -------------------------- 16.3 - Criar Grafo --------------------------------
grafo_completo <- graph_from_adjacency_matrix(
  matriz_filtrada_docs,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

graus_completo <- degree(grafo_completo)

if (!is.null(top_n_docs) && length(graus_completo) > top_n_docs) {
  docs_selecionados <- names(sort(graus_completo, decreasing = TRUE))[1:top_n_docs]
  grafo <- induced_subgraph(grafo_completo, vids = docs_selecionados)
} else {
  grafo <- grafo_completo
}

# -------------------------- 16.4 - Métricas e Clusters ------------------------
grau       <- degree(grafo)
betweenness <- betweenness(grafo, normalized = TRUE)
closeness   <- closeness(grafo, normalized = TRUE)
clus        <- cluster_louvain(delete_vertices(grafo, grau == 0))
V(grafo)$cluster <- NA
V(grafo)[grau > 0]$cluster <- clus$membership

# -------------------------- 16.5 - Diretório de Saída -------------------------
caminho_csv   <- file.path(pasta_saida, "16_cocitacao_documentos.csv")
caminho_png   <- file.path(pasta_saida, "16_rede_cocitacao_documentos.png")
caminho_html  <- file.path(pasta_saida, "16_rede_cocitacao_documentos.html")

# -------------------------- 16.6 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo)$name,
  label = V(grafo)$name,
  group = paste("Cluster", V(grafo)$cluster),
  shape = "dot",
  value = grau,
  title = paste0("Artigo: ", V(grafo)$name,
                 "<br>Grau: ", grau,
                 "<br>Cluster: ", V(grafo)$cluster),
  shadow = TRUE,
  font = list(size = 20, color = "black", face = "arial", vadjust = -10),
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo, what = "edges") %>%
  rename(from = from, to = to, value = weight) %>%
  mutate(
    title = paste0("Cocitações: ", round(value, 3)),
    label = round(value, 3)
  )

# -------------------------- 16.7 - Citações Completas -------------------------
citacoes_cr <- dados_final %>%
  select(CR) %>%
  filter(!is.na(CR)) %>%
  separate_rows(CR, sep = ";") %>%
  mutate(CR = trimws(CR)) %>%
  distinct()

nodes_export <- nodes %>%
  rename(Citacao_Original = label, Cluster = group, Co_citacoes = value) %>%
  mutate(
    Grau = grau,
    Betweenness = round(betweenness, 4),
    Closeness = round(closeness, 4)
  ) %>%
  left_join(citacoes_cr %>% rename(Citacao_Completa = CR),
            by = c("Citacao_Original" = "Citacao_Completa"))

write.csv(nodes_export, file = caminho_csv, row.names = FALSE)

# -------------------------- 16.8 - Visualização Interativa --------------------
visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visEdges(smooth = FALSE, arrows = list(to = list(enabled = FALSE), from = list(enabled = FALSE))) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(
    enabled = TRUE,
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 200,
      centralGravity = 0.2,
      springLength = 200,
      springConstant = 0.02
    ),
    stabilization = list(enabled = TRUE, iterations = 200)
  ) %>%
  htmlwidgets::saveWidget(file = caminho_html)

# -------------------------- 16.9 - Exportar PNG Estático ----------------------
png(caminho_png, width = 1200, height = 900)
networkPlot(
  as_adjacency_matrix(grafo, sparse = FALSE),
  Title = ifelse(is.null(top_n_docs),
                 "Rede de Co-citação de Documentos",
                 paste("Rede de Co-citação de Documentos – Top", top_n_docs)),
  type = "fruchterman",
  size = TRUE,
  labelsize = 1.5,
  cluster = "louvain",
  edgesize = 3,
  remove.isolates = TRUE
)
dev.off()

# -------------------------- 16.10 - Top 10 Artigos Mais Citados ----------------

# 1. Separar as referências citadas (campo CR)
top_citacoes <- dados_final %>%
  select(CR) %>%
  filter(!is.na(CR)) %>%
  separate_rows(CR, sep = ";") %>%
  mutate(CR = trimws(CR)) %>%
  count(CR, sort = TRUE)

# 2. Selecionar os 10 artigos mais citados
top10_citacoes <- top_citacoes %>% slice_max(n, n = 10)

# 3. Salvar como CSV
pasta_saida <- criar_pasta_resultado("16-Top Artigos Mais Citados")
caminho_csv <- file.path(pasta_saida, "16_top10_artigos_mais_citados.csv")
write.csv(top10_citacoes, caminho_csv, row.names = FALSE)

# Exibir no console
print(top10_citacoes)

# ========================== Script 17 – Rede cocitacao periodicos leve ========
# ------------------------------------------------------------------------------
# Finalidade: Criar a rede de co-citação de periódicos e veículos de publicação.
# ------------------------------------------------------------------------------

# -------------------------- 17.1 - Definir Parâmetros -------------------------
top_n_fontes <- solicitar_top_n("Rede Cocitação de Fontes")

# -------------------------- 17.2 - Matriz de Cocitação ------------------------
# Limpeza do campo CR_SO antes da extração para remover ANONYMOUS e NO TITLE CAPTURED
dados_cr <- limpar_citacoes(dados_final, campo = "CR_SO")

pasta_saida <- criar_pasta_resultado("17-Rede cocitacao periodicos leve")


# ---------- 17.2.1 - Normalizar matriz com cosseno de Salton ------------------

dados_f <- metaTagExtraction(dados_cr, Field = "CR_SO", sep = ";")
top_n_matriz_fontes <- top_n_fontes
rede_fontes <- biblioNetwork(dados_f,
                             analysis = "co-citation",
                             network = "sources",
                             sep = ";")
somas_fontes <- Matrix::rowSums(rede_fontes)
if (!is.null(top_n_matriz_fontes)) {
  top_fontes <- names(sort(somas_fontes, decreasing = TRUE))[1:top_n_matriz_fontes]
} else {
  top_fontes <- names(sort(somas_fontes, decreasing = TRUE))
}
matriz_filtrada_fontes <- rede_fontes[top_fontes, top_fontes]

# Exportar matriz de frequência bruta
caminho_frequencia_fontes <- file.path(pasta_saida, "17_matriz_frequencia_fontes.csv")
write.csv(as.matrix(matriz_filtrada_fontes), file = caminho_frequencia_fontes, row.names = TRUE, fileEncoding = "UTF-8")

# Normalizar por cosseno de Salton
matriz_cosseno_fontes <- normalizar_cosseno(matriz_filtrada_fontes)
caminho_cosseno_fontes <- file.path(pasta_saida, "17_matriz_cosseno_fontes.csv")
write.csv(matriz_cosseno_fontes, file = caminho_cosseno_fontes, row.names = TRUE, fileEncoding = "UTF-8")
dados_f <- metaTagExtraction(dados_final, Field = "CR_SO", sep = ";")

# -------------------------- 17.3 - Criar Grafo --------------------------------
# Construir grafo com base na matriz de frequência bruta

grafo_completo <- graph_from_adjacency_matrix(
  matriz_filtrada_fontes,
  mode = "undirected",
  weighted = TRUE,
  diag = FALSE
)

graus_completo <- degree(grafo_completo)

if (!is.null(top_n_fontes) && length(graus_completo) > top_n_fontes) {
  fontes_selecionadas <- names(sort(graus_completo, decreasing = TRUE))[1:top_n_fontes]
  grafo <- induced_subgraph(grafo_completo, vids = fontes_selecionadas)
} else {
  grafo <- grafo_completo
}

# -------------------------- 17.4 - Métricas e Clusters ------------------------
grau       <- degree(grafo)
betweenness <- betweenness(grafo, normalized = TRUE)
closeness   <- closeness(grafo, normalized = TRUE)
clus        <- cluster_louvain(delete_vertices(grafo, grau == 0))
V(grafo)$cluster <- NA
V(grafo)[grau > 0]$cluster <- clus$membership

# -------------------------- 17.5 - Diretório de Saída -------------------------
caminho_csv   <- file.path(pasta_saida, "17_cocitacao_fontes.csv")
caminho_png   <- file.path(pasta_saida, "17_rede_cocitacao_fontes.png")
caminho_html  <- file.path(pasta_saida, "17_rede_cocitacao_fontes.html")

# -------------------------- 17.6 - Criar Data Frames --------------------------
nodes <- data.frame(
  id = V(grafo)$name,
  label = V(grafo)$name,
  group = paste("Cluster", V(grafo)$cluster),
  shape = "dot",
  value = grau,
  title = paste0("Fonte: ", V(grafo)$name,
                 "<br>Grau: ", grau,
                 "<br>Cluster: ", V(grafo)$cluster),
  shadow = TRUE,
  font = list(size = 20, color = "black", face = "arial", vadjust = -10),
  stringsAsFactors = FALSE
)

edges <- as_data_frame(grafo, what = "edges")
if (ncol(edges) >= 2) colnames(edges)[1:2] <- c("from", "to")

edges <- edges %>%
  rename(value = weight) %>%
  mutate(
    title = paste0("Cocitações: ", round(value, 3)),
    value = value * 10  # opcional: escala para visualização
  )

# -------------------------- 17.7 - Exportar Estatísticas ----------------------
nodes_export <- nodes %>%
  rename(Fonte = label, Cluster = group, Co_citacoes = value) %>%
  mutate(
    Grau = grau,
    Betweenness = round(betweenness, 4),
    Closeness = round(closeness, 4)
  )

write.csv(nodes_export, file = caminho_csv, row.names = FALSE)

# -------------------------- 17.8 - Visualização Interativa --------------------
visNetwork(nodes, edges, width = "100%", height = "800px") %>%
  visEdges(smooth = FALSE, arrows = list(to = list(enabled = FALSE), from = list(enabled = FALSE))) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visPhysics(
    enabled = TRUE,
    solver = "repulsion",
    repulsion = list(
      nodeDistance = 200,
      centralGravity = 0.2,
      springLength = 200,
      springConstant = 0.02
    ),
    stabilization = list(enabled = TRUE, iterations = 200)
  ) %>%
  htmlwidgets::saveWidget(file = caminho_html)

# -------------------------- 17.9 - Exportar PNG Estático ----------------------
png(caminho_png, width = 1200, height = 900)
networkPlot(
  as_adjacency_matrix(grafo, sparse = FALSE),
  Title = ifelse(is.null(top_n_fontes),
                 "Rede de Co-citação de Fontes",
                 paste("Rede de Co-citação de Fontes – Top", top_n_fontes)),
  type = "fruchterman",
  size = TRUE,
  labelsize = 1.5,
  cluster = "louvain",
  edgesize = 3,
  remove.isolates = TRUE
)
dev.off()
