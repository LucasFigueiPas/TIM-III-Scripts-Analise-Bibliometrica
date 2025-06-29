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
