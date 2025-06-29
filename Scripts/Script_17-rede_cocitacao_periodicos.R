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
