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
caminho_matriz_frequencia <- file.path(pasta_saida, "15_matriz_frequencia_autores.csv")
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
