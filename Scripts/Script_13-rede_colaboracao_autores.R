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
