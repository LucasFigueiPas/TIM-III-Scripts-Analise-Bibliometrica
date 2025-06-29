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
