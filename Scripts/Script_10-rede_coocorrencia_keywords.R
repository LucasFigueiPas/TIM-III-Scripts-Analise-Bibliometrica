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
