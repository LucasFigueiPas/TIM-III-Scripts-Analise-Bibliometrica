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
pasta_saida <- criar_pasta_resultado("14-Rede colaboracao instituicoes leve")
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
