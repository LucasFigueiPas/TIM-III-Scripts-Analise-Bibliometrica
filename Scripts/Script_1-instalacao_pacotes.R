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
