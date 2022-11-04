library(igraph)


pm <- matrix(NA, ncol = 15, nrow = 15)
pm[upper.tri(pm)] <- runif(sum(upper.tri(pm)))
pm[lower.tri(pm)] <- t(pm)[lower.tri(pm)]
diag(pm) <- 1
g <- sample_sbm(150, pref.matrix=pm, block.sizes=rep(10,15))
g %>%
  ggraph::ggraph(layout = 'kk') +
  ggraph::geom_edge_link()
