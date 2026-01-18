if(!require("devtools")) install.packages("devtools")
library("devtools")
install_github("pablobarbera/scholarnetwork")

library(scholarnetwork)

pubs <- scholar::get_publications(id = "TWc4vkkAAAAJ")
pubs <- pubs[-12,]

edges <- lapply(pubs$author, scholarnetwork:::extractAuthors)
edges <- do.call(rbind, edges)
edges <- aggregate(edges$weight, by = list(node1 = edges$node1, 
                                           node2 = edges$node2), FUN = function(x) sum(x))
names(edges)[3] <- "weight"
network <- igraph::graph.edgelist(as.matrix(edges[, c("node1", 
                                                      "node2")]), directed = FALSE)
igraph::edge_attr(network, "weight") <- edges$weight

network <- decompose(network)[[1]]

fc <- igraph::walktrap.community(network)
nodes <- data.frame(label = igraph::V(network)$name, degree = igraph::strength(network), 
                    group = fc$membership, stringsAsFactors = F)
nodes <- nodes[order(nodes$label), ]

edges <- edges[edges$node1 %in% nodes$label & edges$node2 %in% 
                   nodes$label, ]

d <- list(nodes = nodes, edges = edges)
plotNetwork(d$nodes, d$edges, file="/Users/willrogers/Downloads/portfolio/network.html", 
            fontsize = 15, width = 500, height = 500, charge = -10)
