source("read_in_hoaxy_data.R")

# Network measures

# Girvan and Newman Algorithm network
edgebetweenness <- function(edgeList){
  g <- as.undirected(graph_from_edgelist(as.matrix(edgeList)))
  eb <- edge.betweenness.community(g)
  betweennessnetwork <- plot(eb, as.undirected(g), vertex.size = 10, vertex.label = NA, layout = layout.fruchterman.reingold(g))
  return(betweennessnetwork)
}


closeness_cent <- function(edgeList){
  g <- as.undirected(graph_from_edgelist(as.matrix(edgeList)))
  close <- igraph::closeness(g)
  fine = 500 # this will adjust the resolving power
  pal = colorRampPalette(c('gray','blue', 'plum')) # color scheme
  graphCol = pal(fine)[as.numeric(cut(close,breaks = fine))]
  # now we plot it with those colors
  closenetwork <- plot(as.undirected(g), vertex.color=graphCol, vertex.size = 10, vertex.label = NA, layout = layout.fruchterman.reingold(g))
  return(closenetwork)
}

degreecentrality <- function(edgeList){
  g <- graph_from_edgelist(as.matrix(edgeList))
  degree <- degree(as.undirected(g))
  fine = 500 
  pal = colorRampPalette(c('gray','blue', 'plum')) 
  graphCol = pal(fine)[as.numeric(cut(degree,breaks = fine))]
  # now we plot it with those colors
  degreenetwork <- plot(as.undirected(g), vertex.color=graphCol, vertex.size = 10, vertex.label = NA, layout = layout.fruchterman.reingold(g))
  return (degreenetwork)
}

# Group Measures

group_degree <- function(edgeList){
  g <- graph_from_edgelist(as.matrix(edgeList))
  degree <- degree(as.undirected(g))
  max_degree <- max(degree)
  n <- vcount(g)
  for(i in 1:n){
    total =+ max_degree - degree[[i]]
  }
  return(total/((n-1)*(n-2)))
}

group_closeness <- function(edgeList){
  g <- as.undirected(graph_from_edgelist(as.matrix(edgeList)))
  close <- igraph::closeness(g)
  max_close <- max(close)
  n <- vcount(g)
  for(i in 1:n){
    total =+ max_close - close[[i]]
  }
  return(total/((n-2)*(2*n-3)))
}

group_betweenness <- function(edgeList){
  g <- as.undirected(graph_from_edgelist(as.matrix(edgeList)))
  bet <- betweenness(g)
  max_bet <- max(bet)
  n <- vcount(g)
  for(i in 1:n){
    total =+ max_bet - bet[[i]]
  }
  return(total/((n-1)*(n-2))/(2*n-3))
}

group_pagerank <- function(edgeList){
  g <- as.undirected(graph_from_edgelist(as.matrix(edgeList)))
  pr <- page.rank(g)
  max_pr <- max(pr)
  n <- vcount(g)
  for(i in 1:n){
    total =+ max_pr - pr[[i]]
  }
  return(total/page.rank(make_star(n)))
}

# analysis networks
betweennessnetwork <- edgebetweenness(edgeList)
closenessnetwork <- closeness_cent(as.matrix(edgeList))
degreenetwork <- degreecentrality(edgeList)

# group measures
group_degree(edgeList)
group_closeness(edgeList)
group_betweenness(edgeList)
group_pagerank(edgeList)