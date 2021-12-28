#Dijkstra's algorithm
library(dplyr)
floor <- new_floor
#represent map as a graph with vertices and edges
#vertices
vertices <- c()
#edges
to <- c()
from <- c()
weight <- c()
for(i in 1:(nrow(floor))) {
  for(j in 1:(ncol(floor))) {
    #current node
    v <- paste0(i, ",", j)
    vertices <- c(vertices, v)
    print(length(vertices))
    #neighbours
    n1 <- paste0(i+1, ",", j)
    w1 <- tryCatch(floor[i+1, j], error=function(o) {return(NA)})
    n2 <- paste0(i, ",", j+1)
    w2 <- tryCatch(floor[i, j+1], error=function(o) {return(NA)})
    
    from <- c(from, v, v)
    to <- c(to, n1, n2)
    weight <- c(weight, w1, w2)
  }
}

save(edges, file="edges_100.RData")
save(vertices, file="vertices_100.RData")

edges <- data.frame(from=from, to=to, weight=weight)
edges <- edges %>% filter(!is.na(weight))



start_time <- Sys.time()
#keep tally of the paths built and nodes visited
#initialize all path lengths to Inf and all the visits to 0
tally <- data.frame(shortest_path=rep(Inf, length(vertices)), prev_v=rep(NA,length(vertices)),
                    row.names = vertices, visited=rep(0, length(vertices)))
tally[1,1] <- 0

#algorithm
v <- "1,1"
while(!is.na(.subset2(tally,3)[250000])) {
  #we are only interested in n1 and n2 (down and to the right)
  n1 <- .subset2(edges,2)[which(edges$from==v)[1]]
  n1_weight <- .subset2(edges,3)[which(edges$from==v)[1]]
  n2 <- .subset2(edges,2)[which(edges$from==v)[2]]
  n2_weight <- .subset2(edges,3)[which(edges$from==v)[2]]
  
  path_1 <- tally[v, "shortest_path"] + n1_weight
  if(path_1 < tally[n1, "shortest_path"]) {
    tally[n1, "shortest_path"] <- path_1
  }
  path_2 <- tally[v, "shortest_path"] + n2_weight
  if(path_2 < tally[n2, "shortest_path"]) {
    tally[n2, "shortest_path"] <- path_2
  }
  
  tally[v, "visited"] <- NA
  
  #next visit node
  v <- rownames(tally[which(!is.na(tally$visited)),])[which(tally[which(!is.na(tally$visited)),]$shortest_path == min(tally[which(!is.na(tally$visited)),]$shortest_path))][1]

  print(length(which(is.na(tally$visited))))
}

tail(tally)

end_time <- Sys.time()
end_time-start_time
