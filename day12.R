###NOT A WORKING SOLUTION

gr <- read.delim("/Users/dbogdan/projects/AoC_2021/dummy_day12.txt", header=FALSE, sep="-")
#find neighbours of each node
nb <- list()
for(node in unique(c(gr$V1, gr$V2)))
{
  pairs <- gr[union(which(gr$V1 == node), which(gr$V2 == node)),]
  pairs <- setdiff(unique(c(pairs$V1, pairs$V2)), node)
  nb[[node]] <- pairs
}

#count paths found
path_no <- 0
#mark visits
visit_registry <- vector(mode = "list", length = length(nb))
names(visit_registry) <- names(nb)

#recursion
count_paths <- function(node, visit_registry) {
  print(paste("parent is", node))
  if(node != "end") {
    neighbours <- nb[[node]]
    for(i in 1:length(neighbours)) {
      print(paste("child is", neighbours[i]))
      if(toupper(neighbours[i])==neighbours[i])
        #if uppercase, can visit neighbours
        count_paths(neighbours[i], visit_registry)
      else {
        #if lowercase, check if it has been visited
        if(is.null(visit_registry[[neighbours[i]]])) {
          #if not visited, mark visit then visit neighbours
          visit_registry[[neighbours[i]]] <- "visited"
          count_paths(neighbours[i], visit_registry)
        } 
      }
    }
  } else
    #count a path once end has been reached
    path_no <<- path_no + 1
}


count_paths("start", visit_registry)


