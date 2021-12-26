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
# visit_registry <- vector(mode = "list", length = length(nb))
# names(visit_registry) <- names(nb)
visit_registry <-c("start")

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
        if(! neighbours[i] %in% visit_registry) {
          #if not visited, mark visit then visit neighbours
          visit_registry <- c(visit_registry, neighbours[i])
          print(visit_registry)
          count_paths(neighbours[i], visit_registry)
        } 
      }
    }
  } else
    #count a path once end has been reached
    path_no <<- path_no + 1
}


count_paths("start", "start")

node <- "start"
visit_registry <- "start"
path_no <- 0

while(node != "end") {
  
  neighbours <- nb[[node]]
  
  for(i in 1:length(neighbours)) {
    print(i)
    visit_registry <- c(visit_registry, neighbours[i])
    print(paste("child is", neighbours[i]))
    
    if(toupper(neighbours[i])==neighbours[i]) {
      node <- neighbours[i]
    }
    else {
      if(! neighbours[i] %in% visit_registry) {
        print(paste("node is", node))
        node <- neighbours[i]
      }
    }
  }
  path_no <- path_no + 1
}

