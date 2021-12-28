library(dplyr)

floor <- read.delim("/Users/dbogdan/projects/AoC_2021/input/input_day15.txt", sep="", header=FALSE, colClasses = c("character"))
floor2 <- strsplit(floor$V1[1], split = "") |> unlist()
for(i in 2:length(floor$V1)) {
  floor2 <- rbind(floor2, strsplit(floor$V1[i], split = "") |> unlist())
}
floor <- matrix(as.numeric(floor2), ncol=100)

##Question 1 -----------------------------------------------------------------------

#Dijkstra's algorithm

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
    #neighbours
    n1 <- paste0(i+1, ",", j)
    w1 <- tryCatch(floor[i+1, j], error=function(o) {return(NA)})
    n2 <- paste0(i, ",", j+1)
    w2 <- tryCatch(floor[i, j+1], error=function(o) {return(NA)})
    n3 <- paste0(i, ",", j-1)
    w3 <- tryCatch(floor[i, j-1], error=function(o) {return(NA)})
    if(length(w3) == 0) w3 <- NA
    n4 <- paste0(i-1, ",", j)
    w4 <- tryCatch(floor[i-1, j], error=function(o) {return(NA)})
    if(length(w4) == 0) w4 <- NA
    
    from <- c(from, v, v, v, v)
    to <- c(to, n1, n2, n3, n4)
    weight <- c(weight, w1, w2, w3, w4)
  }
}

edges <- data.frame(from=from, to=to, weight=weight)
edges <- edges %>% filter(!is.na(weight))

#keep tally of the paths built and nodes visited
#initialize all path lengths to Inf and all the visits to 0
tally <- data.frame(shortest_path=rep(Inf, length(vertices)), prev_v=rep(NA,length(vertices)),
                    row.names = vertices, visited=rep(0, length(vertices)))
tally[1,1] <- 0

#algorithm
v <- "1,1"
while(length(which(!is.na(tally$visited))) > 0) {
  #we are only interested in n1 and n2 (down and to the right)
  n1 <- edges$to[which(edges$from==v)[1]]
  n1_weight <-  edges$weight[which(edges$from==v)[1]]
  n2 <- edges$to[which(edges$from==v)[2]]
  n2_weight <-  edges$weight[which(edges$from==v)[2]]
  path_1 <- tally[v, "shortest_path"] + n1_weight
  if(path_1 < tally[n1, "shortest_path"]) {
    tally[n1, "shortest_path"] <- path_1
    tally[n1, "prev_v"] <- v
  }
  path_2 <- tally[v, "shortest_path"] + n2_weight
  if(path_2 < tally[n2, "shortest_path"]) {
    tally[n2, "shortest_path"] <- path_2
    tally[n2, "prev_v"] <- v
  }
  
  tally[v, "visited"] <- NA
  
  #next visit node
  v <- rownames(tally[which(!is.na(tally$visited)),])[which(tally[which(!is.na(tally$visited)),]$shortest_path == min(tally[which(!is.na(tally$visited)),]$shortest_path))][1]
}

##Answer 1 -------------------------------------------------------------------------
tally["100,100", "shortest_path"]


##Question 2 -----------------------------------------------------------------------
#generate new tiles
tiles_list <- list()
tile <- floor
for(i in 1:8) {
  tile_new <- tile+1
  tile_new[which(tile_new > 9)] <- 1
  tiles_list[[i]] <- tile_new
  tile <- tile_new
}

row_1 <- cbind(floor, tiles_list[[1]], tiles_list[[2]], tiles_list[[3]], tiles_list[[4]])
row_2 <- cbind(tiles_list[[1]], tiles_list[[2]], tiles_list[[3]], tiles_list[[4]], tiles_list[[5]])
row_3 <- cbind(tiles_list[[2]], tiles_list[[3]], tiles_list[[4]], tiles_list[[5]], tiles_list[[6]])
row_4 <- cbind(tiles_list[[3]], tiles_list[[4]], tiles_list[[5]], tiles_list[[6]], tiles_list[[7]])
row_5 <- cbind(tiles_list[[4]], tiles_list[[5]], tiles_list[[6]], tiles_list[[7]], tiles_list[[8]])

new_floor <- rbind(row_1, row_2, row_3, row_4, row_5)

##Solutions graveyard --------------------------------------------------------------
#Where correct but useless algorithms go to die 

### DOES NOT SCALE 
### WORKS ONLY ON DUMMY (SMALL INPUT)
#recursively add all the paths - Question 1
to_path <- function(x,y, sum) {
  if((x <= nrow(floor)) & (y <= ncol(floor))) {
    #add to sum
    sum <- sum+floor[x,y]
    #generate next positions
    to_path(x, y+1, sum)
    to_path(x+1, y, sum)
  } else {
    #print(paste(x, y))
    if((x >= nrow(floor)) & (y >= ncol(floor)))
      print(sum)
    #sumlist <<- c(sumlist, sum)
  }
}

sumlist <- c()
to_path(1,1,0)

min(sumlist)-floor[1,1]

