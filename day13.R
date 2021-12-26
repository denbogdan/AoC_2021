library(pheatmap)
input <- readLines("/Users/dbogdan/projects/AoC_2021/input_day13.txt")

#separate coordinates from folding instructions
coordinates <- input[1:840]
coordinates <- strsplit(coordinates, split = ",") |> unlist() |> as.numeric() 
coordinates <- t(matrix(coordinates, nrow=2))

folds <- input[842:853]
folds <- gsub("fold along ", "", folds)
folds <- strsplit(folds, split = "=") |> unlist()
folds <- t(matrix(folds, nrow=2))

#R is indexed at 1 not 0
coordinates <- coordinates+1
folds[,2] <- as.numeric(folds[,2])+1

#create map
map <- matrix(0, nrow=max(coordinates[,1]), ncol=max(coordinates[,2]))
for(i in 1:nrow(coordinates)) {
  x=coordinates[i,1]
  y=coordinates[i,2]
  map[x,y] <- 1
}
map <- t(map)


##Question 1 -----------------------------------------------------------------------
fold_coordinate <- as.numeric(folds[1,2])

up <- map[1:fold_coordinate,]
down <- map[(fold_coordinate+1):nrow(map),]


map_folded <- up
for(i in 1:(nrow(down))) {
  for(j in 1:ncol(down)) {
    if(down[i,j] == 1 | up[fold_coordinate-i,j] == 1) {
      map_folded[fold_coordinate-i,j] <- 1
    }
  }
}

##Answer 1 -------------------------------------------------------------------------
table(map_folded)


##Question 2 -----------------------------------------------------------------------

for(k in 1:nrow(folds)) {
  fold_direction <- folds[k,1]
  fold_coordinate <- as.numeric(folds[k,2])
  
  if(fold_direction == "y") {
    up <- map[1:fold_coordinate,]
    down <- map[(fold_coordinate+1):nrow(map),]
    map_folded <- up
    
    #add new dots
    for(i in 1:(nrow(down))) {
      for(j in 1:ncol(down)) {
        if(down[i,j] == 1 | up[fold_coordinate-i,j] == 1) {
          map_folded[fold_coordinate-i,j] <- 1
        }
      }
    }
    map_folded <- map_folded[1:(nrow(map_folded)-1),]
  } else {
    left <- map[,1:fold_coordinate]
    right <- map[,(fold_coordinate+1):ncol(map)]
    map_folded <- left
    
    #find the shortest side
    n <- min(ncol(left), ncol(right))
    #add new dots
    for(i in 1:n) {
      for(j in 1:nrow(left)) {
        if(right[j,i] == 1 | left[j,fold_coordinate-i] == 1) {
          map_folded[j,fold_coordinate-i] <- 1
        }
      }
    }
    map_folded <- map_folded[,1:(ncol(map_folded)-1)]
  }
  
  map <- map_folded
}

##Answer 2 -------------------------------------------------------------------------
pheatmap(map, cluster_rows = F, cluster_cols = F)

