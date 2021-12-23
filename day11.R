o <- read.delim("/Users/dbogdan/projects/AoC_2021/input_day11.txt", sep="", header=FALSE, colClasses = c("character"))
o2 <- strsplit(o$V1[1], split = "") |> unlist()
for(i in 2:length(o$V1)) 
  o2 <- rbind(o2, strsplit(o$V1[i], split = "") |> unlist())
o <- matrix(as.numeric(o2), ncol=10)

##Question 1 -----------------------------------------------------------------------
directions <- list(c(1,1), c(1,0), c(1,-1), c(0,-1), c(-1,-1), c(-1,0), c(-1,1), c(0,1))

flashes <- 0
flashes_per_step <- 0
n <- 0

while(flashes_per_step != 100) {
  o <- o+1
  while(length(which(o > 9)) > 0) {
    #check each item in matrix and add points until all have fired once
    for(i in 1:nrow(o)) {
      for(j in 1:ncol(o)) {
        point <- o[i,j]
        if(!is.na(point) && point>9) {
          #find diagonals from this point
          for(k in 1:length(directions)) {
            #find direction
            xi <- directions[[k]][1]
            yi <- directions[[k]][2]
            if(i+xi > 0 & j+yi >0)
              neighbour <- tryCatch(o[i+xi, j+yi], error=function(o) {return(NA)})
            else
              neighbour <- NA
            if(!is.na(neighbour))
              o[i+xi, j+yi] <- o[i+xi, j+yi] + 1
          }
          #reset timer so that each fires once only
          o[i,j] <- NA
        }
      }
    }
  }
  
  #cumsum of flashes
  o[is.na(o)] <- 0
  flashes_per_step <- length(which(o==0))
  flashes <- flashes + flashes_per_step
  if(n==99) total <- flashes
  
  #keep track of number of steps
  n <- n+1
}

##Answer 1 -------------------------------------------------------------------------
total

##Answer 2 -------------------------------------------------------------------------
n

