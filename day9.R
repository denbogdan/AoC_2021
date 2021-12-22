mat <- read.delim("/Users/dbogdan/projects/AoC_2021/input_day9.txt", sep="", header=FALSE, colClasses = c("character"))
mat2 <- strsplit(mat$V1[1], split = "") |> unlist()
for(i in 2:length(mat$V1)) {
  mat2 <- rbind(mat2, strsplit(mat$V1[i], split = "") |> unlist())
}
mat <- t(matrix(as.numeric(mat2), ncol=100))


##Question 1 -----------------------------------------------------------------------

#middle
m <- nrow(mat)
n <- ncol(mat)
min_values <- c()
coordinates <- c(0,0)
for(i in 2:(m-1)) {
  for(j in 2:(n-1)) {
    x <- mat[i,j]
    nvals <- c(x, mat[i+1,j], mat[i-1,j], mat[i,j+1], mat[i,j-1])
    if(x==min(nvals)) {
      min_values <- c(min_values, x)
      coordinates <- rbind(coordinates, c(i, j))
    }
  }
}

#sides
top <- mat[1:2,]
top <- cbind(c(top[1,1]), top, c(top[1,n]))
i=1
for(j in 2:(ncol(top)-1)) {
  x <- top[i,j]
  nvals <- c(x, top[i+1,j], top[i,j+1], top[i,j-1])
  if(x==min(nvals)){
    min_values <- c(min_values, x)
    coordinates <- rbind(coordinates, c(1, j-1))
  }
}


bottom <- mat[c(m-1, m),]
bottom <- cbind(bottom[2,1], bottom, bottom[2,n])
i=2
for(j in 2:(ncol(bottom)-1)) {
  x <- bottom[i,j]
  nvals <- c(x, bottom[i-1,j], bottom[i,j+1], bottom[i,j-1])
  if(x==min(nvals)){
    min_values <- c(min_values, x)
    coordinates <- rbind(coordinates, c(m, j-1))
  }
}


left <- mat[,1:2]
left <- rbind(left[1,1], left, left[m,1])
j=1
for(i in 3:(nrow(left)-2)) {
  x <- left[i,j]
  nvals <- c(x, left[i+1,j], left[i,j+1], left[i-1,j])
  if(x==min(nvals)){
    min_values <- c(min_values, x)
    coordinates <- rbind(coordinates, c(i-1, 1))
  }
}

right <- mat[,c(n-1,n)]
right <- rbind(right[1,2], right, right[m,2])
j=2
for(i in 3:(nrow(right)-2)) {
  x <- right[i,j]
  nvals <- c(x, right[i+1,j], right[i,j-1], right[i-1,j])
  if(x==min(nvals)){
    min_values <- c(min_values, x)
    coordinates <- rbind(coordinates, c(i-1, n))
  }
}

coordinates <- as.data.frame(coordinates)
coordinates <- coordinates[2:nrow(coordinates),]
if(9 %in% min_values)
  coordinates <- coordinates[-which(min_values==9),]

##Answer 1 -------------------------------------------------------------------------
if(9 %in% min_values)
  min_values <- min_values[-which(min_values==9)]
sum(min_values+1)



##Question 2 -----------------------------------------------------------------------

#prepare matrix
matbordered <- rbind(c(rep(9, 100)), mat, c(rep(9,100)))
matbordered <- cbind(c(rep(9, 102)), matbordered, c(rep(9,102)))
mat <- matbordered

#replace all 9s with NAs and store a copy of the matrix
mat[which(mat==9)] <- NA
matinit=mat

#to account for the borders
coordinates <- coordinates+1

#rescursion
adjacent <- function(x,y,basin,mat) {
  directions <- list(c(1,1), c(1,0), c(1,-1), c(0,-1), c(-1,-1), c(-1,0), c(-1,1), c(0,1))
  for(i in 1:length(directions)) {
    #find direction
    xi <- directions[[i]][1]
    yi <- directions[[i]][2]
    #consider diagonals separate from other directions
    #scan to see if we have hit a border 
    can9 <- c(mat[x+xi,y], mat[x,y+yi])
    if(is.na(can9[1]) && is.na(can9[2])) {
      #if hit border, add value to basin and move on to next direction
      mat[x,y] <<- NA
    } else {
      #find the next point in this direction
      point <- mat[x,y]
      n <- mat[x+sign(xi),y+sign(yi)]
      #if point has not been visited add to basin
      if(!is.na(n)) {
        if(!is.na(point) && n>point) {
          mat[x,y] <<- NA
          basin <<- c(basin, point)
          adjacent(x+sign(xi), y+sign(yi), basin, mat)
        } else {
          if(!is.na(point) && (n==point || n!=9)) {
            mat[x,y] <<- NA   
            mat[x+sign(xi), y+sign(yi)] <<- NA
          }
        } 
      } else {
        mat[x,y] <<- NA
      }
    }
  }
}

#for each minimum point, find basin
bsns <- c()
for(z in 1:nrow(coordinates)) {
  x <- coordinates[z,1]
  y <- coordinates[z,2]
  basin=c()
  adjacent(x,y,basin,mat)
  xbasins = table(as.numeric(matinit) == as.numeric(matinit)) - table(as.numeric(mat) == as.numeric(matinit))
  bsns <- c(bsns, xbasins)
  matinit <- mat
}


##Answer 2 -----------------------------------------------------------------------
prod(sort(bsns, decreasing = TRUE)[1:3])


