map <- read.delim("/Users/dbogdan/Documents/AoC_2021/input_day2.txt", header=F, sep=" ")
colnames(map) <- c("dir", "inc")

##Question 1 -----------------------------------------------------------------------
x <- 0
y <- 0

for(i in 1:nrow(map)) {
  if(map$dir[i] == "forward") {
    x <- x + map$inc[i] 
  } else {
  if(map$dir[i] == "up")
    y <- y - map$inc[i]
  else
    y <- y + map$inc[i]
  }
}

##Answer 1 -------------------------------------------------------------------------
x*y


##Question 2 -----------------------------------------------------------------------
aim <- 0
x <- 0
y <- 0

for(i in 1:nrow(map)) {
  if(map$dir[i] == "down") {
    aim <- aim + map$inc[i] 
  } else {
    if(map$dir[i] == "up")
      aim <- aim - map$inc[i]
    else{
      x <- x + map$inc[i]
      if (aim != 0)
        y <- y + (map$inc[i]*aim)
    }
  }
}

##Answer 2 -------------------------------------------------------------------------
x*y