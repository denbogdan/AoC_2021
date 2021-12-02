map <- read.delim("/Users/dbogdan/Documents/AoC_2021/input_day2.txt", header=F, sep=" ")
colnames(map) <- c("dir", "inc")

#Part 1
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

x*y


#Part 2

aim <- 0
x <- 0
y <- 0

for(i in 1:nrow(map)) {
  if(map$dir[i] == "down") {
    aim <- aim + map$inc[i] 
  } else {
    if(map$dir[i] == "up")
      aim <- aim - map$inc[i]
    else
      x <- x + map$inc[i]
      if (aim != 0)
        y <- y + (map$inc[i]*aim)
  }
}

x*y

dummy <- data.frame(dir=c("forward", "down", "forward", "up", "down", "forward"), inc=c(5,5,8,3,8,2))

for(i in 1:nrow(dummy)) {
  if(dummy$dir[i] == "down") {
    aim <- aim + dummy$inc[i] 
  } else {
    if(dummy$dir[i] == "up")
      aim <- aim - dummy$inc[i]
    else{
      x <- x + dummy$inc[i]
      if (aim != 0)
        y <- y + (dummy$inc[i]*aim)
    }
  }
}
