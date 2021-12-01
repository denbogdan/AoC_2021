depth <- read.delim("/Users/dbogdan/Documents/AoC_2021/input_day1.txt", header=F)
depth <- depth$V1

#Part 1
count_increments <- function(depth) {
  inc <- 0
  for(i in 2:length(depth)) {
    if(depth[i] > depth[i-1])
      inc <- inc + 1
  } 
  return(inc)
}
count_increments(depth)

#Part 2
window <- c()
for(i in 1:(length(depth)-(length(depth) %% 3))) {
  sum <- depth[i] + depth[i+1] + depth[i+2]
  window <- c(window, sum)
}
count_increments(window)
