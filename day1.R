depth <- read.delim("/Users/dbogdan/Documents/Advent_of_Code/input_day1.txt", header=F)
depth <- depth$V1

#Part 1
inc <- 0
for(i in 2:length(depth)) {
  if(depth[i] > depth[i-1])
    inc <- inc + 1
}

#Part 2
window <- c()
for(i in 1:(length(depth)-(length(depth) %% 3))) {
  sum <- depth[i] + depth[i+1] + depth[i+2]
  window <- c(window, sum)
}

inc <- 0
for(j in 2:length(window)) {
  if(window[j] > window[j-1])
    inc <- inc + 1
}
