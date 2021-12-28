input <- readLines("/Users/dbogdan/projects/AoC_2021/input/input_day14.txt")

#separate template from rules
temp <- input[1] |> (\(x) {strsplit(x, split = "")})()  |> unlist()
rules <- input[3:length(input)]
rules <- strsplit(rules, split = " -> ") |> unlist() |> (\(x) {t(matrix(x, nrow=2))})()


##Question 1 -----------------------------------------------------------------------
#generate all pairs
for(k in 1:10) {
  print(k)
  new_temp <- c()
  for(i in 1:(length(temp)-1)) {
    pair <- paste0(temp[i], temp[i+1])
    insert <- rules[,2][which(rules[,1] == pair)]
    new_temp <- c(new_temp, paste0(insert, temp[i+1]))
  }
  new_temp <- c(temp[1], new_temp)
  temp <- paste0(new_temp) |> (\(x) {strsplit(x, split = "")})()  |> unlist()
}

##Answer 1 -------------------------------------------------------------------------
counts <- table(temp) |> sort()
counts[length(counts)] - counts[1]
