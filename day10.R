library(rstack)
library(dplyr)

lines <- read.delim("/Users/dbogdan/projects/AoC_2021/input_day10.txt", sep="", header=FALSE, colClasses = c("character"))
lines <- sapply(lines$V1, function(x) { strsplit(x, split = "") |> unlist() })

##Question 1 -----------------------------------------------------------------------
#brackets 
brackets <- list(open=c("{", "[", "(", "<"),
                 closed=c("}", "]", ")", ">"))
brackets_values <- data.frame(illegal=brackets$closed,
                              points=c(1197, 57, 3, 25137))
#create a stack
stck <- stack$new()
illegal <- c()
illegal_lines <-c()

for(k in 1:length(lines)) {
  l <- lines[[k]]
  for(i in 1:length(l)) {
    if(l[i] %in% brackets$open)
    {
      stck$push(l[i])
    } else {
      last <- stck$pop()
      last_pair <- brackets$closed[which(brackets$open == last)]
      if(l[i] != last_pair) {
        illegal <- c(illegal, l[i])
        illegal_lines <- c(illegal_lines, k)
        break;
      }
    }
  }
}

##Answer 1 -------------------------------------------------------------------------
left_join(brackets_values, table(illegal) |> as.data.frame(), by="illegal") %>% 
  mutate(points_total=Freq*points) %>% summarise(Total=sum(points_total))


##Question 2 -----------------------------------------------------------------------
#discard corrupted lines
lines <- lines[-illegal_lines]

all_scores <- c()
for(k in 1:length(lines)) {
  #calculate stack
  stck <- stack$new()
  l <- lines[[k]]
  for(i in 1:length(l)) {
    if(l[i] %in% brackets$open) {
      stck$push(l[i])
    } else {
      stck$pop()
    }
  }
  #unstack and score
  score <- 0
  while(!stck$is_empty()) {
    last <- stck$pop()
    inc <- case_when(last == "(" ~ 1,
                     last == "[" ~ 2,
                     last == "{" ~ 3,
                     last == "<" ~ 4)
    score <- (score*5) + inc
  }
  all_scores <- c(all_scores, score)
}

##Answer 2 -------------------------------------------------------------------------
median(all_scores)
