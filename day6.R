sch <- read.delim("/Users/dbogdan/portfolio/AoC_2021/input_day6.txt", header=FALSE, sep=",")
sch <- as.numeric(sch[1,])

##Question 1 -----------------------------------------------------------------------
daily_fish <- function(fish, days) {
  max_timer <- as.numeric(max(names(table(fish))))
  counter <- c(0, as.numeric(table(fish)), rep(0, 9-max_timer))
  for (i in 1:days) {
    new_fish <- counter[1] 
    counter <- c(counter[-1], counter[1])
    counter[7] <- counter[7] + new_fish
  }
  sum(counter)
}

##Answer 1 -------------------------------------------------------------------------
sch <- 1 + sch
daily_fish(sch, 80) |> print(digits = 20)


##Answer 2 -------------------------------------------------------------------------
daily_fish(sch, 256) |> print(digits = 20)


##Solutions graveyard --------------------------------------------------------------
#Where correct but useless algorithms go to die 

library(dplyr)
#Question 1 - iterative
spawn <- function(fish) {
  fish <- case_when(fish==0 ~ c(8, 6),
                    fish==1 ~ 0,
                    fish==2 ~ 1,
                    fish==3 ~ 2,
                    fish==4 ~ 3,
                    fish==5 ~ 4,
                    fish==6 ~ 5,
                    fish==7 ~ 6,
                    fish==8 ~ 7)
  return(fish)
}

for(i in 1:80) {
  print(i)
  sch <- lapply(sch, spawn) |> lapply(unique) |> unlist()
  print(length(sch))
}

length(sch)

#Question 1 - recursive
counter <<- 1
spawn <- function(sch, days=0) {
  fish <- sch[1]
  if(days<80) {
    if(fish == 0) {
      counter <<- counter + 1
      fish <- c(8,6)
      spawn(fish[1], days+1)
      spawn(fish[2], days+1)
    }
    else {
      fish <- case_when(fish==1 ~ 0,
                        fish==2 ~ 1,
                        fish==3 ~ 2,
                        fish==4 ~ 3,
                        fish==5 ~ 4,
                        fish==6 ~ 5,
                        fish==7 ~ 6,
                        fish==8 ~ 7)
      spawn(fish, days+1)
    }
  }
} 

nc <- 0
for (z in 1:length(sch))
{
  counter <- 1
  print(z)
  spawn(sch[z])
  nc <- nc + counter
}
nc