crabs <- read.delim("/Users/dbogdan/portfolio/AoC_2021/input_day7.txt", header=FALSE, sep=",")
crabs <- as.numeric(crabs[1,])

##Question 1 -----------------------------------------------------------------------
m <- median(crabs)
fuel <- 0
for(i in 1:length(crabs))
  fuel <- fuel + abs(m-crabs[i])

##Answer 1 -------------------------------------------------------------------------
fuel

##Question 2 -----------------------------------------------------------------------

cost <- function(target, pos) {
  n <- abs(target- pos)
  cst <- n*(n+1)/2
  return(cst)
}

#R is indexed at 1 not 0
crabs <- crabs+1

crabs_costs <-c()
for(i in 1:max(crabs)) {
  sumcost <- 0
  for(j in 1:length(crabs)){
    sumcost <- sumcost + cost(i, crabs[j])
  }
  crabs_costs <- c(crabs_costs, sumcost)
}

##Answer 2 -------------------------------------------------------------------------
min(crabs_costs)

