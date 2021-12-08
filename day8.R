library(dplyr)

d <- read.delim("/Users/dbogdan/portfolio/AoC_2021/input_day8.txt", header=FALSE, sep=" ")
d1 <- as.matrix(d[,c(12:15)])
d2 <- as.matrix(d[,c(1:10)])

##Question 1 -----------------------------------------------------------------------
counter <- c()
for(i in 1:length(d1)) {
  chr <- strsplit(d1[i], split="") |> unlist()
  counter <- c(counter, case_when(length(unique(chr)) == 2 ~ 1,
                                  length(unique(chr)) == 4 ~ 4,
                                  length(unique(chr)) == 3 ~ 7,
                                  length(unique(chr)) == 7 ~ 8))
}

##Answer 1 -------------------------------------------------------------------------
counter[!is.na(counter)] |> length()


##Question 2 -----------------------------------------------------------------------
calc <- function(t) {
  out <- list()
  
  #find pos[1]
  seven <- strsplit(t[which(nchar(t)==3)], split="") |> unlist()
  one <- strsplit(t[which(nchar(t)==2)], split="") |> unlist()
  out[[1]] <- setdiff(seven, one) 
  
  #find pos[4]
  four <- strsplit(t[which(nchar(t)==4)], split="") |> unlist()
  nine_dummy <- strsplit(t[which(nchar(t)==6)], split="")
  tsplit <- sapply(t, function(x) strsplit(x, split=""))
  for(l in 1:length(nine_dummy)) {
    if(length(setdiff(nine_dummy[[l]], unique(c(four, out[[1]])))) == 1) 
      nine <- nine_dummy[[l]]
  }
  out[[4]] <- setdiff(nine, c(out[[1]], four))
  
  #find pos[5]
  eight <- strsplit(t[which(nchar(t)==7)], split="") |> unlist()
  out[[5]] <- setdiff(eight, nine )
  
  #find pos[7]
  for (i in 1:length(tsplit)) {
    if(length(setdiff(unlist(tsplit[i]), c(out[[1]], out[[4]], one))) == 1) {
      three <- tsplit[i] |> unlist()
    }
  }
  
  out[[7]] <- setdiff(three, c(out[[1]], out[[4]], one) )
  
  #find pos[6]
  out[[6]] <- setdiff(eight, c(out[[1]], out[[4]], out[[7]], out[[5]], one))
  
  #find pos[2]
  for (i in 1:length(tsplit)) {
    if(length(unlist(tsplit[i])) == 5) {
      if(length(setdiff(unlist(tsplit[i]), c(out[[1]], out[[7]], out[[5]], out[[4]])))==1)
        out[[2]] <- setdiff(unlist(tsplit[i]), c(out[[1]], out[[7]], out[[5]], out[[4]]))
    }
  }
  
  #find pos[3]
  out[[3]] <- setdiff(one, out[[2]])
  
  #unlist the out vector for further processing
  outvect <- unlist(out)
  
  return(outvect)
}

numbers_decoded <- rep(0,4)
for(m in 1:nrow(d2)) {
  #select row
  t <- d2[m,]
  
  #calculate outvect
  outvect <- calc(t)
  
  #decode each digit
  numberv <- c()
  for(k in 1: ncol(d1)) {
    x = strsplit(d1[m,k], split="") |> unlist() |> sort()
    number <- case_when(setequal(sort(outvect[1:6]), x) ~ 0,
                        setequal(sort(outvect[2:3]), x) ~ 1,
                        setequal(sort(outvect[c(1,2,7,5,4)]), x) ~ 2,
                        setequal(sort(outvect[c(1:4, 7)]), x) ~ 3,
                        setequal(sort(outvect[c(6,7, 2:3)]), x) ~ 4,
                        setequal(sort(outvect[c(1,6,7,3,4)]), x) ~ 5,
                        setequal(sort(outvect[c(1,6,7,3,4,5)]), x) ~ 6,
                        setequal(sort(outvect[1:3]), x) ~ 7,
                        setequal(sort(outvect[c(1:7)]), x) ~ 8,
                        setequal(sort(outvect[c(6,7, 1:4)]), x) ~ 9)
    numberv <- c(numberv, number)
  }
  #add decoded digots to df
  numbers_decoded <- rbind(numbers_decoded, numberv)
}

##Answer 2 -------------------------------------------------------------------------
apply(numbers_decoded, MARGIN=1, function(x) {paste(as.character(x), collapse="") |> as.numeric()}) |> sum()


##Nota Bene ------------------------------------------------------------------------

#positions on the seven segment display
#     1
#   -----
#  |     |  
# 6|     | 2
#  |  7  | 
#   -----
#  |     | 
# 5|     | 3
#  |     |  
#   -----
#     4
# 