library(tidyr)
library(dplyr)

#wrangle input
lines <- read.delim("/Users/dbogdan/portfolio/AoC_2021/input_day5.txt", header=F, sep=",")
lines <- separate(lines, col = "V2", into = c("y1", "x2"), sep=" -> ") %>% 
  rename(x1 = V1) %>% rename(y2 = V3) %>% mutate_at(vars(y1, x2), list(as.numeric))

##Question 1 -----------------------------------------------------------------------
#determine dimensions of the field
x_range <- range(c(lines$x1, lines$x2))
y_range <- range(c(lines$y1, lines$y2))

#make data matrix with 0s to add lines to
#note input data ranges 0 to n vs R indeces 1 to n+1
#need to add +1 to match the input coordinates to R indeces
field <- matrix(0, y_range[2]+1, x_range[2]+1)

#make function that checks line type
line_type <- function(l) {
  type <- case_when(l[1] == l[3] ~ "vertical",
            l[2] == l[4] ~ "horizontal",
            abs(l[1]-l[3]) == abs(l[2]-l[4]) ~ "diagonal")
  return(type)
}

#add lines to matrix
for(i in 1:nrow(lines)) {
  #select line
  l <- lines[i,] + 1
  #add points to matrix
  if(line_type(l) == "vertical") {
    r <- range(l[2],l[4])
    for(j in r[1]:r[2])
      field[as.numeric(l[1]), j] <- field[as.numeric(l[1]), j] + 1
  } else {
    if(line_type(l) == "horizontal") {
      r <- range(l[1],l[3]) 
      for(j in r[1]:r[2]) 
        field[j, as.numeric(l[2])] <- field[j, as.numeric(l[2])] + 1
    } 
  }
}

##Answer 1 -------------------------------------------------------------------------
length(which(field>=2))



##Question 2 -----------------------------------------------------------------------
#make data matrix with 0s to add lines to
field <- matrix(0, y_range[2]+1, x_range[2]+1)

#add lines to matrix
for(i in 1:nrow(lines)) {
  #select line
  l <- lines[i,] + 1
  #add points to matrix
  if(line_type(l) == "vertical") {
    r <- range(l[2],l[4])
    for(j in r[1]:r[2])
      field[as.numeric(l[1]), j] <- field[as.numeric(l[1]), j] + 1
  } else {
    if(line_type(l) == "horizontal") {
      r <- range(l[1],l[3]) 
      for(j in r[1]:r[2]) 
        field[j, as.numeric(l[2])] <- field[j, as.numeric(l[2])] + 1
    } 
    else {
      if(line_type(l) == "diagonal") {
        x1 <- as.numeric(l[1])
        x2 <- as.numeric(l[3])
        y1 <- as.numeric(l[2])
        y2 <- as.numeric(l[4])
        for(j in 0:as.numeric(abs(l[1]-l[3]))) {
          if(x1-x2 < 0 && y1-y2 < 0)
            field[x1+j, y1+j] <- field[x1+j, y1+j] + 1
          else {
            if(x1-x2 < 0 && y1-y2 > 0)
              field[x1+j, y1-j] <- field[x1+j, y1-j] + 1
              else {
                if(x1-x2 > 0 && y1-y2 > 0)
                  field[x1-j, y1-j] <- field[x1-j, y1-j] + 1
                    else
                  field[x1-j, y1+j] <- field[x1-j, y1+j] + 1
              }
          }
        }
      }
    }
  }
}

##Answer 2 -------------------------------------------------------------------------
length(which(field>=2))







