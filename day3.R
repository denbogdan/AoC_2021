start_time <- Sys.time()

library(stringr)
rep <- read.delim("/Users/dbogdan/Documents/AoC_2021/input_day3.txt", header=F, colClasses = c("factor"))

##Question 1 -----------------------------------------------------------------------
n <- nrow(rep)
rep <- sapply(rep$V1, function(x) str_extract_all(x, boundary("character"))) |> 
  unlist() |> as.numeric() |> matrix(ncol=n) |> t()

#gamma
gamma <- c()
for(i in 1:ncol(rep)) {
  if(sum(rep[,i]) > (n-sum(rep[,i])))
    bit <- 1
  else
    bit <- 0
  gamma <- c(gamma, bit)
}

#epsilon
epsilon <- ifelse(gamma == 0, 1, 0)

##Answer 1 -------------------------------------------------------------------------
gamma_dec <- strtoi(paste0(as.character(gamma), collapse=""), base = 2)
epsilon_dec <- strtoi(paste0(as.character(epsilon), collapse=""), base = 2)
gamma_dec*epsilon_dec



##Question 2 -----------------------------------------------------------------------
oxygen <- rep
i <- 1
while(nrow(oxygen) > 1) {
  #update n
  n <- nrow(oxygen)
  
  #find most common digit
  if(sum(oxygen[,i]) >= (n-sum(oxygen[,i])))
    bit <- 1
  else
    bit <- 0
  
  #keep only rows that match the bit
  oxygen <- oxygen[which(oxygen[,i] == bit),]
  
  #increment 
  i <- i+1
}

CO2 <- rep
i <- 1
while(nrow(CO2) > 1) {
  #update n
  n <- nrow(CO2)
  
  #find most common digit
  if(sum(CO2[,i]) >= (n-sum(CO2[,i])))
    bit <- 0
  else
    bit <- 1
  
  #keep only rows that match the bit
  CO2 <- CO2[which(CO2[,i] == bit),]
  
  #increment 
  i <- i+1
}


##Answer 2 -------------------------------------------------------------------------
oxygen_dec <- strtoi(paste0(as.character(oxygen), collapse=""), base = 2)
CO2_dec <- strtoi(paste0(as.character(CO2), collapse=""), base = 2)
oxygen_dec*CO2_dec

end_time <- Sys.time()
end_time - start_time
