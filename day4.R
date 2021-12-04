library(stringr)

numbers < read.delim("/Users/dbogdan/Documents/AoC_2021/input_day4_numbers.txt", sep=",", header=F)
numbers <- as.numeric(numbers[1,])
boards <- readLines("/Users/dbogdan/Documents/AoC_2021/input_day4_boards.txt")

##Question 1 -----------------------------------------------------------------------
#wrangle input into list of matrices
boards_list <- list()
b <- c()
z <- 1
for (i in seq(1,length(boards), 6)) {
  b <- boards[i]
  print(b)
  
  for(j in 1:4)
    b <- rbind(b, boards[i+j])
  print(b)
  
  boards_list[[z]] <- b
  z <- z+1
}

#convert strings to numeric
boards_list_numeric <- lapply(boards_list, function(x) { sapply(x, function(x) str_extract_all(x, boundary("word"))) |> 
    unlist() |> as.numeric() |> matrix(ncol=5) |> t() })

#find the winner board
stop=FALSE
for(no in numbers) {
  for(i in 1:length(boards_list_numeric)) {
    #find number in board
    if(no %in% boards_list_numeric[[i]] == TRUE)
      boards_list_numeric[[i]][which(boards_list_numeric[[i]]==no)] <- NA
    
    #count how many numbers we have found so far
    winner_row <- rowSums(is.na(boards_list_numeric[[i]])) 
    winner_col <- colSums(is.na(boards_list_numeric[[i]]))
    
    #if we have found any 5 in row or column, break
    if (c(5) %in% winner_col | c(5) %in% winner_row) {
      stop=TRUE
      break;
    }
  }
  if(stop == TRUE)
    break;
}

##Answer 1 -------------------------------------------------------------------------
boards_list_numeric[[i]][is.na(boards_list_numeric[[i]])] <- 0
no * sum(boards_list_numeric[[i]])


##Question 2 -----------------------------------------------------------------------
boards_list_numeric <- lapply(boards_list, function(x) { sapply(x, function(x) str_extract_all(x, boundary("word"))) |> 
    unlist() |> as.numeric() |> matrix(ncol=5) |> t() })

#find the loser board
while(length(boards_list_numeric) >0) {
  stop=FALSE
  for(no in numbers) {
    for(i in 1:length(boards_list_numeric)) {
      #find number in board
      if(no %in% boards_list_numeric[[i]] == TRUE)
        boards_list_numeric[[i]][which(boards_list_numeric[[i]]==no)] <- NA
      
      #count how many numbers we have found so far
      winner_row <- rowSums(is.na(boards_list_numeric[[i]])) 
      winner_col <- colSums(is.na(boards_list_numeric[[i]]))
      
      #if we have found any 5 in row or column, break
      if (c(5) %in% winner_col | c(5) %in% winner_row) {
        stop=TRUE
        break;
      }
    }
    if(stop == TRUE)
      break;
  }
  
  print(length(boards_list_numeric))
  print(boards_list_numeric[[i]])
  boards_list_numeric <- boards_list_numeric[-i]
  print(length(boards_list_numeric))
}

##Answer 2 -------------------------------------------------------------------------
brd <- boards_list_numeric[[1]]
brd[is.na(brd)] <- 0
numbers_left <- brd[which(brd != 0)]
number_called_to_win <- sapply(numbers_left, function(x) {which(numbers==x)})
number_called_to_win <- numbers_left[which(number_called_to_win==min(number_called_to_win))]
number_called_to_win * (sum(brd)-number_called_to_win)

