input <- readLines("/Users/dbogdan/projects/AoC_2021/input_day13.txt")
coordinates <- input[1:840]
coordinates <- strsplit(coordinates, split = ",") |> unlist() |> as.numeric() 
coordinates <- t(matrix(coordinates, nrow=2))


