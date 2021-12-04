#day 4
library(tidyverse)

#call outs
call_outs <- c(90,4,2,96,46,1,62,97,3,52,7,35,50,28,31,37,74,26,59,53,82,
               47,83,80,19,40,68,95,34,55,54,73,12,78,30,63,57,93,72,77,56,91,23,67,64,
               79,85,84,76,10,58,0,29,13,94,20,32,25,11,38,89,21,98,92,42,27,14,99,24,75,
               86,51,22,48,9,33,49,18,70,8,87,61,39,16,66,71,5,69,15,43,88,45,6,81,60,36,44,17,41,65)
col_names <- c("1","2","3","4","5")
bingo_cards <- readr::read_delim("day4/day_4_input.txt", delim = " ", col_names = FALSE) 
names(bingo_cards) <- col_names
bingo_cards_indexed <- bingo_cards %>%
  #make index
  mutate(index = rep(1:100, each=5)) %>%
  #get rid of spaces
  mutate_at(col_names, ~as.integer(gsub(" ","",.)))

bingo_cards_split <- split(bingo_cards_indexed, bingo_cards_indexed[["index"]])
bingo_matrices <- list()
for (i in 1:length(bingo_cards_split)){
  bingo_matrices[[i]] <- (as.matrix(bingo_cards_split[[i]] %>% select(-index)))
}

find_matrix_position <- function(matrix_ins, value){
  tryCatch(expr = {
  co_ords <- which(matrix_ins == value, arr.ind = T)
  co_ords_list <- purrr::map(1:(dim(co_ords)[1]),~as.integer(co_ords[.,]))
  return(co_ords_list)
  }, 
  finally = {print(paste0(value," is not in bingo matrix" )}
  )
}

find_matrix_positions <- function(matrix_ins, values) {
  positions_list <- list()
  for (i in values) {
    positions_list <- c(positions_list,find_matrix_position(matrix_ins,i))
  }
  
  return(positions_list)
}

win_check <- function(matrix_input, call, nrows = 5, ncols = 5){
  matched_positions <- find_matrix_positions(matrix_ins = matrix_input, values = calls)
  
  #row wins
  for (i in 1:nrows){
    for (j in 1:ncols){
      if (any(unlist(purrr::map(matched_positions,~ all(. == c(i,j)))))){
        matched_calls <- c(matched_calls)
        if (j==nrows){
          print(paste0("row ",i," win !"))
          return(i)
        }
      } else {
        break
      }
      }
    }
  
  #column wins
  for (i in 1:ncols){
    for (j in 1:nrows){
      if (any(unlist(purrr::map(matched_positions,~ all(. == c(j,i)))))){
        if (j==ncols){
          print(paste0("column ",i," win !"))
          return(i)
        }
      } else {
        break
      }
    }
  }
}

test_matrix <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)
