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
  find_matrix_position_try <- function(matrix_ins_try, value_try) {
     co_ords <- which(matrix_ins_try == value_try, arr.ind = T)
     co_ords_list <- purrr::map(1:(dim(co_ords)[1]),~as.integer(co_ords[.,]))
     message <- (paste0(value, " is in the bingo grid"))
     return(list(co_ords_list,value_try, message))
  }
  try(return(find_matrix_position_try(matrix_ins_try = matrix_ins, value_try = value)), silent = TRUE)
  message <- (paste0(value, " is not in the bingo grid"))
  return(list(list(),c(),message))
}

find_matrix_positions <- function(matrix_ins, values) {
  positions_list <- list()
  values_list <- c()
  message_list <- c()
  for (i in values) {
    positions_list <- c(positions_list,(find_matrix_position(matrix_ins,i))[[1]])
    values_list <- c(values_list,(find_matrix_position(matrix_ins,i))[[2]])
    message_list <- c(message_list,(find_matrix_position(matrix_ins,i))[[3]])
  }
  
  return(list(positions_list, values_list, message_list))
}

win_check <- function(matrix_input, calls, nrows = 5, ncols = 5){
  matched_positions <- (find_matrix_positions(matrix_ins = matrix_input, values = calls))[[1]]
  matched_values <- (find_matrix_positions(matrix_ins = matrix_input, values = calls))[[2]]
  unmarked_numbers_sum <- sum((matrix_input %>% as.integer())[! (matrix_input %>% as.integer()) %in% matched_values])
  
  #print((find_matrix_positions(matrix_ins = matrix_input, values = calls))[[3]])
  #row wins
  for (i in 1:nrows){
    for (j in 1:ncols){
      if (any(unlist(purrr::map(matched_positions,~ all(. == c(i,j)))))){
        if (j==nrows){
          print(paste0("row ",i," win !"))
          print(paste0("last value called was ", calls[length(calls)]))
          return(calls[length(calls)] * unmarked_numbers_sum)
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
          print(paste0("last value called was ", calls[length(calls)]))
          return(calls[length(calls)] * unmarked_numbers_sum)
        }
      } else {
        break
      }
    }
  }
  
  return("no win")
}

#test_matrix <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)

#perform call outs on by one on each bingo matrix
play_bingo <- function(call_out_input = call_outs) {
call_out_status <- c()
for (i in call_out_input) {
  call_out_status <- c(call_out_status, i)
  print(paste0("call out is ",i, ", round ",length(call_out_status)))
  win_flags <- purrr::map(1:length(bingo_matrices), ~win_check(bingo_matrices[[.]], call_out_status))
  print(win_flags)
  if (any(purrr::map(unlist(win_flags),~ ! . == "no win"))) {
    who_won <- which(unlist(purrr::map(unlist(win_flags),~ ! . == "no win")))
    print(who_won)
    print(paste0("answer: ", win_flags[[who_won]]))
    print(bingo_matrices[[who_won]])
    stop("BINGO !")
  } else {
    print("no winner yet !")
  }
}
}

##Part 2
play_bingo_to_come_last <- function(call_out_input = call_outs) {
  call_out_status <- c()
  for (i in call_out_input) {
    call_out_status <- c(call_out_status, i)
    print(paste0("call out is ",i, ", round ",length(call_out_status)))
    win_flags <- purrr::map(1:length(bingo_matrices), ~win_check(bingo_matrices[[.]], call_out_status))
    who_won <- which(unlist(purrr::map(unlist(win_flags),~ ! . == "no win")))
    if (length(who_won) == length(bingo_matrices)) {
      #who_has_not_won <- (1:length(bingo_matrices))[(which(! (1:length(bingo_matrices)) %in% who_won))]
      #print(who_has_not_won)
      return(win_flags)
      stop("BINGO LAST !")
    } else {
      print("last winner not reached yet")
      print(paste0(length(who_won), " winners so far"))
    }
  }
}




