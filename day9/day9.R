#day9
source("utils.R")

#load in height map
height_map <-  readr::read_csv("day9/day_9_input.txt", col_names = FALSE, col_types = "c")
nc <- nchar(as.character(height_map[1,]))
nr <- nrow(height_map)

for (i in 1:nc){
  height_map <- height_map %>%
    mutate(!!sym(as.character(i)) := substr(X1,i,i)) 
}
height_map <- height_map %>% select(-X1)

#position indexes
positions_indexed <- (matrix(1:(nc*nr), nrow = nr, ncol = nc, byrow = TRUE))

#find position in matrix based on index number
find_matrix_position <- function(matrix_ins, value) {
  co_ords <- which(matrix_ins == value, arr.ind = T) %>% as.numeric()
  return(co_ords)
}

#get near by indexes and heights
near_by_indexes_and_heights <- function(index, positions_index = positions_indexed) {
  #if (index %% 100 == 0) {print(index)}
  #index up
  index_up <- index - nc
  #index right
  index_right <- index + 1
  if (index_right%%nc == 1){
    index_right <- NULL
  }
  #index_down 
  index_down <- index + nc
  #index left
  index_left <- index - 1
  if (index_left%%nc == 0){
    index_left <- NULL
  }
  #near by indexes
  nearby_indexes <- c(index_up, index_right, index_down, index_left)
  nearby_indexes <- nearby_indexes[which(nearby_indexes > 0 & nearby_indexes <= nc*nr)]
  
  #position and values of surrouding heights
  position_of_heights_surrounding <- purrr::map(nearby_indexes,~find_matrix_position(positions_index,.))
  heights_surrounding <- purrr::map(position_of_heights_surrounding, ~as.numeric(height_map[.[1],.[2]]))
  
  #position and value of current index
  position_of_height <- purrr::map(index,~find_matrix_position(positions_index,.))
  height <- unlist(purrr::map(position_of_height, ~as.numeric(height_map[.[1],.[2]])))
  
  if (!all(((heights_surrounding %>% unlist()) > height))) {
    height <- 0
    low_point <- NA
  } else {
    height <- height + 1
    low_point <- index
  }
  
  return(list(nearby_indices = nearby_indexes,
              nearby_heights = heights_surrounding,
              low_point = low_point,
              risk = height))
}
#########################################
print(sum(unlist(purrr::map(1:(nc*nr), ~near_by_indexes_and_heights(., positions_index = positions_indexed)$risk))))
##########################################
#PART 2
#indices of low points 
low_point_indices <- unlist(purrr::map(1:(nc*nr), ~near_by_indexes_and_heights(., positions_index = positions_indexed)$low_point))
low_point_indices <- which(!is.na(low_point_indices))
#fan out from a single index
fan_out <- function(index){

  temp <- near_by_indexes_and_heights(index, positions_index = positions_indexed)
  a <- temp$nearby_indices
  b <- (temp$nearby_heights != 9 )
  return (a[b])

}
#fan out from low point
fan_out_basin <- function(low_point_index){
  
  basin <- c(low_point_index)
  basin_length_old <- length(basin)
  fanned_out <- fan_out(basin)
  #print(fanned_out)
  basin <- c(basin, fanned_out)
  basin <- unique(basin)
  basin_length_new <- length(basin)
  while (basin_length_new != basin_length_old){
     basin_length_old <- basin_length_new
  for (i in basin) {
    fanned_out <- fan_out(i)
    #print(fanned_out)
    basin <- c(basin, fanned_out)
    basin <- unique(basin)
    basin_length_new <- length(basin)
  }
  }
  
  return(basin)
}

#loop through low points, getting the size of each basin
print(prod(sort(unlist(purrr::map(low_point_indices, ~length(fan_out_basin(.)))), decreasing =  TRUE)[1:3]))

