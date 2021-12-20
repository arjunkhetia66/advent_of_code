#day 15
source("utils.R")
library(igraph)

#load in cave map
cave_map <-  readr::read_csv("day15/day_15_input.txt", col_names = FALSE, col_types = "c")
nc <- nchar(as.character(cave_map[1,]))
nr <- nrow(cave_map)

for (i in 1:nc){
  cave_map <- cave_map %>%
    mutate(!!sym(as.character(i)) := substr(X1,i,i)) 
}
cave_map <- cave_map %>% select(-X1)

#########################################
#PART 2
for (i in 2:5){
  for (j in 1:nc) {
    cave_map <- cave_map %>%
      mutate(!!(as.character(j+nc*(i-1))) := as.character(if_else((as.numeric(!!sym(as.character(j))) + (i-1)) <= 9 , (as.numeric(!!sym(as.character(j))) + (i-1)), (as.numeric(!!sym(as.character(j))) - (10-i)))))
               
  }
}
cave_map_new <- cave_map
for (i in 2:5){
  cave_map_to_add <- cave_map
  for (j in 1:ncol(cave_map)) {
    cave_map_to_add <- cave_map_to_add %>%
      mutate(!!(as.character(j)) := as.character(if_else((as.numeric(!!sym(as.character(j))) + (i-1)) <= 9 , (as.numeric(!!sym(as.character(j))) + (i-1)), (as.numeric(!!sym(as.character(j))) - (10-i)))))
  }
  cave_map_new <- cave_map_new %>% rbind(cave_map_to_add)
}
cave_map <- cave_map_new
nc <- ncol(cave_map)
nr <- nrow(cave_map)

#########################################

#position indexes
positions_indexed <- (matrix(1:(nc*nr), nrow = nr, ncol = nc, byrow = TRUE))

#find position in matrix based on index number
find_matrix_position <- function(matrix_ins, value) {
  co_ords <- which(matrix_ins == value, arr.ind = T) %>% as.numeric()
  return(co_ords)
}

#get near by indexes and weights
near_by_indexes_and_weights <- function(index, positions_index = positions_indexed) {
  print(index)
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
  
  #position of weight
  position_of_weight <- purrr::map(index,~find_matrix_position(positions_indexed,.))
  weights <- rep(unlist(purrr::map(position_of_weight, ~as.numeric(cave_map[.[1],.[2]]))), length(nearby_indexes))
  
  return(list(nearby_indices = nearby_indexes, risks = weights))
}

#create graph
graph <- purrr::map(1:(nc*nr), ~as.character(near_by_indexes_and_weights(.)$nearby_indices))
names(graph) <- as.character((1:(nc*nr)))
#create weights
weights <-  purrr::map(1:(nc*nr), ~(near_by_indexes_and_weights(.)$risks))
names(weights) <- as.character((1:(nc*nr)))

# create edgelist with weights
G <- data.frame(stack(graph), weights = stack(weights)[[1]])
set.seed(500)
el <- as.matrix(stack(graph))
g <- graph_from_edgelist(el)
edge.attributes(g)$weight <- G$weights
print("finding shortest path")
sp <- (shortest_paths(g,from = "1", to = as.character(nc*nr))$vpath[[1]])$name

#get weights in shortest path and sum
get_weight <- function(index){
  position_of_weight <- purrr::map(index,~find_matrix_position(positions_indexed,.))
  weight <- (unlist(purrr::map(position_of_weight, ~as.numeric(cave_map[.[1],.[2]]))))
  return(weight)
}
print(sum(unlist(purrr::map(sp,~get_weight(.)))))

