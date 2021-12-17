#day 15
source("utils.R")
library(igraph)

#load in cave map
cave_map <-  readr::read_csv("day15/day_15_test_input.txt", col_names = FALSE, col_types = "c")
nc <- nchar(as.character(cave_map[1,]))
nr <- nrow(cave_map)

for (i in 1:nc){
  cave_map <- cave_map %>%
    mutate(!!sym(as.character(i)) := substr(X1,i,i)) 
}
cave_map <- cave_map %>% select(-X1)

#########################################
#PART 2
#for (i in 2:5){
#  for (j in 1:nc) {
#    print(j)
#    cave_map <- cave_map %>%
#      mutate(!!(as.character(j+nc*(i-1))) := if_else((as.numeric(!!sym(as.character(j))) + 1) <= 9 , (as.numeric(!!sym(as.character(j))) + 1), (as.numeric(!!sym(as.character(j))) - 8)))
               
#  }
#}









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
  #print(index)
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

print(shortest_paths(g,from = "1", to = as.character(nc*nr))$vpath[[1]])
sp <- c(2,   3,   103,  203,  303,  403,  404,  504,  604,  704,  703,  803,  903,  904,  1004, 1104, 1204, 1304, 1404, 1405, 1505, 1605, 1705, 1805, 1806, 1906, 2006, 2106, 2107,
2108, 2208, 2308, 2408, 2508, 2509, 2510, 2511, 2611, 2711, 2811, 2911, 3011, 3012, 3013, 3113, 3213, 3214, 3215, 3216, 3217, 3218, 3318, 3418, 3518, 3519, 3619, 3620, 3621, 3721, 3722, 3723,
3823, 3923, 4023, 4123, 4124, 4125, 4126, 4226, 4326, 4327, 4427, 4527, 4528, 4628, 4629, 4630, 4730, 4731, 4831, 4931, 4932, 4933, 4934, 4935, 4936, 4937, 4938, 5038, 5138, 5139, 5239,
5339, 5439, 5539, 5540, 5640, 5740, 5840, 5841, 5941, 6041, 6141, 6142, 6143, 6243, 6343, 6443, 6543, 6544, 6644, 6744, 6844, 6944, 6945, 6946, 7046, 7047, 7048, 7148, 7149, 7150, 7151,
7251, 7252, 7253, 7254, 7255, 7355, 7356, 7456, 7457, 7557, 7657, 7757, 7857, 7957, 8057, 8058, 8158, 8159, 8160, 8260, 8360, 8460, 8560, 8660, 8760, 8860, 8960, 8961, 8962, 8963, 9063,
9064, 9164, 9264, 9265, 9365, 9465, 9565, 9566, 9567, 9568, 9668, 9669, 9769, 9770, 9771, 9772, 9773, 9774, 9775, 9776, 9777, 9778, 9878, 9879, 9880, 9881, 9981, 9982, 9983, 9984, 9985,
9885, 9785, 9786, 9787, 9687, 9587, 9588, 9589, 9489, 9490, 9491, 9492, 9493, 9494, 9495, 9496, 9596, 9696, 9697, 9797, 9897, 9898, 9899, 9999, 10000)

get_weight <- function(index){
  position_of_weight <- purrr::map(index,~find_matrix_position(positions_indexed,.))
  weight <- (unlist(purrr::map(position_of_weight, ~as.numeric(cave_map[.[1],.[2]]))))
  return(weight)
}
print(sum(unlist(purrr::map(sp,~get_weight(.)))))

