#day 1
#conflict 1
source("utils.R") #conflict 2

#conflict 3

depths_2 <- readr::read_delim("day1/day_1_input.txt", delim = " ", col_names = FALSE) 
names(depths) <- "depth"

#part 1

depths_previous_joiner <- function(depths){
  indexed_depths <- depths %>% mutate(index = 1:nrow(depths)) 
  depths_indexed_for_join <- indexed_depths %>% mutate(join_index = index -1) %>% select(-index)
  cartesian_depths <- indexed_depths %>% left_join(depths_indexed_for_join, 
                                                   by = c("index"="join_index")) 
  
  
  
  return(cartesian_depths)
}

diffs <- depths_previous_joiner(depths) %>% mutate(diff = (depth.y - depth.x) > 0) %>% group_by(diff) %>% count()

#part 2
sums <- runner::runner(x = depths, k = 3, f = sum)
depth_3_window <- depths_previous_joiner(tibble::tibble(summed = sums)[-c(1:2),])
diffs2 <- depth_3_window %>% mutate(diff = (summed.y - summed.x) > 0) %>% group_by(diff) %>% count()
