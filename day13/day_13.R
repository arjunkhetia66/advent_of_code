#day 13
source("utils.R")

dot_coords <- readr::read_delim("day13/day_13_input.txt", delim = ",", col_names = FALSE) 
names(dot_coords) <- c("x", "y")

max_y <- as.numeric(colMax(dot_coords %>% select(y)))
max_x <- as.numeric(colMax(dot_coords %>% select(x)))

#transform points below a horizonal fold, or to the left of a vertical line
transform_folded_points <- function(which_fold, fold_line, map_ins = dot_coords) {
  if (which_fold == "horizontal") {
    #distance to fold line, NA if above fold line
    mapped <- map_ins %>% mutate(dtfl = if_else(y < fold_line, fold_line - y, NULL)) %>%
      mutate(y = if_else(!is.na(dtfl), dtfl, y - fold_line)) %>%
      filter(! y == 0) %>%
      distinct(x,y) %>%
      mutate(y = y - 1)
  } else if (which_fold == "vertical") {
    #distance to fold_line, NA is right of fold line
    mapped <- map_ins %>% mutate(dtfl = if_else(x < fold_line, fold_line - x, NULL)) %>%
      mutate(x = if_else(!is.na(dtfl), dtfl, x - fold_line)) %>% 
      filter(! x == 0) %>%
      distinct(x,y) %>%
      mutate(x = x - 1)
  }
  
  return(mapped)
}


print((transform_folded_points(which_fold = "vertical", fold_line = 655, dot_coords)))

#part 2

fold_list = list(
c("vertical",655),
c("horizontal",447),
c("vertical", 327),
c("horizontal", 223),
c("vertical", 163),
c("horizontal", 111),
c("vertical", 81),
c("horizontal", 55),
c("vertical", 40),
c("horizontal", 27),
c("horizontal", 13),
c("horizontal", 6)
)

folded_map <- dot_coords
for (i in 1:length(fold_list)) {
  folded_map <- transform_folded_points(which_fold =  fold_list[[i]][[1]], fold_line = as.numeric(fold_list[[i]][[2]]), map = folded_map)
}

