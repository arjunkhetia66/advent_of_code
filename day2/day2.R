#day 2
source("utils.R")

#directions table
directions <- readr::read_delim("day2/submarine_directions.txt", delim = " ")
#starting_position
start_pos <- c(0,0,0)
#function to change position
change_position <- function(initial_pos, change) {
  final_position <- initial_pos + change
  
  return(final_position)
}
#lookup to add change as expected to direction table
change_lookup <- function(position, direction, number){
  aim <- position[3]
  change <- case_when(
   #forward X does two things:
   #It increases your horizontal position by X units.
   #It increases your depth by your aim multiplied by X.
   direction == "forward" ~ (c(1,aim,0) * number), 
   direction == "up" ~ c(0, 0, -1) * number, #up X decreases your aim by X units
   direction == "down" ~ c(0, 0, 1) * number #down X increases your aim by X units.
 )
  return(change)
} 

direction_list <- directions[["direction"]]
number_list <- directions[["number"]]

for (i in 1:length(direction_list)) {
  print(i)
  if (i == 1) {
    change_position_input <- change_lookup(start_pos,direction_list[[i]],number_list[[i]])
    new_position <- change_position(start_pos,change_position_input)
  } else {
    change_position_input <- change_lookup(new_position,direction_list[[i]],number_list[[i]])
    new_position <- change_position(new_position,change_position_input)
    }
  print(new_position)
}

