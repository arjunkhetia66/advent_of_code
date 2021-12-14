#day 7

source("utils.R")

horizontal_positions <- scan("day7/day_7_input.txt", sep = ",")
#c(16,1,2,0,4,2,7,1,2,14)#

#part1

#distances to any given positon summed
fuel_cost <- function(positions, final_pos) {
  return(sum(abs(final_pos - positions)))
}

positions_to_test <- min(horizontal_positions):max(horizontal_positions)
#loop through all positions between min and max as the final position
fuel_costs <- purrr::map(positions_to_test, ~fuel_cost(horizontal_positions, .)) %>% unlist()
#min fuel cost
min_fuel_cost <- min(fuel_costs)

#part 2

natural_sum <- function(num){
  sum = (num * (num + 1)) / 2
}

new_fuel_cost <- function(positions_input, final_positions_input) {
  fuel_costs_normal <- abs(final_positions_input - positions_input)
  return(sum(purrr::map(fuel_costs_normal, ~natural_sum(.)) %>% unlist()))
}

#loop through all positions between min and max as the final position
fuel_costs_new <- purrr::map(positions_to_test, ~new_fuel_cost(horizontal_positions, .)) %>% unlist()
#min fuel cost
min_fuel_cost_new <- min(fuel_costs_new)
print(min_fuel_cost_new)