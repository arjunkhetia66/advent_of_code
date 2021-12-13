#day 6
#part 1
rm(list = ls())

source("utils.R")

internal_timers <- c(1,2,4,5,5,5,2,1,3,1,4,3,2,1,5,5,1,2,3,4,4,1,2,3,2,1,4,4,1,5,
5,1,3,4,4,4,1,2,2,5,1,5,5,3,2,3,1,1,3,5,1,1,2,4,2,3,1,1,2,1,
3,1,2,1,1,2,1,2,2,1,1,1,1,5,4,5,2,1,3,2,4,1,1,3,4,1,4,1,5,1,
4,1,5,3,2,3,2,2,4,4,3,3,4,3,4,4,3,4,5,1,2,5,2,1,5,5,1,3,4,2,
2,4,2,2,1,3,2,5,5,1,3,3,4,3,5,3,5,5,4,5,1,1,4,1,4,5,1,1,1,4,
1,1,4,2,1,4,1,3,4,4,3,1,2,2,4,3,3,2,2,2,3,5,5,2,3,1,5,1,1,1,
1,3,1,4,1,4,1,2,5,3,2,4,4,1,3,1,1,1,3,4,4,1,1,2,1,4,3,4,2,2,3
,2,4,3,1,5,1,3,1,4,5,5,3,5,1,3,5,5,4,2,3,2,4,1,3,2,2,2,1,3,4,2,
5,2,5,3,5,5,1,1,1,2,2,3,1,4,4,4,5,4,5,5,1,4,5,5,4,1,1,5,3,3,1,4,
1,3,1,1,4,1,5,2,3,2,3,1,2,2,2,1,1,5,1,4,5,2,4,2,2,3)

no_days <- 80

#change timer
change_timer <- function(timer) {
  if (timer == 0) {
    timer_new <- 6 
  } else {
    timer_new = timer -1
  
  return(timer_new)
  }
}

new_fishies <- c()
for (i in 1:(no_days+1)){
 if (i != 1){
    internal_timers <- purrr::map(internal_timers, ~change_timer(.)) %>% unlist()
    internal_timers <- c(internal_timers, new_fishies)
    #count how many 0s we have
    zero_count <- length(which(internal_timers == 0))
    new_fishies <- rep(8,zero_count)
    }
  print(paste0("day",i))
  #print(internal_timers)
  #print("______________________________________")
}

print(length(internal_timers))

