#day 17
source("utils.R")
#20..30, y=-10..-5
target_area <- list(x_lims = c(14,50), y_lims = c(-267,-225))
#target_area <- list(x_lims = c(20,30), y_lims = c(-10,-5))

#PART 1 = wolfram alpha !!!

#################################
#check if ball in target area
#################################
in_target_x <- function(x_pos, target = target_area){
  
  x_check <- between(x_pos, target$x_lims[1], target$x_lims[2])
  return(x_check)
}
#
in_target_y <- function(y_pos, target = target_area){
  
  y_check <- between(y_pos, target$y_lims[1], target$y_lims[2])
  return(y_check)
}


#################################
#horizontal 
#################################
horizontal_acc <- function(Vx){
  
  if (Vx < 0) {
    acc <  1
  } else if (Vx == 0) {
    acc <- 0
  } else {
    acc <- -1
  }
  
  return(acc)
}

horizontal_update <- function(Vxi, t){
  
  a <- horizontal_acc(Vxi)
  if (t==1){
    s <- Vxi
    max_x <- Vxi + a
    return(list(s,max_x))
  }
  max_x <- Vxi
  s <- max_x
  for (t in 2:t){
    max_x <- max_x + a
    if (max_x <= 0){
      max_x <- 0
      return(list(s,max_x))
    }
    s <- s + max_x
  }
  return(list(s,max_x))
  
}

#horizontal checks
horizontal_checks <-tibble(t = 1:600)
for (i in 1:60){
  horizontal_checks <- horizontal_checks %>%
    mutate(!!sym(as.character(i)) := unlist(in_target_x(unlist(purrr::map(t, ~(horizontal_update(!!(as.numeric(i)), .)[[1]]))))))
}
#filter for where in target is true
horizontal_checks <- horizontal_checks %>%
  filter_at(names(horizontal_checks)[-1], any_vars(. == TRUE))
horizontal_checks <- horizontal_checks %>%
  mutate_at(names(horizontal_checks[-1]), ~if_else(. == TRUE, TRUE,NA))
###############################
#vertical
###############################
vertical_acc <- -1
vertical_position <- function(Vyi, t){
  
  max_y <- Vyi
  s <- max_y
  for (t in 2:t){
    max_y <- max_y - 1
    s <- s + max_y
  }
  if (t == 1){
    s <- Vyi
  }
  return(s)
}

#vertical checks
vertical_checks <-tibble(t = 1:535)
for (i in -400:300){
  if ((i %% 100) == 0) {print(i)}
  vertical_checks <- vertical_checks %>%
    mutate(!!sym(as.character(i)) := unlist(in_target_y(unlist(purrr::map(t, ~(vertical_position(!!(as.numeric(i)), .)))))))
}
#filter for where in target is true
vertical_checks <- vertical_checks %>%
  filter_at(names(vertical_checks)[-1], any_vars(. == TRUE))
vertical_checks <- vertical_checks %>%
  mutate_at(names(vertical_checks[-1]), ~if_else(. == TRUE, TRUE,NA))
vertical_checks <- vertical_checks[,colSums(is.na(vertical_checks))<nrow(vertical_checks)]
###############################
#loop through each time point to get the initial speeds at which both 
#horiontal and vertical speeds cause the probe to enter the target simultaneosly
max_ys_list <- c()
combo_df <- tibble(hps = "", vps = "")
for (i in 1:600){
  hps <- names(which((horizontal_checks %>% filter(t==i) %>% as.list()) == TRUE))[-1]
  vps <- names(which((vertical_checks %>% filter(t==i) %>% as.list()) == TRUE))[-1]
  combos <- expand_grid(hps,vps) 
  combo_df <- combo_df %>% rbind(combos)
  max_ys <- combos$vps %>% as.numeric() %>% max()
  max_ys_list <- c(max_ys_list, max_ys)
}
#part 2
print(combo_df %>% distinct(hps,vps) %>% nrow() -1)
