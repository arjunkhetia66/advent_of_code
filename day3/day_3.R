#day3

source("utils.R")

#binary table
binary_report <- readr::read_csv("day3/day_3_input.txt", col_names = FALSE)
names(binary_report) <- c("binary_lines")

#PART 1
#binary lines as list
binary_lines <- binary_report[["binary_lines"]]
no_in_line <- length(strsplit(binary_lines[[1]], "")[[1]])

#split each position into it's own col
for (i in 1:no_in_line) {
  i_char <- as.character(i)
  binary_report <- binary_report %>%
    mutate( !!i_char :=  substr(binary_lines,i,i))
}

#find sum of each col
modal <- binary_report %>%
  summarise_at(as.character(1:12), ~ as.numeric((sum(as.numeric(.))) > 500))
gamma <- modal %>% as.list() %>% unlist() %>% as.character() %>% paste(collapse = "")
gamma_no <- base::strtoi(gamma, base = 2)

anti_modal <- binary_report %>%
  summarise_at(as.character(1:12), ~ as.numeric((sum(as.numeric(.))) < 500))
epsilon <- anti_modal %>% as.list() %>% unlist() %>% as.character() %>% paste(collapse = "")
epsilon_no <- base::strtoi(epsilon, base = 2)

power_con <- gamma_no * epsilon_no

#PART 2
modal_oxygen <- binary_report %>%
  summarise_at(as.character(1:12), ~ as.numeric((sum(as.numeric(.))) >= as.numeric(nrow(binary_report)/2)))
oxygen <- modal_oxygen %>% as.list() %>% unlist() %>% as.character() %>% paste(collapse = "")
oxygen_split <- strsplit(oxygen,"")[[1]]
binary_report_filt_o2 <- binary_report
for (j in 1:length(oxygen_split)){
  modal_oxygen <- binary_report_filt_o2 %>%
    summarise_at(as.character(1:12), ~ as.numeric((sum(as.numeric(.))) >= as.numeric(nrow(binary_report_filt_o2)/2)))
  oxygen <- modal_oxygen %>% as.list() %>% unlist() %>% as.character() %>% paste(collapse = "")
  oxygen_split <- strsplit(oxygen,"")[[1]]
  binary_report_filt_o2 <- binary_report_filt_o2 %>%
    filter(!!sym(as.character(j)) == oxygen_split[[j]])
  print(binary_report_filt_o2)
  if (nrow(binary_report_filt_o2) == 1) {
    o2_final <- base::strtoi((binary_report_filt_o2[["binary_lines"]]), base = 2)
    stop("found o2 binary")
  }
  }
  
modal_co2 <- binary_report %>%
  summarise_at(as.character(1:12), ~ as.numeric((sum(as.numeric(.))) <= 499))
co2 <- modal_co2 %>% as.list() %>% unlist() %>% as.character() %>% paste(collapse = "")
co2_split <- strsplit(oxygen,"")[[1]]
binary_report_filt_co2 <- binary_report
for (j in 1:length(co2_split)){
  modal_co2 <- binary_report_filt_co2 %>%
    summarise_at(as.character(1:12), ~ as.numeric((sum(as.numeric(.))) <= as.numeric(nrow(binary_report_filt_co2)/2)-1))
  co2 <- modal_co2 %>% as.list() %>% unlist() %>% as.character() %>% paste(collapse = "")
  co2_split <- strsplit(co2,"")[[1]]
  binary_report_filt_co2 <- binary_report_filt_co2 %>%
    filter(!!sym(as.character(j)) == co2_split[[j]])
  print(binary_report_filt_co2)
  if (nrow(binary_report_filt_co2) == 1) {
    co2_final <- base::strtoi((binary_report_filt_co2[["binary_lines"]]), base = 2)
    stop("found co2 binary")
  }
}
