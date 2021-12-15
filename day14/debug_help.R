#helper functions for debugging


#function to count how many of a particular letter in given template
count_letters <- function(template_ins, letter) {
  return((sum(template_ins %in% letter)))
}

#function to count number of each letter in template
count_letters_bigger <- function(template){
  split_template <- strsplit(template,"")[[1]]
  #keep count of letters in template
  letters_count <- split_template %>% tibble()
  names(letters_count) = "letters"
  letters_count <- letters_count %>% distinct(letters)
  
  letters_count <- letters_count %>% 
    mutate(counts = as.numeric(unlist(purrr::map(letters, ~count_letters(split_template, .)))))
  
  return(letters_count)
}

#apply lookup to pairs and insert into template
insertion <- function(split_template){
  split_template_paired <- runner(split_template, k = 2, f = paste, collapse = "")
  inserts <- purrr::map(split_template_paired, ~which_insert(.)) %>% unlist()
  j=1
  for (i in (2:length(inserts))){
    split_template<-append(split_template, inserts[i], after=j)
    j = j + 2
  }
  return(split_template)
  
}