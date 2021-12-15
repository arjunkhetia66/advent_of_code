#day 14
source("utils.R")
source("day14/debug_help.R")

#test template
test_template <- "NNCB"
#load in test pair insertion rules
test_pairs <-  readr::read_delim("day14/day_14_test_input.txt", delim = " -> ", col_names = FALSE) %>%
  rename_columns(list(X1 = "pair", X3 = "insert"))

#real template
real_template <- "PBFNVFFPCPCPFPHKBONB"
#load in real pair insertion rules
real_pairs <-  readr::read_delim("day14/day_14_input.txt", delim = " -> ", col_names = FALSE) %>%
  rename_columns(list(X1 = "pair", X3 = "insert"))

#test or real mode
template <- real_template
pairs_insert <- real_pairs

#split template up
split_template <- strsplit(template,"")[[1]]

#function to lookup which letter is inserted
which_insert <- function(pair_checked, pair_insert_lookup = pairs_insert){
  return(pair_insert_lookup %>% filter(pair == pair_checked) %>% select(insert) %>% as.character())
}

#count pairs in template
pair_count_func <- function(template) {
  
  split_template <- strsplit(template,"")[[1]]
  pair_splits <- runner(split_template, k = 2, f = paste, collapse = "")
split_template_paired_table <- pair_splits %>%
  unique() %>%
  tibble
names(split_template_paired_table) <- "pairs"
split_template_paired_table <- split_template_paired_table %>% filter(nchar(pairs) == 2) %>%
  mutate(counts = as.numeric(unlist(purrr::map(pairs, ~sum(pair_splits %in% .)))))
return(split_template_paired_table)
}

split_template_paired_table <- pair_count_func(template)

#function that takes a pair and generates the two new pairs after insertion
new_pairs <- function(pair, insertion){
  return(c(paste0(strsplit(pair,"")[[1]][[1]],insertion), paste0(insertion,strsplit(pair,"")[[1]][[2]])))
}

counts_post_insertion <- function(pair_counts) {
unique_pairs <- pair_counts
unique_pairs_old <- unique_pairs

#loop through unqiue pairs replacing counts of pairs with new ones after insertion
for (i in unique_pairs$pairs){
  insertion <- which_insert(i)
  how_many_counts <- unique_pairs_old$counts
  names(how_many_counts) <- unique_pairs_old$pairs
  new_pairs_gen <- new_pairs(i, insertion)
  #replace counts for  2 new pairs
  for (j in new_pairs_gen){
    if (j %in% unique_pairs$pairs){
      unique_pairs <- unique_pairs %>%
        mutate(counts = if_else(j == pairs, how_many_counts[[i]] + counts, counts))
    } else {
      unique_pairs <- unique_pairs %>%
        rbind(tibble(pairs = j, counts = how_many_counts[[i]]))
    }
    }
  }

#minus original counts due to insertion replacement
unique_pairs <- unique_pairs %>% left_join(unique_pairs_old, by = "pairs") %>% 
  mutate(counts.y = if_else(is.na(counts.y),0,counts.y)) %>%
  mutate(counts = counts.x - counts.y) %>%
  select(pairs, counts)

return(unique_pairs)

}

#do n loops 
pairs_counted <- split_template_paired_table
for (i in 1:40){
  print(i)
  pairs_counted <- counts_post_insertion(pairs_counted)
}
#check how many times the letters have appeared in their pairs
pairs_counted <- pairs_counted %>% mutate(first_letter = substr(pairs,1,1),
                         second_letter = substr(pairs,2,2)) %>% group_by(second_letter) %>% summarise(sum(counts))
  
print(max(pairs_counted$`sum(counts)`) - min(pairs_counted$`sum(counts)`))
