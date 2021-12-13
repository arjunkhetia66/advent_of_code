library(tidyverse)
library(lubridate)
library(stringr)
library(runner)



#####################################################################
#                   data.table functions                            #  
#####################################################################
make_names <- function(names, unique=FALSE, leading_ = '' ) { 
  
  #some basic cleaning on our colnames
  names <- stringr::str_replace_all(names, c("\\(.*\\)" = "", #remove any parentheses and content from within
                                             "\\s$" = "", #remove whitespace at the end of a string
                                             "[:punct:]" = " ",
                                             "\\s+" = " ")) #remove any punctuation
  
  if( ! substr( leading_, 1, 1 ) %in%  c( '', '.', letters, LETTERS ) )
    stop( "'leading_' must be a lower- or uppercase letter or '.'" )
  
  # USE make.names
  # names <- sub( '^[^A-Za-z\\.]+', '.', names )# replace leading non-lead character with .
  names <- make.names( names, allow_ = TRUE ) # make.names, allow underscores
  names <- gsub( '\\.', '_', names )          # replace . -> _
  names <- gsub( "\\_+", "_", names )         # replace multiple leading _ with single _    
  
  if( unique ) names <- make.unique( names, sep="_")  # make.unique
  
  # REPLACE LEADING _ WITH leading_ 
  # leading <- grepl( '^_', names )
  # substr( names[ leading ], 1, 1 ) = leading_  
  names <- gsub( '^_', leading_, names)
  
  return(names)
  
}


###################################################################
#                         tidyverse functions                     #
###################################################################

rename_columns <- function(input_df, name_mapping_input, keep_all = FALSE){
  #renames columns of a tibble df using a name mapper list
  
  if (keep_all == TRUE) {
    non_specified_names <- as.list(names(input_df)[! names(input_df) %in% names(name_mapping_input)])
    names(non_specified_names) <- non_specified_names
    
    name_mapping_input <- c(non_specified_names, name_mapping_input)
  }
  
  renamed_df <- input_df %>% select(all_of(names(name_mapping_input)))
  names(renamed_df) <- name_mapping_input[names(renamed_df)] %>% unlist() %>% as.character()
  
  return(renamed_df)
  
}

#function that turns a df column into a list, most usful to replace the $ notation
make_list_from_col <- function(input_df, input_col){
  #input_df: data.table df, input_col: string
  
  output_list <- as.list(input_df %>% select(all_of(input_col)))[[1]]
  
  return(output_list)
}

#######################################################################

#function that builds on classic readr::read_csv() function
#replaces spaces with "_" and removes any cols read in as "X1" (if they exist...)
custom_read_csv <- function(filepath, class_lookup_option = FALSE, class_lookup = NULL) {
  #read in our df (at present this defaults to reading in blank colnames as V1, V2... and doesn't remove these)
  #the logic behind this is that given some error in the backing data where we have multiple index cols (as is the case
  #as of Apr 2020), then fread errors out
  
  if (class_lookup_option == TRUE) {
    df <- readr::read_csv(filepath, col_types = class_lookup)
  } else {
    df <- readr::read_csv(filepath)
  }
  
  #find colnames and remove any we don't require
  cols_to_remove <- grep("^[A-Z][1-9]$", names(df))
  #if there are cols to remove, then remove them...
  if(length(cols_to_remove > 0)) { df <- select(df,-cols_to_remove)}
  
  #clean up colnames and make them more consistent
  names(df) <- make_names(names(df)) %>% tolower()
  
  return(df)
}

colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMin <- function(data) sapply(data, min, na.rm = TRUE)