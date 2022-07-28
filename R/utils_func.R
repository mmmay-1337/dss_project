#library(data.table)
library(tidyverse)
# library(foreach)
# library(doParallel)
# library(parallel)
# library(yaml)
# library(keyring)
# library(moment)
# library(zoo)
# library(lubridate)
# library(xgboost)
# library(paradox)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
# library(future)
library(mlr3viz)
# library(lgr) # logging
library(mlr3filters)

# reads in SQL files
.utils_read_sql <- function(con, file, qry_config = NULL, echo = FALSE) {# need to specify a qry_config example
 
  sql_files <- # remove all comments from the sql file
    readLines(file) %>% 
    # collapse it into 1 line (instead of many lines), separated by 'lines'
    paste(collapse = "\n") %>% 
    # remove all comments
    str_remove_all("/\\*(.*\n{1,}\\*/|/\\*.*\\*/|--.*\n") %>% 
    # split the SQL execution string s as recognised by the ;
    strsplit(";") %>% 
    unlist
  
  # remove all blank items (after trimming)
  sql_file <- sql_file[sql_files != "\n"]
   
  # qry_config basically acts as an interpolator - translate all the relevant points
  if (length(qry_config) > 0) {
    for (i in 1:length(qry_config)) {
      # replace
      sql_file <- 
        gsub(paste0("\\?", names(qry_config)[i]), if(!is.na(as.Date(unlist(qry_config[i]), format = "%Y-%m-%d"))) {
          paste0("'", unlist(qry_config[i]), "'")
        } else {
          unlist(qry_config[i])
        }, sql_file)
    }
  }
  
  # run the code
  for (i in 1:length(sql_file)) {
    if (echo == TRUE) {
      cat(sql_file[i])
    } else {}
    # if the query is just 1 line (no splits then it should be a table)
    if (length(sql_file) == 1) {
      out = suppressWarnings(DBI::dbGetQuery(con, sql_file[i]))
      return(out)
    } else {suppressWarnings(DBI::dbGetQuery(con, sql_file[i]))}
  }
  sprintf("completed running SQL script")
}

.utils_std_tbl_upload <- function(table) {
  out <- table %>%
    rename_all(toupper)
  return(out)
}

.utils_assign_nchar <- function(x) {
  ceiling(max(nchar(x), na.rm = TRUE)/10) * 10

}

# create columns
.utils_create_cols <- function(table) {
  table <- as.data.frame(table)
  
  # initialise an empty vector
  x <- c("(")
  
  # to reference the right table name, replace spaces or punctuations with underscores
  z <- names(table) %>% gsub("[[:punct:]]|[[:space:]]", "_", .)

# step logger
.utils_log_step <- function(x, ...) {
  x <- paste(x, ...)
  return(print(paste(paste0(Sys.time(), ":")), x))
}

# log error
.utils_halt_execution <- function(x) {
  stop(paste0(Sys.time(), ":", x))
}

# stratify sample
.utils_stratify_sample <- function(ds
                                   , strata
                                   , sample_prop
                                   , seed = 8564) {
  set.seed(seed)
  # count the number of observations for uniqut combo of strata
  out <- ds %>%
    # note that lazy_dt col names will be diff
    as_tibble %>%
    # revised dplyr::sample_frac can use a group by statement as stratas
    group_by_at({
      {
        strata
      }
    }) %>%
    
    sample_frac(size = sample_prop, replace = FALSE)
  
  # return the stratified sample
  return(out)
  
}

# other useful thingy
# Sys.chmod(paths = file, mode = 0775)

.utils_file_name <- function(x, ...) {
  paste(list(x, ...), collapse = "/") %>% 
    gsub("//", "/", .)
}

# non-working function yet
.utils_rm_objs <- function(default = TRUE) {
  if (default) {
    rm(list = grep("^[zvr]_|^ts_", ls(), value = TRUE), envir = globalenv())
  }
}

 
