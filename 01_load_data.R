# Remove all variables
rm(list = ls(all.names = TRUE))

# Load necessary libraries
library(tidyverse)
library(readr)
library(lubridate)

#dynamic determination of time passed based on filenames

load("experiment_info.Rdata")

raw_data_folder_path = "00_raw_data"
setup_file_path <- str_c(raw_data_folder_path,"/",experiment_name,"/setup.xlsx")
parameters <- readxl::read_xlsx(setup_file_path, sheet = "parameters")
list2env(parameters,globalenv())
save(list = c(names(parameters),"experiment_name"),file = "experiment_info.Rdata")

# Improvements to be made
# make error report based on the mu values.
# Add the option to specify target_OD for each cell with
#   excel sheet. this sheet can also specify parameters

row_ID = c("A","B","C","D","E","F","G","H")
col_ID = c("1","2","3","4","5","6","7","8","9","10","11","12")

load_setup_data <- function(file_path, sheet_name){
  tbl <- readxl::read_xlsx(setup_file_path, col_names = FALSE, sheet = sheet_name)
  colnames(tbl) = col_ID
  tbl <- cbind(tbl,row_ID)
  tbl <-  
    tbl %>% 
    pivot_longer(cols = 1:12, names_to = "col_ID", values_to = "target_OD")
  return(tbl)
}

setup_data <- load_setup_data(setup_file_path,"target_OD")

load_plate_reader <- function(file_path){
  # file_path <- str_c(raw_data_folder_path,"/",experiment_name,"/202201111327.txt")
  tbl <- read_tsv(file_path,
                  col_names = FALSE,
                  skip = 5,
                  col_types = "cccccccccccccc")
  tbl <- tbl[1:8,3:14]
  colnames(tbl) <- col_ID
  tbl <- cbind(tbl,row_ID)
  tbl <- pivot_longer(tbl,
               "1":"12",
               values_to = "OD",
               names_to = "col_ID")  
  return(tbl)
}


# Loading data according to time-stamps


all_data <- tribble(
  ~filename,
  list.files(path = str_c(raw_data_folder_path,"/",experiment_name))
  ) %>% 
  unnest(filename) %>%
  filter(str_detect(filename, pattern = "^[\\d].*"))

all_data <- 
  all_data %>% 
  mutate(time_signature = str_extract(filename, pattern = "^[\\d]*"),
         time_signature = ymd_hm(time_signature)) %>% 
  arrange(time_signature) %>% 
  mutate(interval = interval(first(time_signature), time_signature),
         interval = case_when(
           is.na(interval) ~ period(0),
           is.interval(interval) ~ as.period(interval)),
         interval = period_to_seconds(interval)/3600)

all_data <- 
  all_data %>% 
  mutate(file_path = str_c(raw_data_folder_path,"/",experiment_name,"/",filename)) %>% 
  mutate(data = purrr::map(file_path, load_plate_reader)) %>% 
  unnest(data) %>% 
  full_join(setup_data, by = c("row_ID", "col_ID")) %>% 
  filter(row_ID %in% c("A","B","C","D"),
         col_ID %in% c("1","2","3","4","5","6"))

write_tsv(x = all_data, 
          file = "01_data_clean_up/01_augmented_data")


# log(OD_final) = log(X_2h)+(mu*t)
# log(OD_final)-(mu*t) = log(X_2h)

#Given the growth rate for the last two hour
#set the concentrations such that the desired OD is reached
#in another two hours

# X*exp(mu*t)
# log(X)+(mu*t)

