library(tidyverse)

experiment_name = "20220111_DH5a_growth"
save(list="experiment_name", file = "experiment_info.Rdata")

dir.create(str_c("01_data_clean_up/", experiment_name))
dir.create(str_c("02_data_augmentation/", experiment_name))
dir.create(str_c("03_data_visualisation/", experiment_name))
dir.create(str_c("X_error_report/", experiment_name))

source("01_load_data.R")
source("02_data_augmentation.R")

