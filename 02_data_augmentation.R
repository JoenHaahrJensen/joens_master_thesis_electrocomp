# Remove all variables
rm(list = ls(all.names = TRUE))

# Load necessary libraries
library(tidyverse)
library(readr)
library(lubridate)
load("experiment_info.Rdata")

all_data <- read_tsv(file = "01_data_clean_up/01_augmented_data",
                     lazy = FALSE)

all_data <- 
  all_data %>% 
  group_by(row_ID, col_ID ,target_OD) %>% 
  mutate(OD = OD - blank) %>% 
  nest()

all_data2 <- 
  all_data %>% 
  mutate(model = purrr::map(data, function(tbl) lm(log(OD) ~ interval, data = tbl)),
         tidy_model = purrr::map(model, broom::tidy)) %>% 
  unnest(tidy_model) %>% 
  select(row_ID:estimate)

all_data3 <-
  all_data2 %>% 
  group_by(row_ID,col_ID) %>% 
  pivot_wider(values_from = estimate, names_from = term) %>% 
  rename("intercept" = "(Intercept)",
         "mu" = "interval") %>%
  mutate(new_start_OD = exp(log(target_OD) - (mu * second_phase_duration_h)))

all_data4 <- 
  all_data3 %>% 
  mutate(mu_check = mu > 0.1)

all_volumes <- 
  all_data4 %>% 
  # filter(mu_check == TRUE) %>% 
  unnest(data) %>% 
  filter(interval == max(interval)) %>% 
  mutate(transfer_volume_cells = new_start_OD / OD * target_volume,
         max_cell_volume_check = transfer_volume_cells <= 3000,
         max_cell_volume_correction_factor = case_when(
           transfer_volume_cells > 3000 ~ 3000/transfer_volume_cells,
           transfer_volume_cells <= 3000 ~ 1 
         ), 
         transfer_volume_cells_corrected = transfer_volume_cells * max_cell_volume_correction_factor,
         transfer_volume_media = target_volume - transfer_volume_cells_corrected)

export_volumes <- 
  all_volumes %>% 
  pivot_longer(cols = c(transfer_volume_cells_corrected,transfer_volume_media),
               values_to = "transfer_volume",
               names_to = "source_plate") %>% 
  mutate(source_plate = case_when(
    str_detect(source_plate, pattern = "cells") ~ "cells",
    str_detect(source_plate, pattern = "media") ~ "media"
  )) %>% 
  select(row_ID, col_ID, source_plate, transfer_volume) %>% 
  ungroup(col_ID,row_ID) %>%
  mutate(row_num = case_when(
    row_ID == "A" ~ 0,
    row_ID == "B" ~ 1,
    row_ID == "C" ~ 2,
    row_ID == "D" ~ 3
  ),
  destination_plate = "rebalanced_culture",
  destination_position = (row_num) * 6 + col_ID,
  source_position = case_when(
    source_plate == "cells" ~ (row_num) * 6 + col_ID,
    source_plate == "media" ~ 1)) %>% 
  select(source_plate, source_position, destination_position, transfer_volume) %>% 
  filter(transfer_volume != 0)

tip_volume = 900

###
export_volumes2 <-
  export_volumes %>%
  mutate(transfer_replicates = ceiling(transfer_volume/tip_volume)) %>%
  group_by(source_plate, source_position, destination_position, transfer_volume) %>%
  expand(count = seq(1:transfer_replicates)) %>% 
  mutate(transfer_volume = case_when(
    tip_volume * count == transfer_volume ~ tip_volume,
    count == max(count) ~ transfer_volume %% tip_volume,
    count != max(count) ~ tip_volume ),
    transfer_volume = round(transfer_volume, digits = 0),
    dest_plate = "balanced_culture") %>% 
  select(source_plate, source_position, dest_plate, destination_position,transfer_volume)
#   
export_volumes2 %>%
  ungroup(everything()) %>%
  mutate(source = source_plate) %>% 
  group_by(source) %>%
  nest() %>%
  mutate(filename = str_c("03_data_visualisation/",experiment_name,"/export_volumes_",source,".csv"),
         export_file = purrr::walk2(.x = data, .y = filename, .f = write_csv))
# 
# ggsave(filename = str_c("X_error_report/",experiment_name,"/error_report_visual.png"), plot = visual_error_report)


