library(tidyr)
library(dplyr)

file_path <- paste0("http://rammb.cira.colostate.edu/research",
                    "/tropical_cyclones/tc_extended_best_track_dataset/",
                    "data/ebtrk_atlc_1988_2015.txt")

file_cols_width <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                     4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
file_cols <- c("storm_id", "storm_name", "month", "day",
               "hour", "year", "latitude", "longitude",
               "max_wind", "min_pressure", "rad_max_wind",
               "eye_diameter", "pressure_1", "pressure_2",
               paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
               paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
               paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
               "storm_type", "distance_to_land", "final")

file_data <- read.fwf(file_path, fwf(  file_cols_width,file_cols),na= '-99')
head(file_data,4)

# starts with 
file_data %>%
  select(storm_name, month, day, year, starts_with("radius_34"))

# ends with
file_data %>%
  select(ends_with("e"))

# contains
file_data %>%
  select(contains("34"))

# matches
file_data %>%
 select( matches("_[0-9][0-9]_"))

# fiter
file_data %>%
  select(storm_name, max_wind) %>%
  filter(max_wind >100)

file_data %>%
  select(storm_name, max_wind) %>%
  filter(storm_name %in% c("ALBERTO", "GILBERT")) %>%
  group_by(storm_name) %>%
  summarise(num = n(),
            max_wind = max(max_wind))

# filter on speed > 120

file_data %>%
  select(storm_name, max_wind) %>%
  filter(storm_name %in% c("ALBERTO", "GILBERT")) %>%
  group_by(storm_name) %>%
  summarise(num = n(),
            max_wind = max(max_wind)) %>%
  filter(max_wind > 120)

# missing values - is.na()
file_data %>%
  select(storm_name, radius_34_se ,max_wind) %>%
  filter(!is.na(radius_34_se))
