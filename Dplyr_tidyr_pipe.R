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

# filter without pipe
fd<- filter(file_data,file_data$storm_name=="KATRINA")
fd_reduced<- select(fd, month, day, hour, max_wind)
head(fd_reduced, 5)

#filter with pipe
file_data %>%
  filter(storm_name=='KATRINA') %>%
  select(month,day,hour,max_wind) %>%
  head(5)

#summarize
file_data %>%
  summarise(n_obs =n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

# use funtion in summary
knots_to_mile <- function(knots) {
  mph<-1.152 * knots
}

file_data %>%
  summarise(num = n(),
            worst_wind = knots_to_mile(max(max_wind)),
            worst_pressure = min(min_pressure))
