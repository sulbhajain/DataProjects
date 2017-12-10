library(tidyverse)

file_data <- read.csv("..//R Files/Data Files/flavors_of_cacao.csv")
summary(file_data)
names(file_data)
names(file_data) <- make.names(names(file_data), unique = TRUE)
head(file_data,5)
tail(file_data,6)

# --- replace space with '_'
names(file_data) <- gsub("[[:space:]+]","_",names(file_data))
str(file_data)
head(file_data[2,1])
head(file_data,15)
file_data <- file_data[-1,]
str(file_data)

head(file_data$Company...Maker.if.known.)
head(file_data$Specific.Bean.Origin.or.Bar.Name)
head(file_data$REF)

# --- data clean
file_data <- type.convert( file_data)

# --- remove % sign from cocoa percent
file_data$Cocoa.Percent <- sapply(file_data$Cocoa.Percent, function(x) gsub("%", "", x))
file_data <- type.convert( file_data)
str(file_data)

# ---- convert to numeric 
file_data$Cocoa.Percent <- as.numeric(file_data$Cocoa.Percent)
str(file_data) 
summary(file_data)

# summarize data
sd(file_data$Cocoa.Percent)
mean(file_data$Cocoa.Percent)
sd(file_data$Rating)
mean(file_data$Rating)

#file_data,
c(averageRating = mean(file_data$Rating),
            sdRating = sd(file_data$Rating))

c(average_percent = mean(file_data$Cocoa.Percent),
  sd_percent = sd(file_data$Cocoa.Percent))

grouping(file_data$Review.Date)
# file_data %>%
#   group_by(Review_Date) %>%
#   summarise(averageRating = mean(Rating),
#             sdRating = sd(Rating))

ggplot(file_data, aes(x=file_data$Review.Date,y = file_data$Rating)) + geom_point() 

# -- add jitter to avoid overlapping
ggplot(file_data, aes(x=file_data$Review.Date,y = file_data$Rating)) + geom_point() +
  geom_jitter() 

# --- add liner model to judge if ratings hav improved over time
ggplot(file_data, aes(x=file_data$Review.Date,y = file_data$Rating)) + geom_point() +
  geom_jitter() + geom_smooth(method="lm")

# -- add per cent to color to see how cocoa percent has affected rating
ggplot(file_data, aes(x=file_data$Review.Date,y = file_data$Rating, color = file_data$Cocoa.Percent)) + geom_point() +
  geom_jitter() + geom_smooth(method="lm")


# plot between cocoa percent and rating
ggplot(file_data, aes(x=file_data$Cocoa.Percent, y=file_data$Rating)) + geom_point()

ggplot(file_data, aes(x=file_data$Cocoa.Percent, y=file_data$Rating)) + geom_point() + 
   geom_jitter() + 
  geom_smooth(method = 'loess')

?geom_smooth
