library(lubridate)
ymd("2006-03-12","America")

library(tidyr)
library(deplyr)


plots<-read.csv("plots.csv")
species<- read.csv("species.csv")
head(plots)

counts_df<- data_frame(
  day=c('Monday', 'Tuesday', 'Wednesday'),
  wolf = c(1,2,3),
  hare=c(34,12,34),
  frog=c(4,7,9))
counts_df

c_df<-counts_df %>%
  gather(key='specie', value=count, wolf:frog)
c_df<-c_df[-8, ] # removng 8th row from the gathered dataset will create NA in spread format
c_df

# counter for gather
c_spread<- c_df %>%
  spread(key='specie', value=count)
c_spread

# filter on survey data
surveys<-read.csv("surveys.csv")
head(surveys)
surveys_filter<-surveys %>%
  filter(year==1990, month %in% 1:3)
surveys_filter

surveys_filter <- select(surveys_filter,-year)
head(surveys_filter)

# sorting
survey_sorted<- arrange(surveys_filter, desc(species_id), weight)
head(survey_sorted)

# Write code that returns the record_id, sex and weight of all surveyed individuals of Reithrodontomys montanus (RO).

survey_select <- surveys %>%
  filter(species_id == 'RO') %>%
 select(record_id, sex, weight) 
head(survey_select)

#Write code that returns the average weight and hindfoot length of Dipodomys merriami (DM) 
#individuals observed in each month (irrespective of the year). Make sure to exclude NA values
?mean
survey_s <- surveys %>%
  filter(species_id =='DM') %>%
group_by(species_id,month) %>%
  summarize(av_wt = mean(weight, na.rm = TRUE),
            av_length = mean(hindfoot_length, na.rm = TRUE))
survey_s


# groupby and summarizr
sur <- surveys %>% 
  filter()
  group_by(species_id) %>%
  summarize(N=n())
head(sur)

sur_m<- sur %>%
  mutate(prop = N/sum(N))
head(sur_m)

sur_join<- inner_join(sur, species)
head(sur_join)

# Return only the rows in counts_1990w_join that correspond to the most common species in each genus.
cc<- sur_join %>%
  group_by(genus) %>%
  summarize(row= n()) %>%
  filter(row == max(row))
head(cc)

# Calculate the fraction of total counts by taxa (birds or rodents) represented by each species within that taxon.
cc1<- sur_join %>%
  group_by(taxa) %>%
  summarize(co= n()) %>%
  mutate(frc=co/sum(co))
head(cc1)


new_counts <- surveys %>%
  filter(year == 1990, month %in% 1:3) %>% 
  select(-year) %>%
  group_by(species_id) %>%
  summarize(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  inner_join(species)
head(new_counts)
