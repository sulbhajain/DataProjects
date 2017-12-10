install.packages(c("dplyr", "tidyr", "readr", "readxl"))

library(tidyr)
library(dplyr)
data_file<- read.csv("daily_SPEC_2014.csv.bz2")

str(data_file)

head(data_file$Parameter.Name)

#What is average Arithmetic.Mean for ???Bromine PM2.5 LC??? in the state of Wisconsin in this dataset?

avg_mean<- data_file %>%
  filter(data_file$Parameter.Name == 'Bromine PM2.5 LC' & data_file$State.Name == "Wisconsin")  %>%
  summarize(val=mean(Arithmetic.Mean, na.rm=TRUE))
(avg_mean)

str(data_file)

#Calculate the average of each chemical constituent across all states, monitoring sites and all time points.
avg_chemical <- data_file %>%
  group_by(Parameter.Name) %>%
  summarize(m=mean(Arithmetic.Mean, na.rm=TRUE)) %>%
              arrange(desc(m))
# filter(N == max(N))
(avg_chemical)
 
#Which monitoring site has the highest average level of ???Sulfate PM2.5 LC??? across all time?
#Indicate the state code, county code, and site number.
#Sulfate PM2.5 LC
dt_file<- data_file %>%
#select( State.Code, County.Code, Site.Num, Parameter.Name) %>%
filter(Parameter.Name=='Sulfate PM2.5 LC') %>%
group_by(State.Code,County.Code, Site.Num) %>%
summarize(m = mean(Arithmetic.Mean, na.rm=TRUE)) %>%
arrange(desc(m))

dt_file
#What is the absolute difference in the average levels of ???EC PM2.5 LC TOR??? between the states California and Arizona, across all time and all monitoring sites?
# EC PM2.5 LC TOR
head(data_file$State.Name)
#d1_file<- data_file %>%
  filter(data_file, Parameter.Name == 'EC PM2.5 LC TOR' & State.Name %in% c('Arizona','California')) %>%
group_by(Parameter.Name, State.Name) %>%
  summarize(avg=mean(Arithmetic.Mean, na.rm=TRUE)) %>%  
 spread(State.Name, avg) %>%
 mutate(diff = Arizona - California)

#What is the median level of ???OC PM2.5 LC TOR??? in the western United States, across all time? Define western as any monitoring location that has a Longitude LESS THAN -100.
  
  
filter(data_file, Longitude < -100 & Parameter.Name == "OC PM2.5 LC TOR") %>%
  group_by(Parameter.Name) %>%
  summarize(avg=median(Arithmetic.Mean, na.rm=TRUE)) 

filter(data_file, Parameter.Name %in% c("OC PM2.5 LC TOR", "EC PM2.5 LC TOR")) %>%
  mutate(region = ifelse(Longitude < -100, "west", "east")) %>%
  group_by(Parameter.Name, region) %>%
  summarize(median = median(Arithmetic.Mean)) %>%
  spread(region, median)

#Use the readxl package to read the file aqs_sites.xlsx into R (you may need to install the package first). This file contains metadata about each of the monitoring sites in the EPA???s monitoring system. In particular, the "Land Use" and "Location Setting" variables contain information about what kinds of areas the monitors are located in (i.e. ???residential??? vs. ???forest???).
#How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting"?
library(readxl)

#file_xl<-read_xls("aqs_sites.xlsx")
file_xl<-read.csv("aqs_sites.csv")
head(file_xl)

filter(file_xl, Land.Use=='RESIDENTIAL' & Location.Setting=='SUBURBAN') %>%
summarize(N=n())

d_f<- data_file %>%
  #filter(Parameter.Name=="EC PM2.5 LC TOR" & Longitude>=-100 ) %>% 
  mutate(County.Code=as.numeric(County.Code), Site.Num = as.numeric(Site.Num), State.Code=(State.Code)) %>%
  select(State.Code, County.Code, Site.Num, Parameter.Name, Arithmetic.Mean, Longitude)
  
f_x<- file_xl %>% 
  rename(Site.Num=Site.Number) %>% 
#  filter(Land.Use=='RESIDENTIAL' & Location.Setting=='SUBURBAN') %>%
  mutate(State.Code=as.numeric(State.Code))

str(d_f)
str(f_x)
m<-left_join(d_f, f_x, by=c("State.Code","County.Code","Site.Num")) 

str(m)

#What is the median level of ???EC PM2.5 LC TOR??? amongst monitoring sites that are labelled as both ???RESIDENTIAL??? and ???SUBURBAN??? in the eastern U.S., where eastern is defined as Longitude greater than or equal to -100?
  filter(m,Parameter.Name=="EC PM2.5 LC TOR" 
         & Longitude.y>=-100 
         & Land.Use=='RESIDENTIAL' 
         & Location.Setting=='SUBURBAN') %>%
  group_by(Parameter.Name) %>%
  summarize(md=median(Arithmetic.Mean))

  sites <- rename(file_xl, Site.Num = Site.Number) %>%
    mutate(State.Code=as.numeric(State.Code)) %>%
    select(State.Code, County.Code, Site.Num, Longitude, Land.Use, 
           Location.Setting)
  spec <- mutate(data_file, State.Code = as.numeric(State.Code),
                 County.Code = as.numeric(County.Code),
                 Site.Num = as.numeric(Site.Num)) %>%
    select(State.Code, County.Code, Site.Num, Parameter.Name, Arithmetic.Mean)
  m1 <- left_join(spec, sites, by = c("State.Code", "County.Code", "Site.Num"))

  str(m1)
  filter(m1, Parameter.Name %in% c("OC PM2.5 LC TOR", "EC PM2.5 LC TOR")
         & Land.Use == "RESIDENTIAL" & Location.Setting == "SUBURBAN"
         & Longitude >= -100) %>%
    group_by(Parameter.Name) %>%
    summarize(median = median(Arithmetic.Mean))

#count of all locations  
with(sites, table(Land.Use, Location.Setting))  

str(file_xl)
head(file_xl)
str(data_file)
#--------------------------------------------------
#Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use", which month of the year has the highest average levels of "Sulfate PM2.5 LC"?
d_f1<- data_file %>%
  #filter(Parameter.Name=="EC PM2.5 LC TOR" & Longitude>=-100 ) %>% 
  mutate(County.Code=as.numeric(County.Code), Site.Num = as.numeric(Site.Num), State.Code=(State.Code),
         Month=format(as.Date(m2$Date.Local),"%m")) %>%
  select(State.Code, County.Code, Site.Num, Parameter.Name, Arithmetic.Mean, Date.Local, Month )

#ymd(d_f1$Date.Local)
m2 <- left_join(d_f1, f_x, by = c("State.Code", "County.Code", "Site.Num"))

format(as.Date(m2$Date.Local),"%m")

m3<-m2%>%
filter(Parameter.Name == "Sulfate PM2.5 LC"
       & Land.Use == "COMMERCIAL") %>%
 group_by(Parameter.Name, Month ) %>%
summarize(med = median(Arithmetic.Mean)) 
  
arrange(m3, desc(med))    

data_file %>%
  left_join(file_xl, by = c("Latitude", "Longitude")) %>%
  filter(Land.Use == "COMMERCIAL",
         Parameter.Name == "Sulfate PM2.5 LC") %>%
  mutate(month = lubridate::month(Date.Local, label = TRUE)) %>%
  group_by(month) %>%
  summarise(avg = mean(Arithmetic.Mean, na.rm = TRUE)) %>%
  arrange(desc(avg))
#----------------------------------
#Take a look at the data for the monitoring site identified by State Code 6, County Code 65, and Site Number 8001 (this monitor is in California). At this monitor, for how many days is the sum of "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" greater than 10?

For each of the chemical constituents, there will be some dates that have multiple `Arithmetic Mean` values at this monitoring site. When there are multiple values on a given date, take the average of the constituent values for that date.
d_f1<- data_file %>%
#  filter(Parameter.Name=="Total Nitrate PM2.5 LC" | Parameter.Name=="Sulfate PM2.5 LC" ) %>% 
  mutate(County.Code=as.numeric(County.Code), Site.Num = as.numeric(Site.Num), State.Code=(State.Code)) %>%
  select(State.Code, County.Code, Site.Num, Parameter.Name, Arithmetic.Mean, Date.Local )

m3<-d_f1%>%
  filter(Parameter.Name %in% c( "Sulfate PM2.5 LC","Total Nitrate PM2.5 LC") &
           State.Code==6 &
           County.Code==65 &
           Site.Num==8001) %>%
  group_by(Parameter.Name, Date.Local ) %>%
  summarize(med = mean(Arithmetic.Mean, na.rm=TRUE)) %>%
  group_by(Date.Local) %>%
  summarize(tot=sum(med, na.rm=TRUE)) %>%
  filter(tot>10)

m3
#------------------------------------
#cor()
#Which monitoring site in the dataset has the highest correlation between "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates? Identify the monitoring site by it's State, County, and Site Number code.

#For each of the chemical constituents, there will be some dates that have multiple Sample.Value's at a monitoring site. When there are multiple values on a given date, take the average of the constituent values for that date.

#Correlations between to variables can be computed with the cor() function.
df1<- data_file %>%
  filter(Parameter.Name %in% c("Sulfate PM2.5 LC","Total Nitrate PM2.5 LC") )%>%
  group_by( State.Code, County.Code,  Site.Num, Parameter.Name, Date.Local) %>%
  select(State.Code, County.Code, Site.Num, Parameter.Name, Date.Local, Arithmetic.Mean) %>%
summarize(med = mean(Arithmetic.Mean, na.rm=TRUE)) %>%
spread(Parameter.Name, med) %>%
group_by( State.Code, County.Code,  Site.Num) %>%
  summarize(corr= cor(`Sulfate PM2.5 LC`,`Total Nitrate PM2.5 LC`)) %>%
  arrange(desc(corr))

df1

df2<- data_file %>%
  filter(Parameter.Name=="Total Nitrate PM2.5 LC") %>%
  select(State.Code, County.Code, Site.Num, Arithmetic.Mean) %>%
  group_by( State.Code, County.Code,  Site.Num) %>%
  summarize(med = mean(Arithmetic.Mean, na.rm=TRUE))

cor(df1, df2)
