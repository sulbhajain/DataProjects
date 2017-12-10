
# Manual loading of file
stats <- read.csv(file.choose())
stats

# automatic file load
getwd()
getwd()
stat1 <- read.csv("DemographicData.csv")
stat1


# ----exploring data -----
nrow(stat1)
# imported 195 rows
ncol(stat1)
head(stat1, n=10)
tail(stat1, n =10)
str(stat1)
rnorm(stat1)
summary(stat1)

# ----$ sign
stat1[2,3]
stat1[,2]
stat1[2,]
stat1$Country.Name
stat1[,"Country.Name"]
stat1$Country.Code[2]
levels(stat1$Income.Group)

?ls()
  
