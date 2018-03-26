#### Exploratory data analysis on the M&A dataset

### Read the data

# Read the CSV data
csv <- read.csv("../data/ma_05-15.csv", header=T, sep=";")
# Inspect
head(csv)
View(csv)

# Convert to data frame
df <- data.frame(csv)
# Inspect
head(csv)
View(csv)

# Handle some categorical variables
df$Target.Nation <- factor(df$Target.Nation)
df$Acquiror.Full.Name <- factor(df$Acquiror.Full.Name)

### Some basic plots

plot(df$Date.Announced)
hist(df$Value.of.Transaction...mil.)
plot(df$Target.Nation)

summary(df$Target.Nation)

# The sm.density.compare( ) function in the sm package allows you to superimpose
# the kernal density plots of two or more groups. The format is
# sm.density.compare(x, factor) where x is a numeric vector and factor is the
# grouping variable. More at: https://www.statmethods.net/graphs/density.html
#library(sm)
#sm.density.compare(df$Value.of.Transaction...mil., df$Acquiror.Full.Name)
## add legend via mouse click
#colfill<-c(2:(2+length(levels(cyl.f))))
#legend(locator(1), levels(cyl.f), fill=colfill)

### Inspect data related to Google Inc

df1 <- df[which(df$Acquiror.Full.Name=="Google Inc"),]
hist(df1$Value.of.Transaction...mil.)
df1$Date.Announced <- as.Date(df1$Date.Announced, "%m/%d/%Y")
summary(df1$Date.Announced)
plot(df1$Date.Announced)