#### Exploratory data analysis

### Read the data

# Read the CSV data
csv <- read.csv("../data/master_dataset.csv", header=T, sep=",")
# Inspect
head(csv)
View(csv)

# Convert to data frame
df <- data.frame(csv)
# Inspect
View(df)
summary(df)

### Prepare the dataframe

# Number of observations
nobs <- nrow(df)

# Add `Year` column based on the `Quarter` column
df$Year <- as.integer(substring(df$Quarter, 0, 4))

# Add `SIC3` column based on the `SIC` column but with last digit removed
df$SIC3 <- factor(substring(df$SIC, 0, 3))

# Handle some categorical variables
df$CUSIPShort <- factor(df$CUSIPShort)
df$Quarter <- factor(df$Quarter)
df$SIC <- factor(df$SIC)

# Convert the PIH variables from percent (0-100) to ratio (0-1)
df$PIH <- df$PIH / 100
df$PIH_lag4 <- df$PIH_lag4 / 100

# Inspect the distribution of some variables
par(mfrow=c(2,2))
hist(df$MarketValue, pch=1)
hist(df$ROA, pch=2)
hist(df$PIH, pch=3)
hist(df$AcquisitionCount, pch=4)
par(mfrow=c(1,1))

# Log-transform skewed variables -- NOT APPLIED
df$MarketValue <- log(df$MarketValue)
df$PIH <- log(df$PIH)
df$PIH_lag4 <- log(df$PIH_lag4)

# Reorganize columns and eliminate unnecessary columns
df <- df[c("X", "SIC3", "Name", "Year", "MarketValue", "ROA", "PIH_lag4", "AcquisitionCount")]

# Inspect
View(df)

# Eliminate rows with any missing values
df <- df[complete.cases(df),]
View(df)

# Compute the numbers of data points in terms of firm-quarters and firms
nobs <- nrow(df) # --> 1845 which means that there were 311 rows with missing values (2156-1845=311)
nofs <- length(unique(csv$CUSIP)) # --> 49
nofs <- length(unique(df$Name)) # --> 48

### Compute basic stats and correlations

# Plot a correlation matrix between all numerical variables
library(corrgram)
corrgram(df[,4:8])

# Compute the correlations between all numerical variables
cor(df[,4:8], use="complete.obs", method="pearson")

# Plot PIH vs acquisition count 
plot(df$PIH_lag4, df$AcquisitionCount, xlab="PIH of the preceding year", ylab="Number of acquisitions this year")

# Compute the correlation between PIH and acquisition count 
cor.test(df$PIH_lag4, df$AcquisitionCount, use="complete.obs", method="pearson")
# --> cor=-0.03432, p=0.1406

## Generate table of basic statistics
stats <- data.frame(apply(df[4:8], 2, min), apply(df[4:8], 2, max), apply(df[4:8], 2, mean), apply(df[4:8], 2, sd))
row.names(stats) <- c("Year", "MarketValue", "ROA", "PIH_lag4", "AcquisitionCount")
colnames(stats) <- c("Min", "Max", "Mean", "SD")
mcor <- cor(df[4:8], use="complete.obs", method="pearson")
stats <- cbind(stats, mcor)
write.csv(stats, file="../out/statsC.csv")

### Form and analyze different linear models

# Make `Year` categorical
df$Year <- factor(df$Year)

# Model 1: Just the control variables
lm1 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + ROA, data=df)
lm1r <- summary(lm1)

# Model 2: The control variables and PIH
lm2 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + ROA + PIH_lag4, data=df)
lm2r <- summary(lm2)

## Generate tables of regression results
write.csv(lm1r$coefficients[,c(1,2,4)], file="../out/lm1c.csv")
write.csv(lm2r$coefficients[,c(1,2,4)], file="../out/lm2c.csv")

### Analyze the relative importance of each variable

library(relaimpo)
calc.relimp(lm2, type=c("lmg"), rela=TRUE)

### Analyze within a specific industry

df2 <- df[df$SIC3 == '621',]
View(df2)
lm <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + ROA + PIH_lag4, data=df)
lmr <- summary(lm)
calc.relimp(lm, type=c("lmg"), rela=TRUE)

# EOF