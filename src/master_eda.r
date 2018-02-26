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

# Convert from "%" to ratio
df$PIH <- df$PIH / 100
df$PIH_lag4 <- df$PIH_lag4 / 100

# Reorganize columns and eliminate unnecessary columns
df <- df[c("X", "SIC3", "Name", "Year", "MarketValue", "ROA", "PIH_lag4", "AcquisitionCount")]

# Inspect
View(df)

# Eliminate rows with any missing values
df <- df[complete.cases(df),]
View(df)

### Compute basic stats and correlations

# Plot a correlation matrix between all numerical variables
library(corrgram)
corrgram(df[,4:8])

# Compute the correlations between all numerical variables
cor(df[,5:8], use="complete.obs", method="pearson")

# Plot PIH vs acquisition count 
plot(df$PIH_lag4, df$AcquisitionCount, xlab="PIH one year ago", ylab="Number of acquisitions this year")

# Compute the correlation between PIH and acquisition count 
cor.test(df$PIH_lag4, df$AcquisitionCount, use="complete.obs", method="pearson")
# --> cor=-0.03432, p=0.1406

## Generate table of basic statistics
stats <- data.frame(apply(df[4:8], 2, min), apply(df[4:8], 2, max), apply(df[4:8], 2, mean), apply(df[4:8], 2, sd))
row.names(stats) <- c("Year", "MarketValue", "ROA", "PIH_lag4", "AcquisitionCount")
colnames(stats) <- c("Min", "Max", "Mean", "SD")
mcor <- cor(df[4:8], use="complete.obs", method="pearson")
stats <- cbind(stats, mcor)
write.csv(stats, file="../out/stats.csv")

### Form and analyze different linear models

# Make `Year` categorical
df$Year <- factor(df$Year)

# Model 1: Just the control variables
lm1 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + ROA, data=df)
lm1r <- summary(lm1)

# Model 2: The control variables and PIH
lm2 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + ROA + PIH_lag4, data=df)
lm2r <- summary(lm2)

## Generate table of regression results
write.csv(lm1r$coefficients[,c(1,2,4)], file="../out/lm1.csv")
write.csv(lm2r$coefficients[,c(1,2,4)], file="../out/lm2.csv")

# EOF