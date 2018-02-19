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

# Handle categorical variables
df$CUSIPShort <- factor(df$CUSIPShort)
df$Quarter <- factor(df$Quarter)
df$SIC <- factor(df$SIC)
df$PIH <- df$PIH / 100

# Compute the correlation between PIH and acquisition count 
plot(df$PIH, df$AcquisitionCount)
cor(df$PIH, df$AcquisitionCount, use="complete.obs", method="pearson")
cor(df, use="complete.obs")
cor.test(df$PIH, df$AcquisitionCount, use="complete.obs", method="pearson")

library(corrgram)
corrgram(df[,11:15])
cor(df[,11:15], use="complete.obs", method="pearson")

## Form and analyze different linear models

# Model 1: Just the control variables
lm1 <- lm(AcquisitionCount ~ Quarter + SIC + MarketValue + ROA, data=df)
summary(lm1)

# Model 2: The control variables and PIH
lm2 <- lm(AcquisitionCount ~ Quarter + SIC + MarketValue + ROA + PIH, data=df)
summary(lm2)

## Lag of 4Q for PIH
library(dyn)
lm3 <- dyn$lm(AcquisitionCount ~ lag(PIH,-4), data=df)
summary(lm3)


