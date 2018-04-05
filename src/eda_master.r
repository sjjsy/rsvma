#### Exploratory data analysis on the master dataset

# This file includes all the quantitative analysis that was done to explore
# issues, support our main analysis and explore alternative analyses.

### Read the data

# Read the CSV data
csv <- read.csv("../data/master_dataset.csv", header=T, sep=",")
# Inspect
head(csv)
View(csv)

# Convert to data frame
df <- data.frame(csv)
# Inspect
nrow(df) # Number of observations
View(df)
summary(df)

### Prepare the dataframe

# Add `Year` column based on the `Quarter` column
df$Year <- as.integer(substring(df$Quarter, 0, 4))

# Handle some categorical variables
# Note: not all categorical variables are needed in this analysis
df$Name <- factor(df$Name)
df$CUSIPShort <- factor(df$CUSIPShort)
df$Quarter <- factor(df$Quarter)
df$SIC3 <- factor(df$SIC3)
df$SIC <- factor(df$SIC)

# Note: This is commented, because we decided to keep it as a percentage
## Convert the PIH variables from percent (0-100) to ratio (0-1)
#df$PIH <- df$PIH / 100
#df$PIH_lag4 <- df$PIH_lag4 / 100

# Generate some aggregating categorical variables out of numerical variables
# (with manually chosen and tuned buckets)
df$MarketValueC <- cut(df$MarketValue, c(0,10000,50000,100000,200000,100000000))
df$RDIntensityC <- cut(df$RDIntensity, c(0.00,0.02,0.05,0.10,0.20,1.00))
df$ROAC <- cut(df$ROA, c(-1.00,0.00,0.01,0.02,0.05,1.00))
df$ROARC <- cut(df$ROA_Rel, c(-100,0.5,1.0,1.5,100))
df$SlackC <- cut(df$Slack, c(0,1,2,3,4,1000))
df$SlackRC <- cut(df$Slack_Rel, c(-10,0.5,1.0,1.5,10))
df$PIHC <- cut(df$PIH, c(0,2,5,10,20,100))

# Note: This log-transform is not applied as the instructor did not ask for it
## Log-transform skewed variables
#df$MarketValue <- log(df$MarketValue)
#df$PIH <- log(df$PIH)
#df$PIH_lag4 <- log(df$PIH_lag4)

# Reorganize columns and eliminate the unnecessary ones by only including the
# following in the given order
df <- subset(df, select=c(
  X, SIC3, Name, Year,
  FirmAge,
  MarketValueC, MarketValue, MarketValue_lag1, MarketValue_lag2,
  #RDIntensityC, RDIntensity, RDIntensity_lag1, RDIntensity_lag2,
  ROARC, ROA_Rel, ROARel_lag1, ROARel_lag2,
  SlackRC, Slack_Rel, SlackRel_lag1, SlackRel_lag2,
  PIHC, PIH, PIH_lag1, PIH_lag2,
  AcquisitionCount))

# Inspect
View(df)

## Eliminate rows with any missing values and report the loss of data in terms
# of firms and firm-quarters
# Store the firms present
names.before <- unique(df$Name)
# Eliminate the rows
df <- df[complete.cases(df),]
# Compare to the firms after
names.after <- unique(df$Name)
firms.lost <- setdiff(names.before, names.after)
cat(sprintf("We lost %.0f%% of the firms due to missing data (%.0f -> %.0f)!", 100*length(firms.lost)/length(names.before), length(names.before), length(names.after)))
cat(sprintf("In terms of firm-quarter observations the loss was from %.0f to %.0f.", nrow(csv), nrow(df)))
cat(sprintf("The following %.0f firms were lost: %s.", length(firms.lost), paste(as.character(firms.lost), collapse=", ")))
# Notes on data loss
#   obs.  firms  data
#   2156     49  full data
#   1845     48  eliminated with FA, MV,      ROA,        PIH
#   1502     37  eliminated with FA, MV,      ROA, Slack, PIH -- ALLSTATE CORP, AMERICAN EXPRESS CO, BANK OF AMERICA CORP, CITIGROUP INC, FORD MOTOR CO, GENERAL ELECTRIC CO, GOLDMAN SACHS GROUP INC, JPMORGAN CHASE & CO, MORGAN STANLEY, U S BANCORP, WELLS FARGO & CO, MEDTRONIC PLC.
#    807     20  eliminated with FA, MV, RDI, ROA, Slack, PIH

# Inspect
View(df)

# Define a short string to describe the dataset to facilitate comparison of
# figures and other analysis output
data_desc <- sprintf('%dx%d', nf, no)
outfilepath <- function(name, suffix) {
  sprintf('../out/eda__%s__%s.%s', name, data_desc, suffix)
}

## Make another dataset that just has the 
# Aggregate data frame by Name returning means
# for numeric variables FIXME TODO
dfa <- subset(df, Year == "2005" | Year == "2006", c(Name, MarketValue, ROA_Rel, Slack_Rel, PIH, AcquisitionCount))
dfa <- aggregate(dfa, by=list(dfa$Name), FUN=mean, na.rm=TRUE)
rownames(dfa) <- dfa$Group.1
dfa <- dfa[-c(1:2)]
#colnames(df) <- c("FAge", "MktV", "ROAR", "SlaR", "PIH", "AcqC")
#dfb <- subset(df, Year == "2014" | Year == "2015")
View(dfa)
#plot(dfa$MarketValue_lag1)


### Produce views to the distribution of some variables
# Histograms of market value, ROA, PIH & acquisition count
png(outfilepath('histograms', 'png'), width=800, units='px')
par(mfrow=c(2,2))
hist(df$MarketValue, pch=1, main="Histogram of market value", xlab="Market value (MUSD)")
hist(df$ROA, pch=2, main="Histogram of return on assets", xlab="ROA")
hist(df$PIH, pch=3, main="Histogram of PIH", xlab="PIH")
hist(df$AcquisitionCount, pch=4, main="Histogram of acquisition count", xlab="Acquisition count")
par(mfrow=c(1,1))
dev.off()

png(outfilepath('rdi', 'png'), width=800, units='px')
hist(df$RDIntensity, main="Histogram of R&D intensity", xlab="R&D intensity")
dev.off()

## Barplot of SIC3
# Convert the SIC3 column into a contingency table array with frequencies and
# sort them; then plot the most common factors grouping the rest if more than
# `nmax` are found
freq <- sort(table(df$SIC3), decreasing=T)
nmax <- 16
n <- length(which(freq > 0))
if (n > nmax) {
  freqp <- freq[0:(nmax-1)]
  freqp[nmax] <- sum(freq[nmax:length(freq)])
  names(freqp)[nmax] <- 'oth'
} else {
  freqp <- freq[0:n]
}
# Plot the data
png(outfilepath('sic3', 'png'), width=800, units='px')
barplot(as.vector(freqp), names.arg=names(freqp),
        main="Frequency plot of SIC3",
        xlab="SIC3",
        ylab="Frequency")
dev.off()

### Compute basic stats and correlations

# Plot a correlation matrix between all numerical variables
library(corrgram)
corrgram(df[c(4,6,9,12,15,17)])

# Compute the correlations between all numerical variables
cor(df[,c(4,6,9,12,15,17)], method="pearson")
# Note: fails with observations with missing data without the
# 'use="complete.obs"' argument

# Boxplot acquisition count vs year
boxplot(AcquisitionCount ~ Year, data=df,
        main="Acquisition activity over time",
        xlab="Year",
        ylab="Acquisition count per firm per quarter") 

# Boxplot acquisition count vs market value
boxplot(AcquisitionCount ~ MarketValueC, data=df,
        main="Acquisition activity by firm size",
        xlab="Market value",
        ylab="Acquisition count per firm per quarter",
        names=c("<10B$","10-50B$","50-100B$","100-200B$", ">200B$"))

# Boxplot acquisition count vs R&D intensity
boxplot(AcquisitionCount ~ RDIntensityC, data=df,
        main="Acquisition activity by firm R&D intensity",
        xlab="R&D intensity",
        ylab="Acquisition count per firm per quarter",
        names=c("<2%","2-5%","5-10%","10-20%", ">20%"))

# Boxplot acquisition count vs ROA
boxplot(AcquisitionCount ~ ROAC, data=df,
        main="Acquisition activity by firm ROA",
        xlab="ROA",
        ylab="Acquisition count per firm per quarter",
        names=c("<0%","0-1%","1-2%","2-5%", ">5%"))

# Boxplot PIH vs acquisition count
boxplot(AcquisitionCount ~ PIHC, data=df,
        main="Acquisition activity by PIH",
        xlab="PIH",
        ylab="Acquisition count this quarter",
        names=c("<2%","2-5%","5-10%","10-20%", ">20%"))

# Plot PIH vs acquisition count
par(mfrow=c(2,1))
plot(df$PIH_lag1, df$AcquisitionCount, xlab="PIH one quarter ago", ylab="Quarterly acquisition count")
plot(df$PIH_lag2, df$AcquisitionCount, xlab="PIH two quarters ago", ylab="Quarterly acquisition count")
par(mfrow=c(1,1))

# Plot PIH vs acquisition count
library(ggplot2)
ggplot(df, aes(AcquisitionCount, fill = PIHC)) + geom_histogram(binwidth = 1)
+ facet_grid(PIHC ~ ., margins = TRUE, scales = "free")

### Compute the correlation between PIH and acquisition count 
cor.test(df$PIH_lag1, df$AcquisitionCount, method="pearson")
cor.test(df$PIH_lag2, df$AcquisitionCount, method="pearson")
# Note: The argument 'use="complete.obs"' is necessary if data includes
# observations with missing data
# -->
#   4Q lag and 1845 observations: cor=-0.03432, p=0.1406
#   2Q lag and  807 observations: cor=-0.01992, p=0.5719
#   1Q lag and  807 observations: cor=-0.02424, p=0.4916

### Generate table of basic statistics
stats <- data.frame(apply(df[4:8], 2, min), apply(df[4:8], 2, max), apply(df[4:8], 2, mean), apply(df[4:8], 2, sd))
row.names(stats) <- c("Year", "MarketValue", "ROA", "RDI", "PIH_lag4", "AcquisitionCount")
colnames(stats) <- c("Min", "Max", "Mean", "SD")
mcor <- cor(df[4:8], use="complete.obs", method="pearson")
stats <- cbind(stats, mcor)
write.csv(stats, file=outfilepath('stats', 'csv'))

### Form and analyze different linear models

# Make `Year` categorical for use as a dummy
df$Year <- factor(df$Year)

# Model 1: Just the control variables
lm1 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1 + ROA_lag1 + RDIntensity_lag1, data=df)
lm1r <- summary(lm1)

# Model 2: The control variables and PIH
lm2 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1 + ROA_lag1 + RDIntensity_lag1 + PIH_lag1, data=df)
lm2r <- summary(lm2)

# Model 3: The control variables and PIH robustness check
lm3 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag2 + ROA_lag2 + RDIntensity_lag1 + PIH_lag2, data=df)
lm3r <- summary(lm3)

## Generate tables of regression results
write.csv(lm1r$coefficients[,c(1,2,4)], file=outfilepath('lm1r', 'csv'))
write.csv(lm2r$coefficients[,c(1,2,4)], file=outfilepath('lm2r', 'csv'))
write.csv(lm3r$coefficients[,c(1,2,4)], file=outfilepath('lm3r', 'csv'))

### Analyze the relative importance of each variable

library(relaimpo)
ri <- calc.relimp(lm2, type=c("lmg"), rela=TRUE)

# Plot a pie chart
png(outfilepath('relimpo', 'png'), width=800, units='px')
pie(ri$lmg, col=rainbow(length(ri$lmg)), main="The relative importance of the variables") 
dev.off()

### Analyze within a specific industry

df2 <- df[df$SIC3 == '621',]
View(df2)
lm <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + ROA + PIH_lag4, data=df)
lmr <- summary(lm)
calc.relimp(lm, type=c("lmg"), rela=TRUE)

# EOF
