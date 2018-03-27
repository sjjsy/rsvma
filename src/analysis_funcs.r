#### Data analysis functions

# This file includes the functions that load and adjust the dataset as well as
# those that perform the quantitative analysis that was done to provide the
# results for our research report.

rsvma.load_prepared_master_dataset <- function()
{
  ### Load the data

  # Read the CSV data
  csv <- read.csv("../data/master_dataset.csv", header=T, sep=",")

  # Convert to data frame
  df <- data.frame(csv)

  ### Prepare the dataframe
  
  # Add `Year` column based on the `Quarter` column
  df$Year <- factor(as.integer(substring(df$Quarter, 0, 4)))

  # Factorize the required categorical variables
  df$Name <- factor(df$Name)
  df$CUSIPShort <- factor(df$CUSIPShort)
  df$SIC3 <- factor(df$SIC3)
  df$Quarter <- factor(df$Quarter)

  # Reorganize columns and eliminate the unnecessary ones by only including the
  # following in the given order
  df <- subset(df, select=c(
    X, Name, CUSIPShort, SIC3, Quarter, Year,
    MarketValue_lag1, MarketValue_lag2,
    ROARel_lag1, ROARel_lag2,
    SlackRel_lag1, SlackRel_lag2,
    PIH_lag1, PIH_lag2,
    AcquisitionCount))

  # Eliminate rows with any missing values
  df <- df[complete.cases(df),]
}
rsvma.load_prepared_master_dataset_dev <- function()
{
  ### Load the data
  
  # Read the CSV data
  csv <- read.csv("../data/master_dataset.csv", header=T, sep=",")
  
  # Convert to data frame
  df <- data.frame(csv)
  
  ### Prepare the dataframe
  
  # Add `Year` column based on the `Quarter` column
  df$Year <- factor(as.integer(substring(df$Quarter, 0, 4)))
  
  # Factorize the required categorical variables
  df$Name <- factor(df$Name)
  df$CUSIPShort <- factor(df$CUSIPShort)
  df$SIC3 <- factor(df$SIC3)
  df$Quarter <- factor(df$Quarter)

  # Reorganize columns and eliminate the unnecessary ones by only including the
  # following in the given order
  df <- subset(df, select=c(
    X, Name, CUSIPShort, SIC3, Quarter, Year,
    MarketValue, MarketValue_lag1, MarketValue_lag2,
    PIH, PIH_lag1, PIH_lag2,
    AcquisitionCount))
  
  # Eliminate rows with any missing values
  df <- df[complete.cases(df),]
}
rsvma.compute_descriptive_stats <- function(df, outfilepath)
{
  # This is required for rcorr
  require(Hmisc)

  # Compute basic descriptive statistics
  stats <- data.frame(apply(df, 2, min), apply(df, 2, max), apply(df, 2, mean), apply(df, 2, sd))
  colnames(stats) <- c("Min", "Max", "Mean", "SD")

  # Compute the correlations and the p-values of the correlation coefficients
  # and add them into the table
  r <- rcorr(as.matrix(df), type="pearson")
  stats <- cbind(stats, r$r, r$P)

  # Write the data into a CSV file
  write.csv(stats, file=outfilepath('stats', 'csv'))
  
  # Return the stats
  return(stats)
}
rsvma.run_primary_lms <- function(df, outfilepath)
{
  # We form, compute and store as CSV the results of three linear regression
  # models. In each case we perform cluster-robust inference with this
  # firm-by-quarter dataset to reduce heteroskedasticity, more specifically,
  # the bias that arises from the dataset having several observations per
  # firm which are thus mutually dependent (or clustered by firm). We use
  # the package clubSandwich following the example here:
  # https://cran.r-project.org/web/packages/clubSandwich/vignettes/panel-data-CRVE.html
  require(clubSandwich)

  # Model 1: Just the control variables
  lm1 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1 + ROARel_lag1 + SlackRel_lag1, data=df)
  lm1r <- summary(lm1)
  lm1rc <- coef_test(lm1, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")

  # Model 2: The control variables and PIH
  lm2 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1 + ROARel_lag1 + SlackRel_lag1 + PIH_lag1, data=df)
  lm2r <- summary(lm2)
  lm2rc <- coef_test(lm2, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")

  # Model 3: The control variables and PIH robustness check
  lm3 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag2 + ROARel_lag2 + SlackRel_lag2 + PIH_lag2, data=df)
  lm3r <- summary(lm3)
  lm3rc <- coef_test(lm3, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")

  ## Generate tables of regression results
  write.csv(lm1r$coefficients[,c(1,2,4)], file=outfilepath('lm1r', 'csv'))
  write.csv(lm2r$coefficients[,c(1,2,4)], file=outfilepath('lm2r', 'csv'))
  write.csv(lm3r$coefficients[,c(1,2,4)], file=outfilepath('lm3r', 'csv'))
  write.csv(lm1rc$coefficients[,c(1,2,3)], file=outfilepath('lm1rc', 'csv'))
  write.csv(lm2rc$coefficients[,c(1,2,3)], file=outfilepath('lm2rc', 'csv'))
  write.csv(lm3rc$coefficients[,c(1,2,3)], file=outfilepath('lm3rc', 'csv'))
  
  # Compute variance inflation factors (VIFs) to detect multicollinearity. Any
  # individual VIF greater than 10 indicates multicollinearity.
  vif1 <- 1/(1-lm1r$r.squared^2)
  vif2 <- 1/(1-lm2r$r.squared^2)
  vif3 <- 1/(1-lm3r$r.squared^2)
}
rsvma.run_primary_lms_dev <- function(df, outfilepath)
{
  # We form, compute and store as CSV the results of three linear regression
  # models. In each case we group the coefficients by firm to reduce bias that
  # arises from having several observations per firm. Two packages are required
  # for the grouping.
  require(clubSandwich)
  require(multiwayvcov)
  require(lmtest)
  
  # Model 1: Just the control variables
  lm1 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1, data=df)
  lm1r <- summary(lm1)
  lm1rc <- coeftest(lm1, cluster.vcov(lm1, df$Name))

  # Model 2: The control variables and PIH
  lm2 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1 + PIH_lag1, data=df)
  lm2r <- summary(lm2)
  lm2rc <- coeftest(lm2, cluster.vcov(lm2, df$CUSIPShort))
  #coef_test(lm2, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")[30:33,]
  #coef_test(lm2, vcov="CR2", cluster=df$CUSIPShort, test="Satterthwaite")[30:33,]
  
  # Model 3: The control variables and PIH robustness check with 2 quarter lag
  lm3 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag2 + PIH_lag2, data=df)
  lm3r <- summary(lm3)
  lm3rc <- coeftest(lm3, cluster.vcov(lm3, df$CUSIPShort))
  #coef_test(lm3, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")
  #coef_test(lm3, vcov="CR2", cluster=df$CUSIPShort, test="Satterthwaite")
  
  # Model 4: The control variables and PIH robustness check with 0 quarter lag
  lm4 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue + PIH, data=df)
  lm4r <- summary(lm3)
  lm4rc <- coeftest(lm3, cluster.vcov(lm3, df$Name))

  ## Generate tables of regression results
  write.csv(lm1r$coefficients[,c(1,2,4)], file=outfilepath('lm1r', 'csv'))
  write.csv(lm2r$coefficients[,c(1,2,4)], file=outfilepath('lm2r', 'csv'))
  write.csv(lm3r$coefficients[,c(1,2,4)], file=outfilepath('lm3r', 'csv'))
  write.csv(lm1rc[,c(1,2,4)], file=outfilepath('lm1rc', 'csv'))
  write.csv(lm2rc[,c(1,2,4)], file=outfilepath('lm2rc', 'csv'))
  write.csv(lm3rc[,c(1,2,4)], file=outfilepath('lm3rc', 'csv'))
  
  # Compute variance inflation factors (VIFs) to detect multicollinearity. Any
  # individual VIF greater than 10 indicates multicollinearity.
  vif1 <- 1/(1-lm1r$r.squared^2)
  vif2 <- 1/(1-lm2r$r.squared^2)
  vif3 <- 1/(1-lm3r$r.squared^2)
  vif4 <- 1/(1-lm4r$r.squared^2)

  # Models:          normal             cluster
  #  Y+S3+MV1+PIH1   -5.4e-2  3.4e-5   -5.4e-2  6.5e-2
  #  Y+S3+MV2+PIH2   -4.5e-2  2.2e-4   -4.5e-2  8.1e-2
}