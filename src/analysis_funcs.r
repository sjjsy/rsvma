#### Data analysis functions

# This file includes the functions that load and adjust the dataset as well as
# those that perform the quantitative analysis that was done to provide the
# results for our research report.

rsvma.load_prepared_master_dataset <- function()
{
  cat(sprintf("Loading and preparing the dataset...\n"))
  ### Load the data
  
  # Read the CSV data
  csv <- read.csv("../data/master_dataset.csv", header=T, sep=",")
  
  # Convert to data frame
  df <- data.frame(csv)
  
  ### Prepare the dataframe
  
  # Add `Year` column based on the `Quarter` column
  df$Year <- as.integer(substring(df$Quarter, 0, 4))
  
  # Recompute FirmAge
  df$FirmAge <- as.numeric(df$Year) - as.numeric(df$YearFounded)
  
  # Factorize the required categorical variables
  df$Year <- factor(df$Year)
  df$Name <- factor(df$Name)
  df$CUSIPShort <- factor(df$CUSIPShort)
  df$SIC3 <- factor(df$SIC3)
  df$Quarter <- factor(df$Quarter)

  # Reorganize columns and eliminate the unnecessary ones by only including the
  # following in the given order
  df <- subset(df, select=c(
    X, Name, CUSIPShort, SIC3, Quarter, Year,
    YearFounded, FirmAge,
    MarketValue, MarketValue_lag1, MarketValue_lag2,
    ROA_Rel, ROARel_lag1, ROARel_lag2,
    #Slack_Rel, SlackRel_lag1, SlackRel_lag2,
    PIH, PIH_lag1, PIH_lag2,
    AcquisitionCount))
  
  ## Eliminate rows with any missing values
  # Store the firms present
  names.before <- unique(df$Name)
  # Eliminate the rows
  df <- df[complete.cases(df),]
  # Compare to the firms after
  names.after <- unique(df$Name)
  firms.lost <- setdiff(names.before, names.after)
  cat(sprintf("We lost %.0f%% of the firms due to missing data (%.0f -> %.0f)!\n", 100*length(firms.lost)/length(names.before), length(names.before), length(names.after)))
  cat(sprintf("In terms of firm-quarter observations the loss was from %.0f to %.0f.\n", nrow(csv), nrow(df)))
  cat(sprintf("The following %.0f firms were lost: %s.\n", length(firms.lost), paste(as.character(firms.lost), collapse=", ")))
  ## Sub-sample analysis
  # Reduce the sample to those that have higher PIH than average
  #df <- subset(df, PIH_lag1 > mean(df$PIH))
  ## Return the data frame
  return(df)
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
  lm1 <- lm(AcquisitionCount ~ Year + SIC3 + FirmAge + MarketValue_lag1 + ROARel_lag1 + SlackRel_lag1, data=df)
  lm1r <- summary(lm1)
  lm1rc <- coef_test(lm1, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")

  # Model 2: The control variables and PIH
  lm2 <- lm(AcquisitionCount ~ Year + SIC3 + FirmAge + MarketValue_lag1 + ROARel_lag1 + SlackRel_lag1 + PIH_lag1, data=df)
  lm2r <- summary(lm2)
  lm2rc <- coef_test(lm2, vcov="CR1", cluster=df$CUSIPShort, test="naive-t")

  # Model 3: The control variables and PIH robustness check
  lm3 <- lm(AcquisitionCount ~ Year + SIC3 + FirmAge + MarketValue_lag2 + ROARel_lag2 + SlackRel_lag2 + PIH_lag2, data=df)
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
  # arises from having several observations per firm.
  # https://cran.r-project.org/web/packages/clubSandwich/vignettes/panel-data-CRVE.html
  # https://stats.stackexchange.com/questions/10017/standard-error-clustering-in-r-either-manually-or-in-plm
  require(clubSandwich)
  require(multiwayvcov)
  require(lmtest)
  require(car)
  
  # Model 1: Just the control variables
  lm1 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1, data=df)
  lm1r <- summary(lm1)
  lm1rc <- coeftest(lm1, cluster.vcov(lm1, df$Name))

  # Model 2: The control variables and PIH
  lm2 <- lm(AcquisitionCount ~ Year + SIC3 + MarketValue_lag1 + PIH_lag1, data=df)
  lm2r <- summary(lm2)
  lm2rc <- coeftest(lm2, cluster.vcov(lm2, df$CUSIPShort))
  #lm2rc <- coeftest(lm2, cluster.vcov(lm2, cbind(df$CUSIPShort, df$Quarter)))
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
  # More advanced version
  vif(lm1)
  vif(lm2)
  vif(lm3)
  vif(lm4)
  
  # Models:          normal             cluster
  #  Y+S3+MV1+PIH1   -5.4e-2  3.4e-5   -5.4e-2  6.5e-2
  #  Y+S3+MV2+PIH2   -4.5e-2  2.2e-4   -4.5e-2  8.1e-2
}
rsvma.run_primary_nbs <- function(df, outfilepath)
{
  cat(sprintf("Computing the statistics and saving them to the out folder..."))
  # Require necessary libraries
  require(MASS)
  require(car)
  # Compute models
  m1r <- summary(m1 <- glm.nb(AcquisitionCount ~ Year + SIC3 + FirmAge + MarketValue_lag1 + ROARel_lag1, data=df))
  m2r <- summary(m2 <- glm.nb(AcquisitionCount ~ Year + SIC3 + FirmAge + MarketValue_lag1 + ROARel_lag1 + PIH_lag1, data=df))
  m3r <- summary(m3 <- glm.nb(AcquisitionCount ~ Year + SIC3 + FirmAge + MarketValue_lag2 + ROARel_lag2 + PIH_lag2, data=df))
  # Generate and store tables of regression results
  write.csv(coef(m1r), file=outfilepath('nb_m1r', 'csv'))
  write.csv(coef(m2r), file=outfilepath('nb_m2r', 'csv'))
  write.csv(coef(m3r), file=outfilepath('nb_m3r', 'csv'))
  # Compute the chi-squared test of log-likelihood ratio or
  # Perform sequential likelihood ratio tests for negative binomial generalized linear models
  acomp <- anova(m1, m2, m3)
  # Store the model comparison statistics
  mcomp <- matrix(c(m1$twologlik, m2$twologlik, m3$twologlik, m1$aic, m2$aic, m3$aic, acomp$`LR stat.`, acomp$`Pr(Chi)`),ncol=3,byrow=TRUE)
  rownames(mcomp) <- c("2x Log Likelihood", "AIC", "Anova LLR", "Anova Pr(Chi)")
  colnames(mcomp) <- c("M1", "M2", "M3")
  write.csv(mcomp, file=outfilepath('nb_mcomp', 'csv'))
  # comparison of models:
  #   non-significant p-value --> use model with smaller df
  #   significant p-value --> use model with smaller AIC
  vif(m1)
  vif(m2)
  vif(m3)
  ## Summary of results
  #                                                          l=b    l=b
  #   Variables                   coeff.   p-value  2xloglh  aic   ndev
  #   Y+S3+FA+MV1+RR1+SR1                                    1316  755
  #   Y+S3+FA+MV1+RR1+SR1+PIH1    -1.3e-2  7.9e-2   -3.96e2  
  #   Y+S3+FA+MV2+RR2+SR2+PIH2    -1.3e-2  5.0e-2   -3.95e2
  #   Y+S3+FA+MV1+RR1    +PIH1    -1.7e-2  4.7e-3
  #   Y+S3+FA+MV2+RR2    +PIH2    -1.8e-2  1.6e-3
}
