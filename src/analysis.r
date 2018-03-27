#### Data analysis

# This file houses two main functions that conduct the quantitative analysis
# to provide the results for our research report.

### Read the data
source("analysis_funcs.r")

rsvma.analysis_final <- function()
{
  ## Load and prepare the dataset
  df <- rsvma.load_prepared_master_dataset()
  # Define a short string to describe the dataset to facilitate comparison of
  # figures and other analysis output
  data_desc <- sprintf('%dx%d', length(unique(df$Name)), nrow(df))
  outfilepath <- function(name, suffix) {
    sprintf('../out/%s__%s.%s', name, data_desc, suffix)
  }
  ## Generate table of basic statistics
  stats <- rsvma.compute_descriptive_stats(subset(df, select=c("MarketValue_lag1", "ROARel_lag1", "SlackRel_lag1", "PIH_lag1", "AcquisitionCount")), outfilepath)
  ## Form and analyze the different linear models
  rsvma.run_primary_lms(df, outfilepath)
}

rsvma.analysis_dev <- function()
{
  ## Load and prepare the dataset
  df <- rsvma.load_prepared_master_dataset_dev()
  # Define a short string to describe the dataset to facilitate comparison of
  # figures and other analysis output
  data_desc <- sprintf('%dx%d', length(unique(df$Name)), nrow(df))
  outfilepath <- function(name, suffix) {
    sprintf('../out/dev__%s__%s.%s', name, data_desc, suffix)
  }
  ## Generate table of basic statistics
  stats <- rsvma.compute_descriptive_stats(subset(df, select=c("MarketValue_lag1", "PIH_lag1", "AcquisitionCount")), outfilepath)
  ## Form and analyze the different linear models
  rsvma.run_primary_lms_dev(df, outfilepath)
}

# EOF