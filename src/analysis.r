#### Data analysis

# This file includes all the quantitative analysis that was done to provide the
# results for our research report. Other scripts were used to produce the
# master dataset required by this script.

### Read the data
source("analysis_funcs.r")

rsvma.analysis_main <- function()
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

# EOF