#### Data analysis

# This file houses the main function that conducts the quantitative analysis
# to provide the results for our research report. There is another version
# of the function with "_dev" in the name to offer a way to experiment and
# compare the results to the actual version.

### Read the data
source("analysis_funcs.r")

rsvma.perform_analysis <- function()
{
  ## Load and prepare the dataset
  df <- rsvma.load_prepared_master_dataset()
  # Define a short string to describe the dataset to facilitate comparison of
  # figures and other analysis output
  data_desc <- sprintf('%dx%d', length(unique(df$Name)), nrow(df))
  outfilepath <- function(name, suffix) {
    sprintf('../out/dev__%s__%s.%s', name, data_desc, suffix)
  }
  ## Generate table of basic statistics
  stats <- rsvma.compute_descriptive_stats(subset(df, select=c("FirmAge", "MarketValue", "ROA_Rel", "Slack_Rel", "PIH", "AcquisitionCount")), outfilepath)
  ## Form and analyze the different linear models
  rsvma.run_primary_lms(df, outfilepath)
  ## Form and analyze the different negative binomial models
  rsvma.run_primary_nbs(df, outfilepath)
}

# Run the analysis function to produce results into the "out" folder
rsvma.analysis_final()

# EOF