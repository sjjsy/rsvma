##########################################################
#
# Import & Merge Functions
#
# This script includes all of the steps required to go from initial source data to one final
# R data frame which contains the relevant rows and columns needed for analysis.
#
##########################################################

##########################################################
# Setup
##########################################################
rsvma.setup <- function() 
{
	# Install & load dependencies.
	if( !require(zoo) )
	{
		install.packages("zoo")
		library(zoo)
	}
}

##########################################################
# Compustat index constituents
##########################################################
rsvma.load_targets <- function()
{
	msg <- paste("WRDS: Compustat Daily Updates - Index Constituents",
	"https://wrds-web.wharton.upenn.edu/wrds/ds/comp/index/constituents.cfm",
	"Data Date - Date range 2005-01 - 2015-12",
	"Selected: CONM, SPII, SPMI, TIC, CONM, co_TIC, CUSIP, CIK, SIC, NAICS",
	"Output .csv, compression type .zip, date format YYMMDDn8",
	"Filename: ../data/compustat_index_constituents.csv\n", sep = '\n' )
	cat(msg)
	readline( prompt="Execute the WRDS query above then press enter when ready to continue... ")
	selected_firms <- read.csv("../data/compustat_index_constituents.csv")

	# Convert dates.
	selected_firms$from <- as.Date(as.character(selected_firms$from), format="%Y%m%d")
	selected_firms$thru <- as.Date(as.character(selected_firms$thru), format="%Y%m%d")

	# Filter to S&P 100 firms that were active throughout the time range (n=49).
	selected_firms <- selected_firms[selected_firms$conm == "S&P 100-Ltd" & selected_firms$from <= as.Date("2004-12-31") & is.na(selected_firms$thru),]

	# Drop unnecessary columns.
	selected_firms <- subset(selected_firms, select = -c(gvkeyx,thru,conm,tic,spii,spmi))

	# Rename columns.
	names(selected_firms)[names(selected_firms) == 'from'] <- 'SP100_From'
	names(selected_firms)[names(selected_firms) == 'co_conm'] <- 'Name'
	names(selected_firms)[names(selected_firms) == 'co_tic'] <- 'Ticker'
	names(selected_firms)[names(selected_firms) == 'co_cusip'] <- 'CUSIP'
	names(selected_firms)[names(selected_firms) == 'co_cik'] <- 'CIK'
	names(selected_firms)[names(selected_firms) == 'co_sic'] <- 'SIC'
	names(selected_firms)[names(selected_firms) == 'co_naics'] <- 'NAICS'

	# Dump the selected GVKEY identifiers to a text file for use with the WRDS query system.
	write(selected_firms[['gvkey']], file = '../data/selected_dump.txt', ncolumns = 1)
	
	selected_firms
}

##########################################################
# Compustat quarterly fundamentals
##########################################################
rsvma.load_fundamentals <- function(dataset)
{
	msg <- paste("WRDS: Compustat Daily Updates - Fundamentals Quarterly",
	"https://wrds-web.wharton.upenn.edu/wrds/ds/compd/fundq/index.cfm",
	"Data Date - Date range 2005-01 - 2015-12",
	"GVKEY, upload a plain text file - ../data/selected_dump.txt (automatically created by previous step)",
	"Selected: MKVALT, NIQ (Net Income), ATQ (Total Assets)",
	"Output .csv, compression type .zip, date format YYMMDDn8",
	"Filename: ../data/compustat_selected_fundamentals.csv\n", sep = '\n' )
	cat(msg)
	readline( prompt="Execute the WRDS query above then press enter when ready to continue... ")
	selected_firms_fundamentals <- read.csv("../data/compustat_selected_fundamentals.csv")

	# Join the two tables.
	dataset <- merge(dataset, selected_firms_fundamentals, by = "gvkey")
	rm(selected_firms_fundamentals)

	# Compute ROA as Net Income / Total Assets.
	dataset$roa <- dataset$niq / dataset$atq

	# Rearrange columns and drop unnecessary ones.
	dataset <- subset(dataset, select = c(gvkey,CUSIP,CIK,Ticker,Name,SIC,NAICS,datacqtr,mkvaltq,roa))

	names(dataset)[names(dataset) == 'datacqtr'] <- 'Quarter'
	names(dataset)[names(dataset) == 'mkvaltq'] <- 'MarketValue'
	names(dataset)[names(dataset) == 'roa'] <- 'ROA'

	# Create a temporary column in the main table with the truncated CUSIP identifier that we will have to join on.
	dataset$CUSIPShort <- substr(dataset$CUSIP, 0, 6)
	
	dataset
}

##########################################################
# Thomson Reuters M&A Data
##########################################################
rsvma.load_madata <- function(dataset)
{
	# Use Excel or another tool to dump XLSB file to a CSV file named ma_deal_data.csv.
	# NOTE: The headers probably need to be manually fixed up since there is an extra first row and newlines in many of the values.
	cat("Dump Thomson Reuters XLSB data provided during course to CSV\nFilename: ../data/ma_deal_data.csv\n")
	readline( prompt="Press enter when ready to continue... ")
	cat("Reading M&A CSV, this might take a couple of minutes...\n")
	ma_data <- read.csv("../data/ma_deal_data.csv")

	# Filter to only the acquiror CUSIP values that exist in our selected subset.
	# NOTE: Truncate to the first 6 characters of the the CUSIP since that's as much detail as the M&A data contains.
	ma_data <- ma_data[ma_data$Acquiror.Ultimate.Parent.CUSIP %in% dataset$CUSIPShort,]

	# Filter out deals where the target and acquiror are the same (e.g. stock repurchases).
	# See X.Deal.Number 1634239020 (TXN, 2005) as an example.
	ma_data <- ma_data[as.character(ma_data$Acquiror.Ultimate.Parent.CUSIP) != as.character(ma_data$X.Target.Immediate.Parent.CUSIP),]

	# Rearrange columns and drop unnecessary ones.
	ma_data <- subset(ma_data, select = c(Acquiror.Ultimate.Parent.CUSIP,Acquiror.Ultimate.Parent,Date.Announced,Date.Effective,Date.Withdrawn,Status,Value.of.Transaction...mil.,Target.Name,Target.NAIC.Code,Synopsis))

	# Convert dates to match the quarter format of the Compustat data set.
	ma_data$Date.Announced <- format(as.yearqtr(ma_data$Date.Announced, "%m/%d/%y"), "%YQ%q")
	ma_data$Date.Effective <- format(as.yearqtr(ma_data$Date.Effective, "%m/%d/%y"), "%YQ%q")
	ma_data$Date.Withdrawn <- format(as.yearqtr(ma_data$Date.Withdrawn, "%m/%d/%y"), "%YQ%q")

	# Aggregate count by ultimate parent CUSIP and quarter announced.
	ma_data_agg_count <- aggregate(ma_data, list(CUSIPShort=ma_data$Acquiror.Ultimate.Parent.CUSIP,Quarter=ma_data$Date.Announced), length)

	# Cleanup columns.
	ma_data_agg_count <- subset(ma_data_agg_count, select = c(CUSIPShort,Quarter,Acquiror.Ultimate.Parent.CUSIP))
	names(ma_data_agg_count)[names(ma_data_agg_count) == 'Acquiror.Ultimate.Parent.CUSIP'] <- 'AcquisitionCount'

	# Join back to the main table and remove this temporary aggregation.
	dataset <- merge(dataset, ma_data_agg_count, by = c("CUSIPShort","Quarter") )
	rm(ma_data_agg_count)

	# Aggregate total value by ultimate parent CUSIP and quarter announced.
	# NOTE: Many deals are missing the total value!
	ma_data_agg_sum <- subset(ma_data, select = c(Acquiror.Ultimate.Parent.CUSIP,Date.Announced,Value.of.Transaction...mil.))
	names(ma_data_agg_sum)[names(ma_data_agg_sum) == 'Value.of.Transaction...mil.'] <- 'AcquisitionTotalValue'
	ma_data_agg_sum$AcquisitionTotalValue <- as.numeric(as.character(ma_data_agg_sum$AcquisitionTotalValue))
	ma_data_agg_sum <- aggregate(ma_data_agg_sum$AcquisitionTotalValue, list(CUSIPShort=ma_data_agg_sum$Acquiror.Ultimate.Parent.CUSIP,Quarter=ma_data_agg_sum$Date.Announced), sum, na.rm=TRUE)

	# Cleanup columns.
	names(ma_data_agg_sum)[names(ma_data_agg_sum) == 'x'] <- 'AcquisitionTotalValue'

	# Join back to the main table and remove this temporary aggregation.
	dataset <- merge(dataset, ma_data_agg_sum, by = c("CUSIPShort","Quarter") )
	rm(ma_data_agg_sum)
	
	dataset
}