##########################################################
#
# Import & Merge Functions
#
# This script includes all of the steps required to go from initial source data to one final
# R data frame which contains the relevant rows and columns needed for analysis.
#
# If you want to inspect one of the intermediate tables, setting a breakpoint is the easiest way.
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

	# Install & load dependencies.
	if( !require(dplyr) )
	{
		install.packages("dplyr")
		library(dplyr)
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
	readline( prompt="Execute the WRDS query above and save the file as specified then press enter when ready to continue... ")
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

	# Dump the selected identifiers to a text file for use with the WRDS query system.
	write(as.character(selected_firms[['gvkey']]), file = '../data/selected_dump_gvkey.txt', ncolumns = 1)
	write(as.character(selected_firms[['CUSIP']]), file = '../data/selected_dump_cusip.txt', ncolumns = 1)
	
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
	"GVKEY, upload a plain text file - ../data/selected_dump_gvkey.txt (automatically created by previous step)",
	"Selected: CSHOQ (Common Shares Outstanding), PRCCQ (Price Close - Quarter), NIQ (Net Income), ATQ (Total Assets),",
  "          ACTQ (Current Assets), LCTQ (Current Liabilities), XRDQ (R&D Expense), REVTQ (Total Revenue)",
	"Output .csv, compression type .zip, date format YYMMDDn8",
	"Filename: ../data/compustat_selected_fundamentals.csv\n", sep = '\n' )
	cat(msg)
	readline( prompt="Execute the WRDS query above and save the file as specified then press enter when ready to continue... ")
	selected_firms_fundamentals <- read.csv("../data/compustat_selected_fundamentals.csv")

	# Join the two tables.
	dataset <- merge(dataset, selected_firms_fundamentals, by = "gvkey")
	
	# Rename column.
	names(dataset)[names(dataset) == 'datacqtr'] <- 'Quarter'
	
	# Create a temporary column in the main table with the truncated CUSIP identifier that we will have to join on.
	dataset$CUSIPShort <- substr(dataset$CUSIP, 0, 6)
	
	# Truncate SIC to first 3 digits (industry group).
	dataset$SIC3 <- substr(dataset$SIC, 0, 3)
	
	# Compute market value as price at end of quarter times shares outstanding (units $M).
	# I have verified that this number is exactly the same as Compustat's MKTVALQ in 
	# all cases where both are available, and CSHOQ & PRCCQ are more often available.
	dataset$MarketValue <- dataset$cshoq * dataset$prccq
	
	# Compute ROA as Net Income / Total Assets.
	# WRDS Ref: https://www.wiwi.uni-muenster.de/uf/sites/uf/files/2017_10_12_wrds_data_items.pdf
	dataset$ROA <- dataset$niq / dataset$atq
	
	# Compute unabsorbed slack as Current Assets / Current Liabilities.
	# Ref: Iyer & Miller (2008).
	dataset$Slack <- dataset$actq / dataset$lctq
	
	# Compute R&D intensity as R&D Expense / Total Revenue.
	# The Muenster link above suggests dividing by assets instead but course staff and all other
	# sources suggest revenue or sales. Sales are not available in this Compustat data set.
	dataset$RDIntensity <- dataset$xrdq / dataset$revtq
	
	# Calculate the industry averages for ROA and Slack.
	industry_avgs <- aggregate(subset(dataset,select=c(ROA,Slack)), list(SIC3=dataset$SIC3,Quarter=dataset$Quarter), mean)
	names(industry_avgs)[names(industry_avgs) == 'ROA'] <- 'ROA_IndAvg'
	names(industry_avgs)[names(industry_avgs) == 'Slack'] <- 'Slack_IndAvg'
	dataset <- merge( x = dataset, y = industry_avgs, by = c("SIC3","Quarter"), all.x = TRUE )
	
	# Create columns for relative ROA and Slack.
	dataset$ROA_Rel <- dataset$ROA / dataset$ROA_IndAvg
	dataset$Slack_Rel <- dataset$Slack / dataset$Slack_IndAvg

	# Rearrange columns and drop unnecessary ones.
	dataset <- subset(dataset, select = c(gvkey,CUSIP,CUSIPShort,CIK,Ticker,Name,SIC,SIC3,NAICS,Quarter,MarketValue,
	                                      ROA,ROA_IndAvg,ROA_Rel,Slack,Slack_IndAvg,Slack_Rel,RDIntensity))
	
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

	# Join back to the main table.
	dataset <- merge( x = dataset, y = ma_data_agg_count, by = c("CUSIPShort","Quarter"), all.x = TRUE )

	# Aggregate total value by ultimate parent CUSIP and quarter announced.
	# NOTE: Many deals are missing the total value!
	ma_data_agg_sum <- subset(ma_data, select = c(Acquiror.Ultimate.Parent.CUSIP,Date.Announced,Value.of.Transaction...mil.))
	names(ma_data_agg_sum)[names(ma_data_agg_sum) == 'Value.of.Transaction...mil.'] <- 'AcquisitionTotalValue'
	ma_data_agg_sum$AcquisitionTotalValue <- suppressWarnings(as.numeric(as.character(ma_data_agg_sum$AcquisitionTotalValue)))
	ma_data_agg_sum <- aggregate(ma_data_agg_sum$AcquisitionTotalValue, list(CUSIPShort=ma_data_agg_sum$Acquiror.Ultimate.Parent.CUSIP,Quarter=ma_data_agg_sum$Date.Announced), sum, na.rm=TRUE)

	# Cleanup columns.
	names(ma_data_agg_sum)[names(ma_data_agg_sum) == 'x'] <- 'AcquisitionTotalValue'

	# Join back to the main table.
	dataset <- merge( x = dataset, y = ma_data_agg_sum, by = c("CUSIPShort","Quarter"), all.x = TRUE )
	
	dataset
}

##########################################################
# Institutional Shareholder Data (Thomson Reuters 13F & Brian Bushee classifications)
##########################################################
rsvma.load_instdata<- function(dataset)
{
	msg <- paste("WRDS: Thomson Reuters Institutional (13f) Holdings - s34 Master File",
		"https://wrds-web.wharton.upenn.edu/wrds/ds/tfn/sp34/index.cfm",
		"Data Date - Date range 2005-01 - 2015-12",
		"CUSIP, upload a plain text file - ../data/selected_dump_cusip.txt (automatically created by first step)",
		"Selected: MGRNAME TYPECODE FDATE RDATE CUSIP SHARES NO SHARED SOLE CHANGE PRC SHROUT1",
		"Output .csv, compression type .zip, date format YYMMDDn8",
		"Filename: ../data/tr_13f_holdings.csv\n", sep = '\n' )
	cat(msg)
	readline( prompt="Execute the WRDS query above and save the file as specified then press enter when ready to continue... ")
	selected_firms_13f <- read.csv("../data/tr_13f_holdings.csv")
	
	cat("Ensure that Bushee's classification data is in place\nhttp://acct.wharton.upenn.edu/faculty/bushee/IIclass.html\nFilename: ../data/bushee_classifications.csv\n")
	readline( prompt="Press enter when ready to continue... ")
	bushee_classifications <- read.table("../data/bushee_classifications.csv", na.strings=".")
	colnames(bushee_classifications) <- c('mgrno','mgrno_ver','permkey','year','type','dedclass','dedclass_perm','invstyle','invstyle_perm','growstyle','growstyle_perm','taxsens','taxsens_ext')
	
	# Filter to dedicated shareholders between 2005 and 2015.
	# LIMITATION: If a shareholder is not present in Bushee's classification data then we do not consider it, regardless of its actual behavior.
	bushee_classifications <- bushee_classifications[(!is.na(bushee_classifications$dedclass) | !is.na(bushee_classifications$dedclass_perm)) & (bushee_classifications$dedclass_perm == "DED" | (is.na(bushee_classifications$dedclass_perm) & bushee_classifications$dedclass == "DED")),]
	bushee_classifications <- bushee_classifications[bushee_classifications$year >= 2005 & bushee_classifications$year <= 2015,]
	
	# Filter the 13F data to include only the previously specified shareholders from Bushee's data.
	selected_firms_13f <- selected_firms_13f[selected_firms_13f$mgrno %in% bushee_classifications$mgrno,]
	
	# Format quarterly dates to match the master dataset.
	selected_firms_13f$rdate <- format(as.yearqtr(as.character(selected_firms_13f$rdate), "%Y%m%d"), "%YQ%q")
	
	# Aggregate the total number of shares and the average number of shares outstanding by CUSIP and quarter.
	selected_firms_13f$shares <- as.numeric(as.character(selected_firms_13f$shares))
	selected_firms_13f$CUSIPShort <- substr(as.character(selected_firms_13f$cusip), 0, 6)
	selected_firms_13f_agg_sumshares <- aggregate(selected_firms_13f$shares, list(CUSIPShort=selected_firms_13f$CUSIPShort,Quarter=selected_firms_13f$rdate), sum, na.rm=TRUE)
	names(selected_firms_13f_agg_sumshares)[names(selected_firms_13f_agg_sumshares) == 'x'] <- 'TotalIH'
	selected_firms_13f_agg_avgshrout <- aggregate(1000000*selected_firms_13f$shrout1, list(CUSIPShort=selected_firms_13f$CUSIPShort,Quarter=selected_firms_13f$rdate), mean, na.rm=TRUE)
	names(selected_firms_13f_agg_avgshrout)[names(selected_firms_13f_agg_avgshrout) == 'x'] <- 'AvgShrOut'
	
	# Calculate percentage of institutional holdings (PIH).
	selected_firms_13f_agg <- merge( selected_firms_13f_agg_sumshares,selected_firms_13f_agg_avgshrout, by = c("CUSIPShort","Quarter") )
	selected_firms_13f_agg$PIH <- 100 * (selected_firms_13f_agg$TotalIH / selected_firms_13f_agg$AvgShrOut)
	selected_firms_13f_agg <- subset(selected_firms_13f_agg, select = c(CUSIPShort,Quarter,PIH))
	
	# Join back to the main table.
	dataset <- merge( x = dataset, y = selected_firms_13f_agg, by = c("CUSIPShort","Quarter"), all.x = TRUE )
	
	# Add 1 and 2 period lagged PIH and control variables.
	dataset <- dataset %>% group_by(CUSIPShort) %>% mutate(PIH_lag1 = lag(PIH,1,order_by = Quarter),
	                                                       MarketValue_lag1 = lag(MarketValue,1,order_by = Quarter),
	                                                       ROARel_lag1 = lag(ROA_Rel,1,order_by = Quarter),
	                                                       SlackRel_lag1 = lag(Slack_Rel,1,order_by = Quarter),
	                                                       RDIntensity_lag1 = lag(RDIntensity,1,order_by = Quarter),
	                                                       AcquisitionTotalValue_lag1 = lag(AcquisitionTotalValue,1,order_by = Quarter))
	
	dataset <- dataset %>% group_by(CUSIPShort) %>% mutate(PIH_lag2 = lag(PIH,2,order_by = Quarter),
	                                                       MarketValue_lag2 = lag(MarketValue,2,order_by = Quarter),
	                                                       ROARel_lag2 = lag(ROA_Rel,2,order_by = Quarter),
	                                                       SlackRel_lag2 = lag(Slack_Rel,2,order_by = Quarter),
	                                                       RDIntensity_lag2 = lag(RDIntensity,2,order_by = Quarter),
	                                                       AcquisitionTotalValue_lag2 = lag(AcquisitionTotalValue,2,order_by = Quarter))
	
	dataset
}

##########################################################
# Manual additions (currently only founding year).
##########################################################
rsvma.load_manualadditions<- function(dataset)
{
  msg <- paste("Manually acquired founding years for target firms",
               "Filename: ../data/founding_years.csv\n", sep = '\n' )
  cat(msg)
  readline( prompt="Place the data at the specified location and press enter to continue... ")
  founding_years <- read.csv("../data/founding_years.csv")
  
  # Join back to the main table.
  dataset <- merge( x = dataset, y = founding_years, by = c("CUSIPShort"), all.x = TRUE )
  
  current_year <- as.numeric( format( Sys.Date(), "%Y" ) )
  dataset$FirmAge <- current_year - as.numeric( dataset$YearFounded )
  
  dataset
}
	
