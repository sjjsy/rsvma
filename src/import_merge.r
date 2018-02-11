##########################################################
#
# Import & Merge Script
#
# Usage (interactive session):
# > source("path/to/import_merge.r", chdir = TRUE)
#
# On success, final output will be available as master_dataset.
#
##########################################################

source("import_merge_funcs.r")

rsvma.main <- function()
{
	rsvma.setup()
	master_dataset <- rsvma.load_targets()
	master_dataset <- rsvma.load_fundamentals(master_dataset)
	master_dataset <- rsvma.load_madata(master_dataset)
	master_dataset <- rsvma.load_instdata(master_dataset)
	cat("Done.\n")
	master_dataset
}

master_dataset <- rsvma.main()