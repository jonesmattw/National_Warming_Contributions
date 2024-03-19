rm(list=ls())
print("Calculating transient climate response to emissions")

library(stringr)
library(tidyr)
library(dplyr)

####
# Force the working directory to ROOT as defined in SETUP.R
####

wd_parent <- str_split(getwd(), "/")[[1]]
wd_parent <- wd_parent[length(wd_parent)]
while (!(wd_parent == "National_Warming_Contributions")){
  setwd("..")
  wd_parent <- str_split(getwd(), "/")[[1]]
  wd_parent <- wd_parent[length(wd_parent)]
}
source("./Scripts/SETUP.R")
setwd(ROOT)

#//////////////////////////////////////////////////////////////////////////////#
#### Prep ####
#//////////////////////////////////////////////////////////////////////////////#

# Set Output Directory
INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/GMST/")
if (!dir.exists(OUTDIR)) {
  dir.create(OUTDIR, recursive=TRUE)
}

#//////////////////////////////////////////////////////////////////////////////#
#### LOAD EMISSIONS DATA ####
#//////////////////////////////////////////////////////////////////////////////#

df_EMISSIONS_CUMULATIVE <- read.csv(paste0(INDIR, "/EMISSIONS_CUMULATIVE_ref", ref_year, "_CO2e100_GWPstar.csv"))

#//////////////////////////////////////////////////////////////////////////////#
#### CALCULATE GMST RESPONSE ####
#//////////////////////////////////////////////////////////////////////////////#

df_GMST <- df_EMISSIONS_CUMULATIVE

# Apply factor to convert to TCRE
# (Convert from PgCO2-equivalent to PgC-equivalent units in the process)
df_GMST$Data <- df_GMST$Data / 3.664 * TCRE

# Log correct units
df_GMST$Unit = "°C"

#//////////////////////////////////////////////////////////////////////////////#
#### Save ####
#//////////////////////////////////////////////////////////////////////////////#

write.csv(df_GMST,
          paste0(OUTDIR, "GMST_ref", ref_year, ".csv"),
          row.names = F)
