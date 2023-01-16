rm(list=ls())

library(stringr)

####
# Force the working directory to ROOT as defined in SETUP.R
####

wd_parent <- str_split(getwd(), "/")[[1]]
wd_parent <- wd_parent[length(wd_parent)]
if (wd_parent == "National_Warming_Contributions") {
  # if current wd is /National_Warming_Contributions/
  source("./Scripts/SETUP.R") # get ROOT
} else if (wd_parent == "Scripts") { 
  # if current wd is /National_Warming_Contributions/Scripts/
  source("./SETUP.R") # get ROOT
}
setwd(ROOT)

source("./Scripts/TRUNK/TR.1_collate_emissions.R")
source("./Scripts/TRUNK/TR.2_calculate_CO2e_emissions_GWPstar.R")
source("./Scripts/TRUNK/TR.3_calculate_GMST_response.R")
source("./Scripts/TRUNK/TR.4_calculate_contributions.R")
source("./Scripts/PLOTTING/PL.1_emissions.R")
source("./Scripts/PLOTTING/PL.2_GMST.R")
source("./Scripts/PLOTTING/PL.3_contributions_GMST.R")
source("./Scripts/PLOTTING/PL.4_GMST_maps.R")
source("./Scripts/TRUNK/TR.5_tidy_data_files.R")
