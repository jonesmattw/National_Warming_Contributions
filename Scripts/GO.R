rm(list=ls())

library(stringr)

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

source("../Scripts/TRUNK/TR.1_collate_emissions.R")
source("../Scripts/TRUNK/TR.2_calculate_CO2e_emissions_GWPstar.R")
source("../Scripts/TRUNK/TR.3_calculate_GMST_response.R")
source("../Scripts/TRUNK/TR.4_calculate_contributions.R")
source("../Scripts/TRUNK/TR.5_tidy_data_files.R")

source("../Scripts/PLOTTING/PL.1_emissions.R")
source("../Scripts/PLOTTING/PL.2_GMST.R")
source("../Scripts/PLOTTING/PL.3_contributions_GMST.R")
if (ref_year %in% c(1850)) {
  source("../Scripts/PLOTTING/PL.4_GMST_maps.R")
}

