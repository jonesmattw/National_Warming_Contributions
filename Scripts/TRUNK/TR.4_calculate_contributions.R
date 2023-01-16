rm(list=ls())
print("Calculating regional contributions to global GMST")

library(stringr)
library(tidyr)
library(dplyr)

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

#//////////////////////////////////////////////////////////////////////////////#
#### GMST data ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Output/GMST/")
OUTDIR <- paste0(ROOT, "Output/GMST/")

df_GMST <- read.csv(paste0(INDIR, "GMST_ref", ref_year, ".csv"))

df_GMST.GLOBAL <- df_GMST %>% subset(ISO3 == "GLOBAL")

df_GMST <- merge(df_GMST, df_GMST.GLOBAL,
                  by = c("Year", "Gas", "Unit", "Component"),
                  suffixes = c("", "_REF"),
                  all.x = TRUE)

df_GMST$Data_fraction <- df_GMST$Data / df_GMST$Data_REF
rm(df_GMST.GLOBAL)

df_GMST$Data_fraction[abs(df_GMST$Data_fraction) %in% c(Inf, -Inf, NA, NaN)] <- NA

df_GMST <- df_GMST %>% 
  select(-c(CNTR_NAME_REF, ISO3_REF, Data_REF)) %>%
  rename(Data_degrees=Data)

## SAVE ##

write.csv(df_GMST,
          paste0(OUTDIR, "GMST_contributions_ref", ref_year, ".csv"),
          row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### CUMULATIVE EMISSIONS - Native Units ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/EMISSIONS/")

df_EMISSIONS_CUMULATIVE <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_ref", ref_year, ".csv"))

df_EMISSIONS_CUMULATIVE.GLOBAL <- df_EMISSIONS_CUMULATIVE %>% subset(ISO3 == "GLOBAL")

df_EMISSIONS_CUMULATIVE <- merge(df_EMISSIONS_CUMULATIVE, df_EMISSIONS_CUMULATIVE.GLOBAL,
                 by = c("Year", "Gas", "Unit", "Component"),
                 suffixes = c("", "_REF"),
                 all.x = TRUE)

df_EMISSIONS_CUMULATIVE$Data_fraction <- df_EMISSIONS_CUMULATIVE$Data / df_EMISSIONS_CUMULATIVE$Data_REF
rm(df_EMISSIONS_CUMULATIVE.GLOBAL)

df_EMISSIONS_CUMULATIVE$Data_fraction[abs(df_EMISSIONS_CUMULATIVE$Data_fraction) %in% c(Inf, -Inf, NA, NaN)] <- NA

df_EMISSIONS_CUMULATIVE <- df_EMISSIONS_CUMULATIVE %>% 
  select(-c(CNTR_NAME_REF, ISO3_REF, Data_REF))

## SAVE ##

write.csv(df_EMISSIONS_CUMULATIVE,
          paste0(OUTDIR, "EMISSIONS_CUMULATIVE_ref", ref_year, "_contributions.csv"),
          row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### CUMULATIVE EMISSIONS - CO2-equivalent ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/EMISSIONS/")

df_EMISSIONS_CO2e <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_CO2e100_GWPstar.csv"))

df_EMISSIONS_CO2e.GLOBAL <- df_EMISSIONS_CO2e %>% subset(ISO3 == "GLOBAL")

df_EMISSIONS_CO2e <- merge(df_EMISSIONS_CO2e, df_EMISSIONS_CO2e.GLOBAL,
                           by = c("Year", "Gas", "Unit", "Component"),
                           suffixes = c("", "_REF"),
                           all.x = TRUE)

df_EMISSIONS_CO2e$Data_fraction <- df_EMISSIONS_CO2e$Data / df_EMISSIONS_CO2e$Data_REF
rm(df_EMISSIONS_CO2e.GLOBAL)

df_EMISSIONS_CO2e$Data_fraction[abs(df_EMISSIONS_CO2e$Data_fraction) %in% c(Inf, -Inf, NA, NaN)] <- NA

df_EMISSIONS_CO2e <- df_EMISSIONS_CO2e %>% 
  select(-c(CNTR_NAME_REF, ISO3_REF, Data_REF))

## SAVE ##

write.csv(df_EMISSIONS_CO2e,
          paste0(OUTDIR, "EMISSIONS_CUMULATIVE_CO2e100_GWPstar_contributions.csv"),
          row.names = F)

