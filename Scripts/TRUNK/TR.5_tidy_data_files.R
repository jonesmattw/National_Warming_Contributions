rm(list=ls())
print("Export public data")

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

source(paste0(ROOT, "Scripts/HELPERS.R"))

#//////////////////////////////////////////////////////////////////////////////#
#### Emissions Data ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/PUBLIC_DATA/")

df <- read.csv(paste0(INDIR, "EMISSIONS_merged_input.csv"))

df$Gas[df$Gas == "CO2"] <- CO2_expression
df$Gas[df$Gas == "CH4"] <- CH4_expression
df$Gas[df$Gas == "N2O"]  <- N2O_expression
df$Gas[df$Gas == "3-GHG"]  <- GHG3_expression

df$Unit <- df_UNITS$Unit[match(df$Gas, df_UNITS$Gas)]

df <- df %>% 
  select(CNTR_NAME, ISO3, Gas, Component, Year, Data, Unit) %>%
  arrange(CNTR_NAME, ISO3, Gas, Component, Year)

write.csv(df,
          paste0(paste0(OUTDIR, "EMISSIONS_ANNUAL_", as.character(start_year), "-", as.character(final_year), ".csv")),
          row.names = F)

# #//////////////////////////////////////////////////////////////////////////////#
# #### Cumulative Emissions Data ####
# #//////////////////////////////////////////////////////////////////////////////#
# 
# INDIR <- paste0(ROOT, "Output/EMISSIONS/")
# OUTDIR <- paste0(ROOT, "Output/PUBLIC_DATA/")
# 
# df <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_ref", ref_year, ".csv"))
# 
# df$Gas[df$Gas == "CO2"] <- CO2_expression
# df$Gas[df$Gas == "CH4"] <- CH4_expression
# df$Gas[df$Gas == "N2O"]  <- N2O_expression
# df$Gas[df$Gas == "3-GHG"]  <- GHG3_expression
# 
# df$Unit <- df_UNITS$Unit_CUMULATIVE[match(df$Gas, df_UNITS$Gas)]
# 
# df <- df %>% 
#   subset(Year >= ref_year) %>%
#   select(CNTR_NAME, ISO3, Gas, Component, Year, Data, Unit) %>%
#   arrange(CNTR_NAME, ISO3, Gas, Component, Year)
# 
# write.csv(df,
#           paste0(paste0(OUTDIR, "CUMULATIVE_EMISSIONS_", as.character(ref_year+1), "-", as.character(final_year), ".csv")),
#           row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### PgCO2e Cumulative Emissions Data ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/PUBLIC_DATA/")

df <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_CO2e100_GWPstar.csv"))

df$Gas[df$Gas == "CO2"] <- CO2_expression
df$Gas[df$Gas == "CH4"] <- CH4_expression
df$Gas[df$Gas == "N2O"]  <- N2O_expression
df$Gas[df$Gas == "3-GHG"]  <- GHG3_expression

df$Unit <- df_UNITS$Unit_CUMULATIVE[df_UNITS$Gas == "3-GHG"]

df <- df %>% 
  subset(Year >= ref_year) %>%
  select(CNTR_NAME, ISO3, Gas, Component, Year, Data, Unit) %>%
  arrange(CNTR_NAME, ISO3, Gas, Component, Year)

write.csv(df,
          paste0(paste0(OUTDIR, "EMISSIONS_CUMULATIVE_CO2e100_", as.character(ref_year+1), "-", as.character(final_year), ".csv")),
          row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### GMST ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Output/GMST/")
OUTDIR <- paste0(ROOT, "Output/PUBLIC_DATA/")

df <- read.csv(paste0(INDIR, "GMST_ref", ref_year,".csv"))

df$Gas[df$Gas == "CO2"] <- CO2_expression
df$Gas[df$Gas == "CH4"] <- CH4_expression
df$Gas[df$Gas == "N2O"]  <- N2O_expression
df$Gas[df$Gas == "3-GHG"] <- GHG3_expression

df <- df %>%
  subset(Year >= ref_year) %>%
  select(CNTR_NAME, ISO3, Gas, Component, Year, Data, Unit) %>%
  arrange(CNTR_NAME, ISO3, Gas, Component, Year)

write.csv(df,
          paste0(paste0(OUTDIR, "GMST_response_", as.character(ref_year+1), "-", as.character(final_year), ".csv")),
          row.names = F)

# #//////////////////////////////////////////////////////////////////////////////#
# #### GMST Contributions ####
# #//////////////////////////////////////////////////////////////////////////////#
# 
# INDIR <- paste0(ROOT, "Output/GMST/")
# OUTDIR <- paste0(ROOT, "Output/PUBLIC_DATA/")
# 
# df <- read.csv(paste0(INDIR, "GMST_contributions_ref", ref_year,".csv"))
# 
# df$Gas[df$Gas == "CO2"] <- CO2_expression
# df$Gas[df$Gas == "CH4"] <- CH4_expression
# df$Gas[df$Gas == "N2O"]  <- N2O_expression
# df$Gas[df$Gas == "3-GHG"] <- GHG3_expression
# 
# df$Data <- df$Data_fraction * 100
# df$Unit <- "%"
# 
# df <- df %>% 
#   subset(Year >= ref_year) %>%
#   select(-c(Data_degrees, Data_fraction)) %>%
#   select(CNTR_NAME, ISO3, Gas, Component, Year, Data, Unit) %>%
#   arrange(CNTR_NAME, ISO3, Gas, Component, Year)
# 
# write.csv(df,
#           paste0(paste0(OUTDIR, "GMST_response_contributions_", as.character(ref_year+1), "-", as.character(final_year), ".csv")),
#           row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### COPY COUNTRY GROUPINGS TO PUBLIC DATA DIR ####
#//////////////////////////////////////////////////////////////////////////////#

INDIR <- paste0(ROOT, "Input/")
OUTDIR <- paste0(ROOT, "Output/PUBLIC_DATA/")

in_file <- "COUNTRY_GROUPINGS.xlsx"
out_file <- "COUNTRY_GROUPINGS.xlsx"

file.copy(paste0(INDIR, in_file), 
          paste0(OUTDIR, out_file),
          overwrite = TRUE)
