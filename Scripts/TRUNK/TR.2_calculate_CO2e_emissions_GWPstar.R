rm(list=ls())
print("Calculating CO2-equivalent emissions based on Global Warming Potential (100 year time horizon)")

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
#### Prep ####
#//////////////////////////////////////////////////////////////////////////////#

# Set Output Directory
INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/EMISSIONS/")

#//////////////////////////////////////////////////////////////////////////////#
#### LOAD DATASETS ####
#//////////////////////////////////////////////////////////////////////////////#

df_EMISSIONS_CUMULATIVE <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_merged_input.csv"))

#//////////////////////////////////////////////////////////////////////////////#
#### CALCULATE CO2e for N2O USING GWP100 APPROACH ####
#//////////////////////////////////////////////////////////////////////////////#

# Multiply cumulative emissions by the GWP100 of the gas

mask <- df_EMISSIONS_CUMULATIVE$Gas == "N2O"
df_EMISSIONS_CUMULATIVE$Data[mask] <- (df_EMISSIONS_CUMULATIVE$Data[mask] * 
                                         N2O_to_CO2e_GWP100 / 
                                         1e3) # Incluces Tg to Pg conversion
df_EMISSIONS_CUMULATIVE$Unit[mask] <- "PgCO2e"

#//////////////////////////////////////////////////////////////////////////////#
#### CALCULATE CO2e for METHANE USING GWP* APPROACH ####
#//////////////////////////////////////////////////////////////////////////////#

##### METHANE: Separate the gas from the input dataframe #####
df_CH4_CUMULATIVE <- df_EMISSIONS_CUMULATIVE %>% 
  subset(Gas == "CH4" & Component != "Total")
  
df_EMISSIONS_CUMULATIVE <- df_EMISSIONS_CUMULATIVE %>% 
  subset(Gas != "CH4")

##### METHANE: Convert Fossil CH4 to CO2e  #####

df_CH4_CUMULATIVE$GWP100[df_CH4_CUMULATIVE$Component == "Fossil"] <- CH4_to_CO2e_GWP100_FOS
df_CH4_CUMULATIVE$GWP100[df_CH4_CUMULATIVE$Component == "LULUCF"] <- CH4_to_CO2e_GWP100_LULUCF

df_CH4_CUMULATIVE$Data_A <- df_CH4_CUMULATIVE$Data
df_CH4_CUMULATIVE$Data_B <- NA

for (row in seq(1,nrow(df_CH4_CUMULATIVE))) {
  Year = df_CH4_CUMULATIVE$Year[row]
  if (Year >= ref_year) {
    
    ISO3 = df_CH4_CUMULATIVE$ISO3[row]
    Component = df_CH4_CUMULATIVE$Component[row]
    
    Em_0 <- df_CH4_CUMULATIVE$Data[df_CH4_CUMULATIVE$Year == (Year - 20) & 
                                     df_CH4_CUMULATIVE$ISO3 == ISO3 &
                                     df_CH4_CUMULATIVE$Component == Component]
    Em_1 <- df_CH4_CUMULATIVE$Data[row]
    
    if (length(Em_0) != 0) {
      df_CH4_CUMULATIVE$Data_B[row] <- Em_1 - Em_0
    }
  }
}

# Remove years 1850-1869 as estimates of CO2 under GWP* cannot be made for the 
# first 20 years of the time series (data for t-20 years is needed).
df_CH4_CUMULATIVE <- df_CH4_CUMULATIVE %>%
  subset(Year >= ref_year)

# Set constants for the GWP100 calculations
# see https://www.ipcc.ch/report/ar6/wg1/downloads/report/IPCC_AR6_WGI_Chapter_07.pdf 
# see https://www.nature.com/articles/s41612-021-00169-8
g = 1.13 
s = 0.25

df_CH4_CUMULATIVE$Data_A <- g * s * df_CH4_CUMULATIVE$Data_A * df_CH4_CUMULATIVE$GWP100 
df_CH4_CUMULATIVE$Data_B <- g * (1-s) * 100 * df_CH4_CUMULATIVE$Data_B * df_CH4_CUMULATIVE$GWP100 / 20

df_CH4_CUMULATIVE$Data <- ((df_CH4_CUMULATIVE$Data_A + df_CH4_CUMULATIVE$Data_B) 
                           / 1e3) # Incluces Tg to Pg conversion
df_CH4_CUMULATIVE <- df_CH4_CUMULATIVE %>% select(-c(Data_A, Data_B, GWP100))

df_CH4_CUMULATIVE$Unit <- "PgCO2e"

##### METHANE: Sum CO2e emissions across components (Fossil + LULUCF) #####

temp <- df_CH4_CUMULATIVE %>%
  group_by(CNTR_NAME, Year, ISO3, Gas, Unit) %>%
  summarise(Data = sum(Data)) %>%
  ungroup()
temp$Component <- "Total"
df_CH4_CUMULATIVE <- rbind(df_CH4_CUMULATIVE, temp)

##### METHANE: Recombine CH4 emissions with the input dataframe  #####

df_EMISSIONS_CUMULATIVE <- rbind(df_EMISSIONS_CUMULATIVE, df_CH4_CUMULATIVE)
rm(temp, df_CH4_CUMULATIVE)

#//////////////////////////////////////////////////////////////////////////////#
#### EXPRESS RELATIVE TO A REFERENCE YEAR ####
#//////////////////////////////////////////////////////////////////////////////#

# Up to this point, cumulative emissions have been calculated from the reference 
# year 1850. For N2O, cumulative emissions have been converted to PgCO2e using
# GWP100. For CO2, no conversion to CO2e needed. For CH4, emissions have been 
# converted to PgCO2e using the GWP* including all emissions since 1850, but the
# earliest possible estimate is 1870. 
# Summing the PgCO2e of CH4, N2O and CO2 does not work at present because no CH4
# estimates exist during 1850-1869. Hence we need to re-base all estimates of 
# PgCO2e to year 1870.

# Express emissions relative to ref_year

df_EMISSIONS_CUMULATIVE.ref_year <- subset(df_EMISSIONS_CUMULATIVE, Year == ref_year)
df_EMISSIONS_CUMULATIVE <- subset(df_EMISSIONS_CUMULATIVE, Year >= ref_year + 1)

df_EMISSIONS_CUMULATIVE <- merge(df_EMISSIONS_CUMULATIVE, df_EMISSIONS_CUMULATIVE.ref_year,
                                 by = c("CNTR_NAME", "ISO3", "Gas", "Unit", "Component"),
                                 suffixes = c("", "_REF"),
                                 all.x = TRUE)

df_EMISSIONS_CUMULATIVE$Data <- df_EMISSIONS_CUMULATIVE$Data - df_EMISSIONS_CUMULATIVE$Data_REF

df_EMISSIONS_CUMULATIVE <- df_EMISSIONS_CUMULATIVE %>% select(-c(Year_REF, Data_REF))
rm(df_EMISSIONS_CUMULATIVE.ref_year)

#//////////////////////////////////////////////////////////////////////////////#
#### SUM ACROSS GASES ####
#//////////////////////////////////////////////////////////////////////////////#

df_EMISSIONS_CUMULATIVE.3GHG <- df_EMISSIONS_CUMULATIVE %>%
  group_by(Year, ISO3, CNTR_NAME, Component) %>%
  summarise(Data = sum(Data, na.rm = T)) %>%
  ungroup()

# Set gas and unit labels
df_EMISSIONS_CUMULATIVE.3GHG$Gas = "3-GHG"
df_EMISSIONS_CUMULATIVE.3GHG$Unit = "PgCO2e"

# Subset to common period
df_EMISSIONS_CUMULATIVE.3GHG <- df_EMISSIONS_CUMULATIVE.3GHG %>%
  subset(Year <= final_year)

# Merge totals with individual gases
df_EMISSIONS_CUMULATIVE <- rbind(df_EMISSIONS_CUMULATIVE, df_EMISSIONS_CUMULATIVE.3GHG)
rm(df_EMISSIONS_CUMULATIVE.3GHG)

#//////////////////////////////////////////////////////////////////////////////#
#### Save  ####
#//////////////////////////////////////////////////////////////////////////////#

# Record all units as PgCO2e
df_EMISSIONS_CUMULATIVE$Unit = "PgCO2e"

write.csv(df_EMISSIONS_CUMULATIVE,
          paste0(OUTDIR, "EMISSIONS_CUMULATIVE_CO2e100_GWPstar.csv"),
          row.names = F)
