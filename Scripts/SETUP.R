# Set the root directory for the project
ROOT <- "/Volumes/LaCie8/National_Warming_Contributions/"

# Set the start year 
start_year <- 1850
ref_year <- start_year + 20 # 20 prior years required for the GWP* calculation for methane
final_year <- 2021

# Set the TCRE value (express in unit °C per PgC)
# e.g. 1.65°C per 1000 PgC = 1.65 e-3 °C per PgC
TCRE = 1.65 / 1000 # Source: IPCC AR6

# Set GWP100 values (from IPCC AR6)
CH4_to_CO2e_GWP100_FOS = 29.8 # Source: IPCC AR6
CH4_to_CO2e_GWP100_LULUCF = 27.2 # Source: IPCC AR6
N2O_to_CO2e_GWP100 = 273 # Source: IPCC AR6

# Set groups to include in the output
# must be present as a tab name in the spreadsheet "Input/COUNTRY_GROUPINGS.xlsx"
GROUPS <- c(
  "EU27",
  "ANNEXII",
  "ANNEXI",
  "NONANNEX",
  "LDC",
  "BASIC",
  "EIT",
  "LMDC",
  "OECD"
)