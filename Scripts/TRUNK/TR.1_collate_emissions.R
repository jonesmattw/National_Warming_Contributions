rm(list=ls())
print("Pre-processing the emissions data -- blending data sources and combining to a single .csv")

library(stringr)
library(readxl)
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
INDIR <- paste0(ROOT, "Input/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/EMISSIONS/")

# Load constants and namelists stored externally
df_GCP_CNTR_ISO3_CODES <- read_excel(paste0(ROOT, "Input/CNTR_ISO3_CODES.xlsx"), sheet = "GCP_NAMES_CODES")
df_HN_ISO3_lookup <- read_excel(paste0(ROOT, "Input/CNTR_ISO3_CODES.xlsx"), sheet = "HN_ISO3_lookup")

#//////////////////////////////////////////////////////////////////////////////#
#### Load FF CO2 data ####
#//////////////////////////////////////////////////////////////////////////////#

df_CO2ff <- read.csv(paste0(INDIR, "CO2/GCB2022/GCB2022v27_MtCO2_flat.csv"))
df_CO2ff <- df_CO2ff %>% select(c(Country, Year, Total))
colnames(df_CO2ff)[1] <- "CNTR_NAME"

# Clean up the data column
colnames(df_CO2ff)[3] <- "PgCO2"
df_CO2ff$PgCO2[is.na(df_CO2ff$PgCO2)] <- 0 
df_CO2ff$PgCO2 <- as.numeric(df_CO2ff$PgCO2) / 1000

# Clean up some country names
df_CO2ff$CNTR_NAME[df_CO2ff$CNTR_NAME == "C\xf4te d'Ivoire"] <- "Côte d'Ivoire"
df_CO2ff$CNTR_NAME[df_CO2ff$CNTR_NAME == "Cura\xe7ao"] <- "Curaçao"
df_CO2ff$CNTR_NAME[df_CO2ff$CNTR_NAME == "International Transport"] <- "Bunkers"

# Get ISO3 Code
df_CO2ff <- df_CO2ff %>% 
  inner_join(df_GCP_CNTR_ISO3_CODES, by = c("CNTR_NAME" = "CNTR_NAME")) %>%
  select(c(colnames(df_CO2ff), "ISO3"))

# Get global totals
df_CO2ff_GLOBAL <- df_CO2ff %>%
  group_by(Year) %>%
  summarise(PgCO2 = sum(PgCO2, na.rm=T)) %>%
  ungroup()
df_CO2ff_GLOBAL$ISO3 <- "GLOBAL"
df_CO2ff_GLOBAL$CNTR_NAME <- "GLOBAL"
df_CO2ff <- rbind(df_CO2ff, df_CO2ff_GLOBAL)

for (GROUP in GROUPS) {
  LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
  df_temp <- df_CO2ff %>% 
    subset(ISO3 %in% LIST) %>% 
    group_by(Year) %>%
    summarise(PgCO2 = sum(PgCO2, na.rm=T)) %>%
    ungroup()
  df_temp$ISO3 <- GROUP
  df_temp$CNTR_NAME <- GROUP
  
  df_CO2ff <- rbind(df_CO2ff, df_temp)
}

# Restrict to period 1850-onwards
df_CO2ff <- df_CO2ff %>% subset(Year >= start_year)

# Prep for export
df_CO2ff$Gas <- "CO2"
df_CO2ff$Unit <- "PgCO2"
df_CO2ff$Component <- "Fossil"
colnames(df_CO2ff)[which(colnames(df_CO2ff) == "PgCO2")] = "Data"

#//////////////////////////////////////////////////////////////////////////////#
#### Load LULUCF CO2 data ####
#//////////////////////////////////////////////////////////////////////////////#

mutual_year_min = 0

# Load BLUE
#df_CO2luc_BLUE <- read_excel(paste0(INDIR, "CO2/GCB2021/BLUE_ELUC-net-with-peat_GCB2021_countries.xlsx"))
df_CO2luc_BLUE <- read_excel(paste0(INDIR, "CO2/GCB2022/2022-10-07_GCB2022_ELUC_country-level_net-sinks-sources_BLUE_1850-2021_v3.xlsx"), sheet="net")
colnames(df_CO2luc_BLUE)[1] <- "Year"
df_CO2luc_BLUE <- df_CO2luc_BLUE %>%
  mutate_all(~replace(., is.na(.), 0)) %>% # replaces nans with zeros
  select(where( ~ is.numeric(.x) && sum(.x) != 0)) %>% # remove countries with all zeros 
  pivot_longer(cols = -Year, names_to = "ISO3", values_to = "PgCO2") %>%
  mutate(PgCO2 = PgCO2 / 1e3) %>%
  subset(!(ISO3 %in% c("v1.8", "Sum", "DIFF", "DIFF rel")))
df_CO2luc_BLUE$Model <- "BLUE"
if (min(df_CO2luc_BLUE$Year) > mutual_year_min) {
  mutual_year_min = min(df_CO2luc_BLUE$Year)
}

# Load H&N
# df_CO2luc_HN <- read_excel(paste0(INDIR, "CO2/GCB2021/HN2021_ELUC-net-with-peat_GCB2021_countries.xlsx"))
df_CO2luc_HN <- read_excel(paste0(INDIR, "CO2/GCB2022/2022-10-07_GCB2022_ELUC_country-level_net-sinks-sources_HN21_1850-2021_v3.xlsx"), sheet="net")
colnames(df_CO2luc_HN)[1] <- "Year"
df_CO2luc_HN <- df_CO2luc_HN %>%
  mutate_all(~replace(., is.na(.), 0)) %>% # replaces nans with zeros
  select(where( ~ is.numeric(.x) && sum(.x) != 0)) %>% # remove countries with all zeros 
  pivot_longer(cols = -Year, names_to = "ISO3", values_to = "PgCO2") %>%
  mutate(PgCO2 = PgCO2 / 1e3) %>%
  subset(!(ISO3 %in% c("v1.8", "Sum", "DIFF", "DIFF rel")))
df_CO2luc_HN$Model <- "H&N"
if (min(df_CO2luc_HN$Year) > mutual_year_min) {
  mutual_year_min = min(df_CO2luc_HN$Year)
}

# Load OSCAR
# df_CO2luc_OSCAR <- read_excel(paste0(INDIR, "CO2/GCB2021/OSCAR_ELUC-net-with-peat_GCB2021_IPCCcountries.xlsx"))
df_CO2luc_OSCAR <- read_excel(paste0(INDIR, "CO2/GCB2022/2022-10-07_GCB2022_ELUC_country-level_net-sinks-sources_OSCAR_1850-2021_v3.xlsx"), sheet="net")
colnames(df_CO2luc_OSCAR)[1] <- "Year"
df_CO2luc_OSCAR <- df_CO2luc_OSCAR %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% # replaces nans with zeros
  select(where( ~ is.numeric(.x) && sum(.x) != 0)) %>% # remove countries with all zeros 
  pivot_longer(cols = -Year, names_to = "ISO3", values_to = "PgCO2") %>%
  mutate(PgCO2 = PgCO2 / 1e3) %>%
  subset(!(ISO3 %in% c("v1.8", "Sum", "DIFF", "DIFF rel")))
df_CO2luc_OSCAR$Model <- "OSCAR"
if (min(df_CO2luc_OSCAR$Year) > mutual_year_min) {
  mutual_year_min = min(df_CO2luc_OSCAR$Year)
}

# Take average of BLUE, H&N and OSCAR for each country
df_CO2luc <- rbind(df_CO2luc_HN, df_CO2luc_BLUE, df_CO2luc_OSCAR)
rm(df_CO2luc_HN, df_CO2luc_BLUE, df_CO2luc_OSCAR)
df_CO2luc <- df_CO2luc %>%
  group_by(Year, ISO3) %>%
  summarise(PgCO2 = mean(PgCO2)) %>%
  ungroup()
  
df_CO2luc$PgCO2 <- df_CO2luc$PgCO2 * 3.664
df_CO2luc <- df_CO2luc %>%
  subset(Year >= mutual_year_min)

# Get CNTR NAME Code
df_CO2luc <- df_CO2luc %>% 
  inner_join(df_GCP_CNTR_ISO3_CODES, by = c("ISO3" = "ISO3")) %>%
  select(c(colnames(df_CO2luc), "CNTR_NAME"))

# Clean
df_CO2luc <- df_CO2luc %>% subset(!is.na(Year))

# Get global totals
df_CO2luc_GLOBAL <- df_CO2luc %>%
  group_by(Year) %>%
  summarise(PgCO2 = sum(PgCO2, na.rm=T)) %>%
  ungroup()
df_CO2luc_GLOBAL$ISO3 <- "GLOBAL"
df_CO2luc_GLOBAL$CNTR_NAME <- "GLOBAL"
df_CO2luc <- rbind(df_CO2luc, df_CO2luc_GLOBAL)

for (GROUP in GROUPS) {
  LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
  df_temp <- df_CO2luc %>% 
    subset(ISO3 %in% LIST) %>%
    group_by(Year) %>%
    summarise(PgCO2 = sum(PgCO2, na.rm=T)) %>%
    ungroup()
  df_temp$ISO3 <- GROUP
  df_temp$CNTR_NAME <- GROUP
  
  df_CO2luc <- rbind(df_CO2luc, df_temp)
}

# Restrict to period 1850-onwards
df_CO2luc <- df_CO2luc %>% subset(Year >= start_year)

# Prep for export
df_CO2luc$Gas <- "CO2"
df_CO2luc$Unit <- "PgCO2"
df_CO2luc$Component <- "LULUCF"
colnames(df_CO2luc)[which(colnames(df_CO2luc) == "PgCO2")] = "Data"

#//////////////////////////////////////////////////////////////////////////////#
#### Load FF CH4 data ####
#//////////////////////////////////////////////////////////////////////////////#

# df_CH4ff <- read.csv(paste0(INDIR, "CH4/Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv"))
df_CH4ff <- read.csv(paste0(INDIR, "CH4/Guetschow-et-al-2022-PRIMAP-hist_v2.4_no_rounding_11-Oct-2022.csv"))

# Select category M.0.EL ( = National Total excluding LULUCF)
# and M.AG (agriculture) emissions (which will soon be subtracted from M.0.EL)
df_CH4ff <- df_CH4ff %>% 
  rename(scenario = `scenario..PRIMAP.hist.`,
         country = `area..ISO3.`,
         category = `category..IPCC2006_PRIMAP.`) %>%
  subset(scenario == "HISTTP" & entity == "CH4" & category %in% c("M.0.EL", "M.AG")) %>%
  select(-c(source, scenario, entity, unit))

# Convert to long format
df_CH4ff <- df_CH4ff %>% 
  pivot_longer(cols = -c("country", "category"), names_to = "Year", values_to = "TgCH4")
df_CH4ff$Year <- as.numeric(substr(df_CH4ff$Year, 2, 5))

# Convert to wide format and subtract M.AG (agriculture) emissions from M.0.EL (total)
df_CH4ff <- df_CH4ff %>% 
  pivot_wider(names_from = category, values_from = TgCH4) 

df_CH4ff$TgCH4 <- df_CH4ff$M.0.EL - df_CH4ff$M.AG

df_CH4ff <- df_CH4ff %>% 
  select(-c(M.0.EL, M.AG))

# Convert from GgGAS to TgGAS
df_CH4ff$TgCH4 <- df_CH4ff$TgCH4 / 1e3

# Get ISO3 Code
colnames(df_CH4ff)[1] <- c("ISO3")
df_CH4ff <- df_CH4ff %>% 
  inner_join(df_GCP_CNTR_ISO3_CODES, by = c("ISO3" = "ISO3")) %>%
  select(c(colnames(df_CH4ff), "CNTR_NAME"))

# Get global totals
df_CH4ff_GLOBAL <- df_CH4ff %>%
  group_by(Year) %>%
  summarise(TgCH4 = sum(TgCH4, na.rm=T)) %>%
  ungroup()
df_CH4ff_GLOBAL$ISO3 <- "GLOBAL"
df_CH4ff_GLOBAL$CNTR_NAME <- "GLOBAL"
df_CH4ff <- rbind(df_CH4ff, df_CH4ff_GLOBAL)

for (GROUP in GROUPS) {
  LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
  df_temp <- df_CH4ff %>% 
    subset(ISO3 %in% LIST) %>%
    group_by(Year) %>%
    summarise(TgCH4 = sum(TgCH4, na.rm=T)) %>%
    ungroup()
  df_temp$ISO3 <- GROUP
  df_temp$CNTR_NAME <- GROUP
  
  df_CH4ff <- rbind(df_CH4ff, df_temp)
}

# Restrict to period 1850-onwards
df_CH4ff <- df_CH4ff %>% subset(Year >= start_year)

# Prep for export
df_CH4ff$Gas <- "CH4"
df_CH4ff$Unit <- "TgCH4"
df_CH4ff$Component <- "Fossil"
colnames(df_CH4ff)[which(colnames(df_CH4ff) == "TgCH4")] = "Data"

#//////////////////////////////////////////////////////////////////////////////#
#### Load LULUCF CH4 data ####
#//////////////////////////////////////////////////////////////////////////////#

# df_CH4luc.PRIMAP <- read.csv(paste0(INDIR, "CH4/Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv"))
df_CH4luc.PRIMAP <- read.csv(paste0(INDIR, "CH4/Guetschow-et-al-2022-PRIMAP-hist_v2.4_no_rounding_11-Oct-2022.csv"))

# Select M.AG (agriculture) emissions
df_CH4luc.PRIMAP <- df_CH4luc.PRIMAP %>%
  rename(scenario = `scenario..PRIMAP.hist.`,
         country = `area..ISO3.`,
         category = `category..IPCC2006_PRIMAP.`) %>%
  subset(scenario == "HISTTP" & entity == "CH4" & category %in% c("M.AG")) %>%
  select(-c(source, scenario, entity, unit, category))

# Convert to long format
df_CH4luc.PRIMAP <- df_CH4luc.PRIMAP %>% 
  pivot_longer(cols = -"country", names_to = "Year", values_to = "TgCH4")
df_CH4luc.PRIMAP$Year <- as.numeric(substr(df_CH4luc.PRIMAP$Year, 2, 5))

# Convert from GgGAS to TgGAS
df_CH4luc.PRIMAP$TgCH4 <- df_CH4luc.PRIMAP$TgCH4 / 1e3

# Get ISO3 Code
colnames(df_CH4luc.PRIMAP)[1] <- c("ISO3")
df_CH4luc.PRIMAP <- df_CH4luc.PRIMAP %>%
  inner_join(df_GCP_CNTR_ISO3_CODES, by = c("ISO3" = "ISO3")) %>%
  select(c(colnames(df_CH4luc.PRIMAP), "CNTR_NAME"))

df_CH4luc <- df_CH4luc.PRIMAP

# Get global totals
df_CH4luc_GLOBAL <- df_CH4luc %>%
  group_by(Year) %>%
  summarise(TgCH4 = sum(TgCH4, na.rm=T)) %>%
  ungroup()
df_CH4luc_GLOBAL$ISO3 <- "GLOBAL"
df_CH4luc_GLOBAL$CNTR_NAME <- "GLOBAL"
df_CH4luc <- rbind(df_CH4luc, df_CH4luc_GLOBAL)

for (GROUP in GROUPS) {
  LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
  df_temp <- df_CH4luc %>% 
    subset(ISO3 %in% LIST) %>%
    group_by(Year) %>%
    summarise(TgCH4 = sum(TgCH4, na.rm=T)) %>%
    ungroup()
  df_temp$ISO3 <- GROUP
  df_temp$CNTR_NAME <- GROUP
  
  df_CH4luc <- rbind(df_CH4luc, df_temp)
}

# Restrict to period 1850-onwards
df_CH4luc <- df_CH4luc %>% subset(Year >= start_year)

# Prep for export
df_CH4luc$Gas <- "CH4"
df_CH4luc$Unit <- "TgCH4"
df_CH4luc$Component <- "LULUCF"
colnames(df_CH4luc)[which(colnames(df_CH4luc) == "TgCH4")] = "Data"

#//////////////////////////////////////////////////////////////////////////////#
#### Load FF N2O data ####
#//////////////////////////////////////////////////////////////////////////////#

# df_N2Off <- read.csv(paste0(INDIR, "N2O/Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv"))
df_N2Off <- read.csv(paste0(INDIR, "CH4/Guetschow-et-al-2022-PRIMAP-hist_v2.4_no_rounding_11-Oct-2022.csv"))

# Select category M.0.EL ( = National Total excluding LULUCF)
# and M.AG (agriculture) emissions (which will soon be subtracted from M.0.EL)
df_N2Off <- df_N2Off %>% 
  rename(scenario = `scenario..PRIMAP.hist.`,
         country = `area..ISO3.`,
         category = `category..IPCC2006_PRIMAP.`) %>%
  subset(scenario == "HISTTP" & entity == "N2O" & category %in% c("M.0.EL", "M.AG")) %>%
  select(-c(source, scenario, entity, unit))

# Convert to long format
df_N2Off <- df_N2Off %>%
  pivot_longer(cols = -c("country", "category"), names_to = "Year", values_to = "TgN2O") 
df_N2Off$Year <- as.numeric(substr(df_N2Off$Year, 2, 5))

# Convert to wide format and subtract M.AG (agriculture) emissions from M.0.EL
df_N2Off <- df_N2Off %>% 
  pivot_wider(names_from = category, values_from = TgN2O) 

df_N2Off$TgN2O <- df_N2Off$M.0.EL - df_N2Off$M.AG

df_N2Off <- df_N2Off %>% 
  select(-c(M.0.EL, M.AG))

# Convert from GgGAS to TgGAS
df_N2Off$TgN2O <- df_N2Off$TgN2O / 1e3

# Get ISO3 Code
colnames(df_N2Off)[1] <- "ISO3"
df_N2Off <- df_N2Off %>% 
  inner_join(df_GCP_CNTR_ISO3_CODES, by = c("ISO3" = "ISO3")) %>%
  select(c(colnames(df_N2Off), "CNTR_NAME"))

# Get global totals
df_N2Off_GLOBAL <- df_N2Off %>%
  group_by(Year) %>%
  summarise(TgN2O = sum(TgN2O, na.rm=T)) %>%
  ungroup()
df_N2Off_GLOBAL$ISO3 <- "GLOBAL"
df_N2Off_GLOBAL$CNTR_NAME <- "GLOBAL"
df_N2Off <- rbind(df_N2Off, df_N2Off_GLOBAL)

for (GROUP in GROUPS) {
  LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
  df_temp <- df_N2Off %>% 
    subset(ISO3 %in% LIST) %>%
    group_by(Year) %>%
    summarise(TgN2O = sum(TgN2O, na.rm=T)) %>%
    ungroup()
  df_temp$ISO3 <- GROUP
  df_temp$CNTR_NAME <- GROUP
  
  df_N2Off <- rbind(df_N2Off, df_temp)
}

# Restrict to period 1850-onwards
df_N2Off <- df_N2Off %>% subset(Year >= start_year)

# Prep for export
df_N2Off$Gas <- "N2O"
df_N2Off$Unit <- "TgN2O"
df_N2Off$Component <- "Fossil"
colnames(df_N2Off)[which(colnames(df_N2Off) == "TgN2O")] = "Data"

#//////////////////////////////////////////////////////////////////////////////#
#### Load LULUCF N2O data ####
#//////////////////////////////////////////////////////////////////////////////#

# df_N2Oluc.PRIMAP <- read.csv(paste0(INDIR, "N2O/Guetschow-et-al-2021-PRIMAP-hist_v2.3.1_20-Sep_2021.csv"))
df_N2Oluc.PRIMAP <- read.csv(paste0(INDIR, "CH4/Guetschow-et-al-2022-PRIMAP-hist_v2.4_no_rounding_11-Oct-2022.csv"))

# Select M.AG (agriculture) emissions
df_N2Oluc.PRIMAP <- df_N2Oluc.PRIMAP %>%
  rename(scenario = `scenario..PRIMAP.hist.`,
         country = `area..ISO3.`,
         category = `category..IPCC2006_PRIMAP.`) %>%
  subset(scenario == "HISTTP" & entity == "N2O" & category %in% c("M.AG")) %>%
  select(-c(source, scenario, entity, unit, category))

# Convert to long format
df_N2Oluc.PRIMAP <- df_N2Oluc.PRIMAP %>% 
  pivot_longer(cols = -c("country"), names_to = "Year", values_to = "TgN2O") 
df_N2Oluc.PRIMAP$Year <- as.numeric(substr(df_N2Oluc.PRIMAP$Year, 2, 5))

# Convert from GgGAS to TgGAS
df_N2Oluc.PRIMAP$TgN2O <- df_N2Oluc.PRIMAP$TgN2O / 1e3

# Get ISO3 Code
colnames(df_N2Oluc.PRIMAP)[1] <- c("ISO3")
df_N2Oluc.PRIMAP <- df_N2Oluc.PRIMAP %>%
  inner_join(df_GCP_CNTR_ISO3_CODES, by = c("ISO3" = "ISO3")) %>%
  select(c(colnames(df_N2Oluc.PRIMAP), "CNTR_NAME"))

df_N2Oluc <- df_N2Oluc.PRIMAP

# Get global totals
df_N2Oluc_GLOBAL <- df_N2Oluc %>%
  group_by(Year) %>%
  summarise(TgN2O = sum(TgN2O, na.rm=T)) %>%
  ungroup()
df_N2Oluc_GLOBAL$ISO3 <- "GLOBAL"
df_N2Oluc_GLOBAL$CNTR_NAME <- "GLOBAL"
df_N2Oluc <- rbind(df_N2Oluc, df_N2Oluc_GLOBAL)

for (GROUP in GROUPS) {
  LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
  df_temp <- df_N2Oluc %>% 
    subset(ISO3 %in% LIST) %>%
    group_by(Year) %>%
    summarise(TgN2O = sum(TgN2O, na.rm=T)) %>%
    ungroup()
  df_temp$ISO3 <- GROUP
  df_temp$CNTR_NAME <- GROUP
  
  df_N2Oluc <- rbind(df_N2Oluc, df_temp)
}

# Restrict to period 1850-onwards
df_N2Oluc <- df_N2Oluc %>% subset(Year >= start_year)

# Prep for export
df_N2Oluc$Gas <- "N2O"
df_N2Oluc$Unit <- "TgN2O"
df_N2Oluc$Component <- "LULUCF"
colnames(df_N2Oluc)[which(colnames(df_N2Oluc) == "TgN2O")] = "Data"

#//////////////////////////////////////////////////////////////////////////////#
#### MERGE OUTPUT ####
#//////////////////////////////////////////////////////////////////////////////#

df_merged <- rbind(df_CO2ff, df_CO2luc, 
                   df_CH4ff, df_CH4luc,
                   df_N2Off, df_N2Oluc)

#//////////////////////////////////////////////////////////////////////////////#
#### INCLUDE THE SUM OF FF and LULUCF EMISSIONS ####
#//////////////////////////////////////////////////////////////////////////////#

temp <- df_merged %>%
  group_by(ISO3, CNTR_NAME, Gas, Unit, Year) %>%
  arrange(ISO3, CNTR_NAME, Gas, Unit, Year) %>%
  summarise(Data = sum(Data, na.rm=T)) %>%
  ungroup()
temp$Component <- "Total"

df_merged <- rbind(df_merged, temp)

# Export to CSV
write.csv(df_merged,
          paste0(OUTDIR, "/EMISSIONS_merged_input.csv"),
          row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### CALCULATE CUMULATIVE EMISSIONS ####
#//////////////////////////////////////////////////////////////////////////////#

df_merged$Data[is.na(df_merged$Data)] = 0

df_merged_cumulative <- df_merged %>%
  group_by(ISO3, CNTR_NAME, Gas, Unit, Component) %>%
  arrange(ISO3, CNTR_NAME, Gas, Unit, Component, Year) %>%
  mutate(Data = cumsum(Data)) %>%
  ungroup()
  
# Export to CSV
write.csv(df_merged_cumulative,
          paste0(OUTDIR, "/EMISSIONS_CUMULATIVE_merged_input.csv"),
          row.names = F)

#//////////////////////////////////////////////////////////////////////////////#
#### CALCULATE CUMALTIVE EMISSIONS RELATIVE TO A REFERENCE YEAR ####
#//////////////////////////////////////////////////////////////////////////////#

# Express cumulative emissions relative to ref_year

df_merged_cumulative.ref_year <- subset(df_merged_cumulative, Year == ref_year)
df_merged_cumulative <- subset(df_merged_cumulative, Year >= ref_year + 1)

df_merged_cumulative <- merge(
  df_merged_cumulative, df_merged_cumulative.ref_year,
  by = c("CNTR_NAME", "ISO3", "Gas", "Unit", "Component"),
  suffixes = c("", "_REF"),
  all.x = TRUE)

df_merged_cumulative$Data <- df_merged_cumulative$Data - df_merged_cumulative$Data_REF

df_merged_cumulative <- df_merged_cumulative %>% select(-c(Year_REF, Data_REF))
rm(df_merged_cumulative.ref_year)

write.csv(df_merged_cumulative,
          paste0(OUTDIR, "/EMISSIONS_CUMULATIVE_ref", ref_year, ".csv"),
          row.names = F)
