rm(list=ls())
print("Plotting European contribution to GMST based on modelled global and regional GMST")

library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lemon)
library(scales)
library(ggpubr)
library(RColorBrewer)

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

source(paste0(ROOT, "../Scripts/HELPERS.R"))

####
# Prep
####

# Directories

INDIR <- paste0(ROOT, "Output/GMST/")
OUTDIR <- paste0(ROOT, "Output/OUTREACH/")
if (!dir.exists(OUTDIR)) {
  dir.create(OUTDIR, recursive=TRUE)
}

fixed_ISO3s = c("ANNEXI", "BASIC", "EIT", "LDC", "OECD")#, "CHN", "IND", "IDN", "BRA", "USA", "RUS", "EU27")
fixed_names = c("UNFCCC Annex-I parties", "BASIC bloc (Brazil, South Africa, India, China)", "Economies in Transition", "Least Developed Countries", "OECD members")
colour_custom = colorRampPalette(brewer.pal(length(fixed_ISO3s), "Set1"))(length(fixed_ISO3s))
show_col(colour_custom)

#//////////////////////////////////////////////////////////////////////////////#
#### Load GMST Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_GMSTcontribution <- read.csv(paste0(INDIR, "GMST_contributions_ref", ref_year, ".csv"))
df_GMSTcontribution <- df_GMSTcontribution %>% 
  subset(ISO3 %in% fixed_ISO3s) %>%
  subset(!(ISO3 == "GLOBAL")) %>%
  select(-Data_degrees) %>%
  rename(Data = Data_fraction)

df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "CO2"] <- CO2_expression
df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "CH4"] <- CH4_expression
df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "N2O"]  <- N2O_expression
df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "3-GHG"] <- GHG3_expression

df_GMSTcontribution$Gas <- factor(df_GMSTcontribution$Gas, levels = fixed_gas_order)

# Manually set the order of ISO3 variables 
df_GMSTcontribution$ISO3 <- as.character(df_GMSTcontribution$ISO3)
df_GMSTcontribution$ISO3 <- as.factor(df_GMSTcontribution$ISO3)
df_GMSTcontribution$ISO3 <- factor(df_GMSTcontribution$ISO3, levels = fixed_ISO3s)

# Manually set the order of component variables 
df_GMSTcontribution$Component <- factor(df_GMSTcontribution$Component,
                                        levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Make the plots ####
#//////////////////////////////////////////////////////////////////////////////#

ymax = c()
ymin = c()
for (g in fixed_gas_order) {
  ymax_g = y_axis_upper(max(df_GMSTcontribution$Data[df_GMSTcontribution$Gas == g], na.rm=T))
  ymax = c(ymax, ymax_g)
  
  ymin_g = y_axis_lower(max(df_GMSTcontribution$Data[df_GMSTcontribution$Gas == g], na.rm=T),
                        min(df_GMSTcontribution$Data[df_GMSTcontribution$Gas == g], na.rm=T))
  ymin = c(ymin, ymin_g)
}

blank_data <- data.frame(Gas = fixed_gas_order,
                         x = ref_year,
                         ymax = ymax,
                         ymin = ymin)
blank_data$Gas <- factor(blank_data$Gas, levels = fixed_gas_order)

fig_GMSTcontribution <- ggplot() +
  
  geom_line(data = df_GMSTcontribution,
            aes(x = Year, y = Data * 100, colour = ISO3))+#, alpha = ISO3)) +
  
  geom_blank(data = blank_data, aes(x = x, y = ymax * 100)) +
  geom_blank(data = blank_data, aes(x = x, y = ymax * 100)) +
  
  # Labels
  ggtitle(bquote("Grouped Contributions to Change in Global Mean Surface Temperature")) +
  
  # Fix the axes ranges
  ylab(bquote("Contribution to Change in GMST (%)")) +
  
  # Customise the scales
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = year_limits(ref_year, final_year), 
                     breaks = year_breaks(ref_year, final_year),
                     expand = c(0,0,0,0)) +
  scale_colour_manual(values = colour_custom, labels = fixed_names) +
  #scale_alpha_manual(values = alpha_custom) +
  
  # Facet
  facet_rep_grid(Gas ~ Component, space = "fixed", scales = "free",
                 switch = "y",
                 labeller = labeller(Gas = label_parsed, Component = component_labels)) +
  
  # Legend
  guides(color = guide_legend(ncol = 1, byrow = TRUE)) +
  
  # Apply theme
  theme

fig_GMSTcontribution

#//////////////////////////////////////////////////////////////////////////////#
#### SAVE ####
#//////////////////////////////////////////////////////////////////////////////#

ggsave(paste0(OUTDIR, "PLOTS_Chart_GMST_contributions_ref",  ref_year ,"_groups.pdf"),
       plot = fig_GMSTcontribution, device = "pdf",
       height = 30, width = 25, units = "cm",
       dpi = 300)

ggsave(paste0(OUTDIR, "PLOTS_Chart_GMST_contributions_ref",  ref_year ,"_groups.png"), 
       plot = fig_GMSTcontribution, device = "png",
       height = 30, width = 25, units = "cm",
       dpi = 300)




####
# Prep
####

# Directories

INDIR <- paste0(ROOT, "Output/GMST/")
OUTDIR <- paste0(ROOT, "Output/OUTREACH/")

fixed_ISO3s = c("BRA", "CHN", "EU27", "DEU", "IND", "IDN", "RUS", "GBR", "USA") 
fixed_names = c("Brazil", "China", "EU27", "Germany", "India", "Indonesia", "Russia", "UK", "USA")
colour_custom = colorRampPalette(brewer.pal(length(fixed_ISO3s), "Set1"))(length(fixed_ISO3s))
show_col(colour_custom)

#//////////////////////////////////////////////////////////////////////////////#
#### Load GMST Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_GMSTcontribution <- read.csv(paste0(INDIR, "GMST_contributions_ref", ref_year, ".csv"))
df_GMSTcontribution <- df_GMSTcontribution %>% 
  subset(ISO3 %in% fixed_ISO3s) %>%
  subset(!(ISO3 == "GLOBAL")) %>%
  select(-Data_degrees) %>%
  rename(Data = Data_fraction)

df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "CO2"] <- CO2_expression
df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "CH4"] <- CH4_expression
df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "N2O"]  <- N2O_expression
df_GMSTcontribution$Gas[df_GMSTcontribution$Gas == "3-GHG"] <- GHG3_expression

df_GMSTcontribution$Gas <- factor(df_GMSTcontribution$Gas, levels = fixed_gas_order)

# Manually set the order of ISO3 variables 
df_GMSTcontribution$ISO3 <- as.character(df_GMSTcontribution$ISO3)
df_GMSTcontribution$ISO3 <- as.factor(df_GMSTcontribution$ISO3)
df_GMSTcontribution$ISO3 <- factor(df_GMSTcontribution$ISO3, levels = fixed_ISO3s)

# Manually set the order of component variables 
df_GMSTcontribution$Component <- factor(df_GMSTcontribution$Component,
                                        levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Make the plots ####
#//////////////////////////////////////////////////////////////////////////////#

ymax = c()
ymin = c()
for (g in fixed_gas_order) {
  ymax_g = y_axis_upper(max(df_GMSTcontribution$Data[df_GMSTcontribution$Gas == g], na.rm=T))
  ymax = c(ymax, ymax_g)
  
  ymin_g = y_axis_lower(max(df_GMSTcontribution$Data[df_GMSTcontribution$Gas == g], na.rm=T),
                        min(df_GMSTcontribution$Data[df_GMSTcontribution$Gas == g], na.rm=T))
  ymin = c(ymin, ymin_g)
}

blank_data <- data.frame(Gas = fixed_gas_order,
                         x = ref_year,
                         ymax = ymax,
                         ymin = ymin)
blank_data$Gas <- factor(blank_data$Gas, levels = fixed_gas_order)

fig_GMSTcontribution <- ggplot() +
  
  geom_line(data = df_GMSTcontribution,
            aes(x = Year, y = Data * 100, colour = ISO3))+#, alpha = ISO3)) +
  
  geom_blank(data = blank_data, aes(x = x, y = ymax * 100)) +
  geom_blank(data = blank_data, aes(x = x, y = ymax * 100)) +
  
  # Labels
  ggtitle(bquote("National/Regional Contributions to Change in Global Mean Surface Temperature")) +
  
  # Fix the axes ranges
  ylab(bquote("Contribution to Change in GMST (%)")) +
  
  # Customise the scales
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(limits = year_limits(ref_year, final_year), 
                     breaks = year_breaks(ref_year, final_year),
                     expand = c(0,0,0,0)) +
  scale_colour_manual(values = colour_custom, labels = fixed_names) +
  #scale_alpha_manual(values = alpha_custom) +
  
  # Facet
  facet_rep_grid(Gas ~ Component, space = "fixed", scales = "free",
                 switch = "y",
                 labeller = labeller(Gas = label_parsed, Component = component_labels)) +
  
  # Legend
  guides(color = guide_legend(ncol = 3, byrow = FALSE)) +
  
  # Apply theme
  theme

fig_GMSTcontribution

#//////////////////////////////////////////////////////////////////////////////#
#### SAVE ####
#//////////////////////////////////////////////////////////////////////////////#

ggsave(paste0(OUTDIR, "PLOTS_Chart_GMST_contributions_ref",  ref_year ,"_somecountries.pdf"),
       plot = fig_GMSTcontribution, device = "pdf",
       height = 30, width = 25, units = "cm",
       dpi = 300)

ggsave(paste0(OUTDIR, "PLOTS_Chart_GMST_contributions_ref",  ref_year ,"_somecountries.png"), 
       plot = fig_GMSTcontribution, device = "png",
       height = 30, width = 25, units = "cm",
       dpi = 300)

