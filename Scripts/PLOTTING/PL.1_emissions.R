rm(list=ls())
print("Plotting the global and regional EMISSIONS data")

library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lemon)
library(scales)
library(ggpubr)

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

####
# Prep
####

# Directories

INDIR <- paste0(ROOT, "Output/EMISSIONS/")
OUTDIR <- paste0(ROOT, "Output/PLOTS/")

# Constants
fixed_gas_order_ANNUAL <- fixed_gas_order[!grepl('3-GHG', fixed_gas_order)] # Cannot combine annual CH4 with N2O and CO2
fixed_gas_order_CUMULATIVE <- fixed_gas_order
fixed_gas_unit_order <- fixed_gas_unit_order[!grepl('3-GHG', fixed_gas_unit_order)] 
fixed_gas_unit_order_CUMULATIVE <- fixed_gas_unit_order_CUMULATIVE

#//////////////////////////////////////////////////////////////////////////////#
#### Load Emissions Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_EMISSIONS <- read.csv(paste0(INDIR, "EMISSIONS_merged_input.csv"))

df_EMISSIONS <- df_EMISSIONS %>% subset(ISO3 %in% fixed_ISO3s)

df_EMISSIONS$Gas[df_EMISSIONS$Gas == "CO2"] <- CO2_expression
df_EMISSIONS$Gas[df_EMISSIONS$Gas == "CH4"] <- CH4_expression
df_EMISSIONS$Gas[df_EMISSIONS$Gas == "N2O"]  <- N2O_expression
df_EMISSIONS$Gas[df_EMISSIONS$Gas == "3-GHG"]  <- GHG3_expression
df_EMISSIONS$Gas <- factor(df_EMISSIONS$Gas, levels = fixed_gas_order_ANNUAL)

df_EMISSIONS$Unit <- df_UNITS$Unit[match(df_EMISSIONS$Gas, df_UNITS$Gas)]
df_EMISSIONS$Unit <- factor(df_EMISSIONS$Unit, levels = fixed_unit_order)

df_EMISSIONS$Gas_Unit <- df_UNITS$Gas_Unit[match(df_EMISSIONS$Gas, df_UNITS$Gas)]
df_EMISSIONS$Gas_Unit <- factor(df_EMISSIONS$Gas_Unit, levels = fixed_gas_unit_order)

# Manually set the order of ISO3 variables 
df_EMISSIONS$ISO3 <- as.character(df_EMISSIONS$ISO3)
df_EMISSIONS$ISO3 <- as.factor(df_EMISSIONS$ISO3)
df_EMISSIONS$ISO3 <- factor(df_EMISSIONS$ISO3, levels = fixed_ISO3s)

# Manually set the order of component variables 
df_EMISSIONS$Component <- factor(df_EMISSIONS$Component, levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Load Cumulative Emissions Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_EMISSIONS_CUMULATIVE <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_ref", ref_year, ".csv"))

df_EMISSIONS_CUMULATIVE <- df_EMISSIONS_CUMULATIVE %>% 
  subset(ISO3 %in% fixed_ISO3s)

# Append CO2e Emissions Data
df_EMISSIONS_CUMULATIVE_CO2e <- read.csv(paste0(INDIR, "EMISSIONS_CUMULATIVE_CO2e100_GWPstar.csv"))
df_EMISSIONS_CUMULATIVE_CO2e <- df_EMISSIONS_CUMULATIVE_CO2e %>%
  subset(ISO3 %in% fixed_ISO3s & Gas == "3-GHG")
df_EMISSIONS_CUMULATIVE <- rbind(df_EMISSIONS_CUMULATIVE, df_EMISSIONS_CUMULATIVE_CO2e)

df_EMISSIONS_CUMULATIVE$Gas[df_EMISSIONS_CUMULATIVE$Gas == "CO2"] <- CO2_expression
df_EMISSIONS_CUMULATIVE$Gas[df_EMISSIONS_CUMULATIVE$Gas == "CH4"] <- CH4_expression
df_EMISSIONS_CUMULATIVE$Gas[df_EMISSIONS_CUMULATIVE$Gas == "N2O"]  <- N2O_expression
df_EMISSIONS_CUMULATIVE$Gas[df_EMISSIONS_CUMULATIVE$Gas == "3-GHG"]  <- GHG3_expression
df_EMISSIONS_CUMULATIVE$Gas <- factor(df_EMISSIONS_CUMULATIVE$Gas, levels = fixed_gas_order_CUMULATIVE)

df_EMISSIONS_CUMULATIVE$Unit <- df_UNITS$Unit_CUMULATIVE[match(df_EMISSIONS_CUMULATIVE$Gas, df_UNITS$Gas)]
df_EMISSIONS_CUMULATIVE$Unit <- factor(df_EMISSIONS_CUMULATIVE$Unit, levels = fixed_unit_order_CUMULATIVE)

df_EMISSIONS_CUMULATIVE$Gas_Unit <- df_UNITS$Gas_Unit_CUMULATIVE[match(df_EMISSIONS_CUMULATIVE$Gas, df_UNITS$Gas)]
df_EMISSIONS_CUMULATIVE$Gas_Unit <- factor(df_EMISSIONS_CUMULATIVE$Gas_Unit, levels = fixed_gas_unit_order_CUMULATIVE)

# Manually set the order of ISO3 variables 
df_EMISSIONS_CUMULATIVE$ISO3 <- as.character(df_EMISSIONS_CUMULATIVE$ISO3)
df_EMISSIONS_CUMULATIVE$ISO3 <- as.factor(df_EMISSIONS_CUMULATIVE$ISO3)
df_EMISSIONS_CUMULATIVE$ISO3 <- factor(df_EMISSIONS_CUMULATIVE$ISO3, levels = fixed_ISO3s)

# Manually set the order of component variables 
df_EMISSIONS_CUMULATIVE$Component <- factor(df_EMISSIONS_CUMULATIVE$Component, levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### FINALISE PLOTTING DATA ####
#//////////////////////////////////////////////////////////////////////////////#

df_EMISSIONS$Data[df_EMISSIONS$ISO3 == "GLOBAL"] <- df_EMISSIONS$Data[df_EMISSIONS$ISO3 == "GLOBAL"] / 10
levels(df_EMISSIONS$ISO3) <- gsub("GLOBAL","GLOBAL ÷ 10", levels(df_EMISSIONS$ISO3))

df_EMISSIONS_CUMULATIVE$Data[df_EMISSIONS_CUMULATIVE$ISO3 == "GLOBAL"] <- df_EMISSIONS_CUMULATIVE$Data[df_EMISSIONS_CUMULATIVE$ISO3 == "GLOBAL"] / 10
levels(df_EMISSIONS_CUMULATIVE$ISO3) <- gsub("GLOBAL","GLOBAL ÷ 10", levels(df_EMISSIONS_CUMULATIVE$ISO3))

#//////////////////////////////////////////////////////////////////////////////#
#### PLOT ANNUAL EMISSIONS ####
#//////////////////////////////////////////////////////////////////////////////#

ymax = c()
ymin = c()
for (g in fixed_gas_order_ANNUAL) {
  ymax_g = y_axis_upper(max(df_EMISSIONS$Data[df_EMISSIONS$Gas == g], na.rm=T))
  ymax = c(ymax, ymax_g)
  
  ymin_g = y_axis_lower(max(df_EMISSIONS$Data[df_EMISSIONS$Gas == g], na.rm=T),
                        min(df_EMISSIONS$Data[df_EMISSIONS$Gas == g], na.rm=T))
  ymin = c(ymin, ymin_g)
}

blank_data <- data.frame(Gas = fixed_gas_order_ANNUAL,
                         Gas_Unit = fixed_gas_unit_order,
                         x = ref_year,
                         ymax = ymax,
                         ymin = ymin)
blank_data$Gas <- factor(blank_data$Gas, levels = fixed_gas_order_ANNUAL)
blank_data$Gas_Unit <- factor(blank_data$Gas_Unit, levels = fixed_gas_unit_order)

fig_EMISSIONS <- ggplot() +
  
  geom_line(data = df_EMISSIONS,
            aes(x = Year, y = Data, colour = ISO3)) +#, linetype = ISO3, alpha = ISO3)) +
  
  geom_blank(data = blank_data, aes(x = x, y = ymin)) +
  geom_blank(data = blank_data, aes(x = x, y = ymax)) +
  
  # Labels
  ggtitle(bquote("Annual Greenhouse Gas Emissions")) +
  
  # Fix the axes ranges
  ylab(NULL) +
  
  # Customise the scales
  scale_y_continuous(expand=c(0,NA)) +
  scale_x_continuous(limits = year_limits(ref_year, final_year), 
                     breaks = year_breaks(ref_year, final_year),
                     expand = c(0,0,0,0)) +
  scale_colour_manual(values = colour_custom) + 
  #scale_linetype_manual(values = linetype_custom) + 
  #scale_alpha_manual(values = alpha_custom) +
  
  # Facet
  facet_rep_grid(Gas_Unit ~ Component, space = "fixed", scales = "free",
             switch = "y",
             labeller = labeller(Gas_Unit = label_parsed, Component = component_labels)) +
  
  # Legend
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Apply theme
  theme

fig_EMISSIONS

#//////////////////////////////////////////////////////////////////////////////#
#### PLOT CUMULATIVE EMISSIONS ####
#//////////////////////////////////////////////////////////////////////////////#

ymax = c()
ymin = c()
for (g in fixed_gas_order_CUMULATIVE) {
  ymax_g = y_axis_upper(max(df_EMISSIONS_CUMULATIVE$Data[df_EMISSIONS_CUMULATIVE$Gas == g], na.rm=T))
  ymax = c(ymax, ymax_g)
  
  ymin_g = y_axis_lower(max(df_EMISSIONS_CUMULATIVE$Data[df_EMISSIONS_CUMULATIVE$Gas == g], na.rm=T),
                        min(df_EMISSIONS_CUMULATIVE$Data[df_EMISSIONS_CUMULATIVE$Gas == g], na.rm=T))
  ymin = c(ymin, ymin_g)
}

blank_data <- data.frame(Gas = fixed_gas_order_CUMULATIVE,
                         Gas_Unit = fixed_gas_unit_order_CUMULATIVE,
                         x = ref_year,
                         ymax = ymax,
                         ymin = ymin)
blank_data$Gas <- factor(blank_data$Gas, levels = fixed_gas_order_CUMULATIVE)
blank_data$Gas_Unit <- factor(blank_data$Gas_Unit, levels = fixed_gas_unit_order_CUMULATIVE)

fig_EMISSIONS_CUMULATIVE <- ggplot() +
  
  geom_line(data = df_EMISSIONS_CUMULATIVE,
            aes(x = Year, y = Data, colour = ISO3))+#, linetype = ISO3, alpha = ISO3)) +
  
  geom_blank(data = blank_data, aes(x = x, y = ymin)) +
  geom_blank(data = blank_data, aes(x = x, y = ymax)) +
  
  # Labels
  ggtitle(bquote("Cumulative Greenhouse Gas Emissions")) +
  
  ylab(NULL) +
  
  # Customise the scales
  scale_y_continuous(expand=c(0,NA), labels = comma) +
  scale_x_continuous(limits = year_limits(ref_year, final_year), 
                     breaks = year_breaks(ref_year, final_year),
                     expand = c(0,0,0,0)) +
  scale_colour_manual(values = colour_custom) + 
  # scale_linetype_manual(values = linetype_custom) + 
  # scale_alpha_manual(values = alpha_custom) +
  
  # Facet
  facet_rep_grid(Gas_Unit ~ Component, space = "fixed", scales = "free",
                 switch = "y",
                 labeller = labeller(Gas_Unit = label_parsed, Component = component_labels)) +
  
  # Legend
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Apply theme
  theme

fig_EMISSIONS_CUMULATIVE

#//////////////////////////////////////////////////////////////////////////////#
#### SAVE ####
#//////////////////////////////////////////////////////////////////////////////#

ggsave(paste0(OUTDIR, "Fig_EMISSIONS.pdf"), 
       plot = fig_EMISSIONS, device = "pdf",
       height = 25, width = 25, units = "cm",
       dpi = 300)

ggsave(paste0(OUTDIR, "Fig_EMISSIONS.png"),
       plot = fig_EMISSIONS, device = "png",
       height = 25, width = 25, units = "cm",
       dpi = 300)

ggsave(paste0(OUTDIR, "Fig_EMISSIONS_CUMULATIVE.pdf"),
       plot = fig_EMISSIONS_CUMULATIVE, device = "pdf",
       height = 30, width = 25, units = "cm",
       dpi = 300)

ggsave(paste0(OUTDIR, "Fig_EMISSIONS_CUMULATIVE.png"),
       plot = fig_EMISSIONS_CUMULATIVE, device = "png",
       height = 30, width = 25, units = "cm",
       dpi = 300)

