rm(list=ls())
print("Plotting GMST related to modelled cumulative emissions")

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

INDIR <- paste0(ROOT, "Output/GMST/")
OUTDIR <- paste0(ROOT, "Output/PLOTS/")

#//////////////////////////////////////////////////////////////////////////////#
#### Load GMST Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_GMST <- read.csv(paste0(INDIR, "GMST_ref", ref_year,".csv"))
df_GMST <- df_GMST %>% 
  subset(ISO3 %in% fixed_ISO3s)

df_GMST$Gas[df_GMST$Gas == "CO2"] <- CO2_expression
df_GMST$Gas[df_GMST$Gas == "CH4"] <- CH4_expression
df_GMST$Gas[df_GMST$Gas == "N2O"]  <- N2O_expression
df_GMST$Gas[df_GMST$Gas == "3-GHG"] <- GHG3_expression

df_GMST$Gas <- factor(df_GMST$Gas, levels = fixed_gas_order)

# Manually set the order of ISO3 variables 
df_GMST$ISO3 <- as.character(df_GMST$ISO3)
df_GMST$ISO3 <- as.factor(df_GMST$ISO3)
df_GMST$ISO3 <- factor(df_GMST$ISO3, levels = fixed_ISO3s)

# Manually set the order of component variables 
df_GMST$Component <- factor(df_GMST$Component,
                            levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Make the plots ####
#//////////////////////////////////////////////////////////////////////////////#

ymax = c()
ymin = c()
for (g in fixed_gas_order) {
  ymax_g = y_axis_upper(max(df_GMST$Data[df_GMST$Gas == g], na.rm=T))
  ymax = c(ymax, ymax_g)
  
  ymin_g = y_axis_lower(max(df_GMST$Data[df_GMST$Gas == g], na.rm=T),
                        min(df_GMST$Data[df_GMST$Gas == g], na.rm=T))
  ymin = c(ymin, ymin_g)
}

blank_data <- data.frame(Gas = fixed_gas_order,
                         x = ref_year,
                         ymax = ymax,
                         ymin = ymin)
blank_data$Gas <- factor(blank_data$Gas, levels = fixed_gas_order)

fig_GMST <- ggplot() +
  
  geom_line(data = df_GMST,
            aes(x = Year, y = Data, colour = ISO3))+
  
  geom_blank(data = blank_data, aes(x = x, y = ymax)) +
  geom_blank(data = blank_data, aes(x = x, y = ymin)) +
  
  # Labels
  ggtitle(bquote("Change in Global Mean Surface Temperature (°C) caused by Greenhouse Gas Emissions")) +
  
  # Fix the axes ranges
  coord_cartesian(ylim = c(NA, NA)
  ) +
  ylab(bquote("Change in GMST (°C)")) +
  
  # Customise the scales
  scale_y_continuous(expand = c(0,0.000001)) +
  scale_x_continuous(limits = year_limits(ref_year, final_year), 
                     breaks = year_breaks(ref_year, final_year),
                     expand = c(0,0.000001)) +
  scale_colour_manual(values = colour_custom) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  
  # Facet
  facet_rep_grid(Gas ~ Component, space = "fixed", scales = "free",
                 switch = "y",
                 labeller = labeller(Gas = label_parsed, Component = component_labels)) +
  
  # Legend
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
  
  # Apply theme
  theme

fig_GMST

#//////////////////////////////////////////////////////////////////////////////#
#### SAVE ####
#//////////////////////////////////////////////////////////////////////////////#

ggsave(paste0(OUTDIR, "Fig_GMST_ref", ref_year, ".pdf"),
       plot = fig_GMST, device = "pdf",
       height = 30, width = 25, units = "cm",
       dpi = 300)

ggsave(paste0(OUTDIR, "Fig_GMST_ref", ref_year, ".png"), 
       plot = fig_GMST, device = "png",
       height = 30, width = 25, units = "cm",
       dpi = 300)
