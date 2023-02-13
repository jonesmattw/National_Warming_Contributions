rm(list=ls())
print("Plotting GMST related to modelled cumulative emissions")

library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lemon)
library(scales)
library(ggpubr)
library(RColorBrewer)
library(sf)

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
SHAPEDIR <- paste0(ROOT, "Input/ISO3_borders/")
OUTDIR <- paste0(ROOT, "Output/PLOTS/")

#//////////////////////////////////////////////////////////////////////////////#
#### Load GMST Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_GMST <- read.csv(paste0(INDIR, "GMST_ref", ref_year,".csv"))
df_GMST <- df_GMST

df_GMST$Gas[df_GMST$Gas == "CO2"] <- CO2_expression
df_GMST$Gas[df_GMST$Gas == "CH4"] <- CH4_expression
df_GMST$Gas[df_GMST$Gas == "N2O"]  <- N2O_expression
df_GMST$Gas[df_GMST$Gas == "3-GHG"] <- GHG3_expression

df_GMST$Gas <- factor(df_GMST$Gas, levels = fixed_gas_order)

# Manually set the order of ISO3 variables 
df_GMST$ISO3 <- as.character(df_GMST$ISO3)
df_GMST$ISO3 <- as.factor(df_GMST$ISO3)

# Manually set the order of component variables 
df_GMST$Component <- factor(df_GMST$Component, 
                            levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Load GMST Data ####
#//////////////////////////////////////////////////////////////////////////////#

df_GMSTcontribution <- read.csv(paste0(INDIR, "GMST_contributions_ref", ref_year, ".csv"))
df_GMSTcontribution <- df_GMSTcontribution %>%
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

# Manually set the order of component variables 
df_GMSTcontribution$Component <- factor(df_GMSTcontribution$Component,
                                        levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Load Shapefile ####
#//////////////////////////////////////////////////////////////////////////////#

country_borders <- st_read(paste0(SHAPEDIR, "GCP_Country_Domains_v1.6.shp"))
sf::sf_use_s2(FALSE)
country_borders <- st_simplify(country_borders, preserveTopology = FALSE, dTolerance = 0.05)
country_borders <- country_borders %>% 
  select(ISO3_CODE, geometry) %>%
  rename(ISO3 = ISO3_CODE)

#//////////////////////////////////////////////////////////////////////////////#
#### Plotting theme and other plotting constants ####
#//////////////////////////////////////////////////////////////////////////////#

# Mapping theme
map_theme <- theme_minimal() + 
  theme(plot.title = element_text(size = 12, colour = "black", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size = 12, colour = "black", hjust = 0.5),
        
        panel.ontop=TRUE,
        panel.border=element_rect(fill = NA, colour = "black", size = 0.5),
        panel.background=element_blank(),
        
        axis.ticks = element_line(colour = "black", size = 0.25),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black"),
        axis.line = element_blank(),#(colour = "black", size = 0.5),
        
        panel.grid.major = element_line(colour = "grey75", size = 0.1),
        panel.grid.minor = element_blank(),#element_line(colour = "grey90", size = 0.1),
        
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.title = element_text(hjust = 0.5, colour = "black", size = 12),
        legend.text= element_text(colour = "black", size = 12),
        legend.direction = "horizontal", 
        legend.box = 'horizontal',
        
        
        plot.margin = margin(t = 0.1, r = 0.25, b = 0.1, l = 0.1, unit = "cm"),
        strip.text = element_text(size = 14, colour = "black", face="bold")
  )

# Continuous colour scales

colour_ramp_hotcold = rev(c("#57084E",  "#AB1F59",  "#FF4746",  "#FF8748",  "#FFC94A",  "#FFFF4C",  "#CEFF4E",  "#91FF51",
                        "#54FF55",  "#58FF99",  "#5CFFDC",  "#60FFFF",  "#64DFFF") #https://hihayk.github.io/scale/#2/10/66/33/45/-200/50/50/FF4746/255/71/69/white
                        )
# Discrete colour scales
colour_ramp_change <- c('#0000a4', '#003b9f', '#007799', '#00b294', '#24d69c', '#6de4b3', '#b6f1c9', #'#ffffe0',  
                        '#ffdde9', '#ffbcf2', '#ff9afb', '#f376db', '#da4f92', '#c22749', '#a90000')

# New facet label names for Year variable
years_to_plot = c(1960, 2000, final_year)
years_to_plot.label = list()
for (year_to_plot in years_to_plot) {
  years_to_plot.label = c(years_to_plot.label, paste0(ref_year+1, " to ", year_to_plot))
}

year_labeller <- function(variable,value){
  return(years_to_plot.label[value])
}

#//////////////////////////////////////////////////////////////////////////////#
#### Test Map ####
#//////////////////////////////////////////////////////////////////////////////#

ggplot() + 
  geom_sf(data = country_borders, size = 0.5, color = "black", fill = NA) + 
  ggtitle("TEST MAP: Countries") + 
  coord_sf(xlim=c(-180, 180), ylim=c(-60,75)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180), expand = c(0,0)) +
  scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90), expand = c(0,0)) +
  map_theme + xlab("Longitude (°)") + ylab("Latitude (°)") 

#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION % ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMSTcontribution %>%
  subset(Gas == "3-GHG" &
           Year %in% years_to_plot &
           Component == "Total")

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  mutate(Data = Data * 100) %>% # to % from fraction
  subset(!is.na(Data))

scaling_factor = 1
breaks = c(0.5,1,2.5,5,7.5,10,15,20,25) * scaling_factor
labels = breaks / scaling_factor

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.5, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtitle(NULL) + 
  coord_sf(xlim=c(-180, 180), ylim=c(-60,75)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180), expand = c(0,0)) +
  scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90), expand = c(0,0)) +
  scale_fill_gradientn(colours = colour_ramp_hotcold, 
                    trans = "pseudo_log", 
                    breaks = breaks,
                    labels = labels) +
  #scale_fill_manual(values = colour_ramp_hotcold, na.translate = F) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  guides(
    fill = guide_colorsteps(
      title = "Contribution to Global Change in GMST (%)",
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  facet_wrap(.~Year, ncol = 1, labeller = labeller(Year = year_labeller)) +
  map_theme

filename = paste0(OUTDIR, "Map_GMST_contributions_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 12, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}


#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION *C ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas == "3-GHG" &
           Year %in% years_to_plot &
           Component == "Total")

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  subset(!is.na(Data))

scaling_factor = 100
breaks = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.2, 0.25) * scaling_factor
labels = breaks / scaling_factor

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.5, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtitle(NULL) + 
  coord_sf(xlim=c(-180, 180), ylim=c(-60,75)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180), expand = c(0,0)) +
  scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90), expand = c(0,0)) +
  scale_fill_gradientn(colours = colour_ramp_hotcold, 
                    trans = "pseudo_log", 
                    breaks = breaks,
                    labels = labels) +
  #scale_fill_manual(values = colour_ramp_hotcold, na.translate = F) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  guides(
    fill = guide_colorsteps(
      title = "Contribution to Global Change in GMST (°C)",
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
    facet_wrap(.~Year, ncol = 1, labeller = labeller(Year = year_labeller)) +
  map_theme

filename = paste0(OUTDIR, "Map_GMST_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 12, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}
 


#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION FROM CO2 ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas %in% c("CO[2]", "3-GHG") &
           Year %in% years_to_plot &
           Component == "Total") %>%
  pivot_wider(id_cols = c(Year, ISO3), names_from = Gas, values_from = Data) %>%
  mutate(Data = 100 * `CO[2]` / `3-GHG`)

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  subset(!is.na(Data))

df_thisplot$Data[df_thisplot$Data <= 0] = 1e-16
df_thisplot$Data[df_thisplot$Data >= 100] = 100

scaling_factor = 1
breaks = c(seq(10, 90,10)) * scaling_factor
labels = breaks / scaling_factor

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.5, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtitle(NULL) + 
  coord_sf(xlim=c(-180, 180), ylim=c(-60,75)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180), expand = c(0,0)) +
  scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90), expand = c(0,0)) +
  scale_fill_gradientn(colours = colour_ramp_hotcold, 
                       #trans = "pseudo_log", 
                       breaks = breaks,
                       labels = labels) +
  #scale_fill_manual(values = colour_ramp_hotcold, na.translate = F) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  guides(
    fill = guide_colorsteps(
      title = bquote("Portion of Country's Overall Warming Contribution Related to"~CO[2]~"Emissions (%)"),
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
    facet_wrap(.~Year, ncol = 1, labeller = labeller(Year = year_labeller)) +
  map_theme

filename = paste0(OUTDIR, "Map_GMST_CO2fraction_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 12, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}


#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION FROM Agriculture ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas == "3-GHG" &
           Year %in% years_to_plot &
           Component %in% c("Total", "LULUCF")) %>%
  pivot_wider(id_cols = c(Year, ISO3), names_from = Component, values_from = Data) %>%
  mutate(Data = 100 * LULUCF / Total)

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  subset(!is.na(Data))

df_thisplot$Data[df_thisplot$Data <= 0] = 1e-16
df_thisplot$Data[df_thisplot$Data >= 100] = 100

scaling_factor = 1
breaks = c(seq(10, 90,10)) * scaling_factor
labels = breaks / scaling_factor

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.5, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtitle(NULL) + 
  coord_sf(xlim=c(-180, 180), ylim=c(-60,75)) +
  scale_x_continuous(breaks = c(-180, -120, -60, 0, 60, 120, 180), expand = c(0,0)) +
  scale_y_continuous(breaks = c(-90, -60, -30, 0, 30, 60, 90), expand = c(0,0)) +
  scale_fill_gradientn(colours = colour_ramp_hotcold, 
                       #trans = "pseudo_log", 
                       breaks = breaks,
                       labels = labels) +
  #scale_fill_manual(values = colour_ramp_hotcold, na.translate = F) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  guides(
    fill = guide_colorsteps(
      title = bquote("Portion of Country's Overall Warming Contribution Related to LULUCF Emissions (%)"),
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
    facet_wrap(.~Year, ncol = 1, labeller = labeller(Year = year_labeller)) +
  map_theme

filename = paste0(OUTDIR, "Map_GMST_LULUCFfraction_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 12, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}
 
