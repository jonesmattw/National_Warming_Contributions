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
library(magick)
library(cowplot)
library(readxl)
library(viridis)

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
SHAPEDIR <- paste0(ROOT, "Input/ISO3_borders/")
OUTDIR <- paste0(ROOT, "Output/OUTREACH/")
if (!dir.exists(OUTDIR)) {
  dir.create(OUTDIR, recursive=TRUE)
}

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
#### Load GMST contribution data ####
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

# #//////////////////////////////////////////////////////////////////////////////#
# #### Load GMST per capita data ####
# #//////////////////////////////////////////////////////////////////////////////#
# 
# df_GMST_percap <- read.csv(paste0(INDIR, "GMST_percap_ref", ref_year,".csv"))
# df_GMST_percap <- df_GMST_percap
# 
# df_GMST_percap$Gas[df_GMST_percap$Gas == "CO2"] <- CO2_expression
# df_GMST_percap$Gas[df_GMST_percap$Gas == "CH4"] <- CH4_expression
# df_GMST_percap$Gas[df_GMST_percap$Gas == "N2O"]  <- N2O_expression
# df_GMST_percap$Gas[df_GMST_percap$Gas == "3-GHG"] <- GHG3_expression
# 
# df_GMST_percap$Gas <- factor(df_GMST_percap$Gas, levels = fixed_gas_order)
# 
# # Manually set the order of ISO3 variables 
# df_GMST_percap$ISO3 <- as.character(df_GMST_percap$ISO3)
# df_GMST_percap$ISO3 <- as.factor(df_GMST_percap$ISO3)
# 
# # Manually set the order of component variables 
# df_GMST_percap$Component <- factor(df_GMST_percap$Component, 
#                             levels = fixed_component_order)

#//////////////////////////////////////////////////////////////////////////////#
#### Load Shapefile ####
#//////////////////////////////////////////////////////////////////////////////#

country_borders <- st_read(paste0(SHAPEDIR, "GCP_Country_Domains_v1.6.shp"))
sf::sf_use_s2(FALSE)
country_borders <- st_simplify(country_borders, preserveTopology = FALSE, dTolerance = 0.05)
country_borders <- country_borders %>% 
  select(ISO3_CODE, geometry) %>%
  rename(ISO3 = ISO3_CODE)

# Merge EU27 Countries

GROUP <- "EU27"
LIST <- as.vector(read_excel(paste0(ROOT, "Input/COUNTRY_GROUPINGS.xlsx"), sheet = paste0(GROUP,"_LIST"), col_names =  F)[[1]])
# df_temp <- df_CO2ff %>% 
#   subset(ISO3 %in% LIST) %>% 
#   group_by(Year) %>%
#   summarise(PgCO2 = sum(PgCO2, na.rm=T)) %>%
#   ungroup()
# df_temp$ISO3 <- GROUP
# df_temp$CNTR_NAME <- GROUP
# 
# df_CO2ff <- rbind(df_CO2ff, df_temp)

# Merge EU27 Countries
country_borders_EU27_merged_A <- country_borders %>%
  filter(!(ISO3 %in% LIST))

country_borders_EU27_merged_B <- country_borders %>%
  filter(ISO3 %in% LIST) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  mutate(ISO3 = "EU27")

country_borders_EU27_merged <- country_borders_EU27_merged_A %>%
  bind_rows(country_borders_EU27_merged_B)

rm(country_borders_EU27_merged_A, country_borders_EU27_merged_B)

#//////////////////////////////////////////////////////////////////////////////#
#### Plotting theme and other plotting constants ####
#//////////////////////////////////////////////////////////////////////////////#

# Mapping theme
map_theme <- theme_minimal() + 
  theme(plot.title = element_text(size = 12, colour = "grey50", hjust = 0.5, face="bold"),
        plot.subtitle = element_text(size = 12, colour = "grey50", hjust = 0.5),
        
        panel.ontop=TRUE,
        panel.border=element_rect(fill = NA, colour = "grey50", size = 0.5),
        panel.background=element_blank(),
        
        axis.ticks = element_blank(),#element_line(colour = "grey50", size = 0.25),
        axis.text = element_blank(),#element_text(size = 12, colour = "grey50"),
        axis.title = element_text(size = 12, colour = "grey50"),
        axis.line = element_blank(),#element_blank(),#(colour = "grey50", size = 0.5),
        
        panel.grid.major = element_blank(),#element_line(colour = "grey75", size = 0.1),
        panel.grid.minor = element_blank(),#element_line(colour = "grey90", size = 0.1),
        
        legend.position = "bottom",
        legend.justification = c(0.5,0.5),
        legend.title = element_text(hjust = 0.5, colour = "grey50", size = 12),
        legend.text= element_text(colour = "grey50", size = 12),
        legend.direction = "horizontal", 
        legend.box = 'horizontal',
        
        
        plot.margin = margin(t = 0.1, r = 0.25, b = 0.1, l = 0.1, unit = "cm"),
        strip.text = element_text(size = 14, colour = "grey50", face="bold")
  )


map_theme_robinson <- theme(
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.box = 'horizontal',
  legend.title = element_text(size=12, colour="grey50", hjust = 0.5),
  legend.text = element_text(size=11, colour="grey50", hjust = 0.5),
  legend.justification = c(0.5,0.5),
  legend.box.background = element_rect(fill=NA, colour=NA),
  legend.background = element_rect(fill=NA, colour=NA),
  
  strip.placement="top",
  strip.background=element_rect(fill=NA),
  strip.text=element_text(size=12, face="bold"),
  
  plot.title = element_text(size = 12, colour = "grey50", hjust = 0.5, face="bold"),
  plot.subtitle = element_text(size = 10, colour = "grey50", hjust = 0.5),
  plot.background=element_rect(fill=NA),
  
  panel.background=element_rect(fill=NA),
  panel.grid = element_line(color = "grey50", size = 0.2),
  panel.ontop=FALSE,
  
  axis.text = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  
  plot.caption = element_text(size = 10, colour = "grey50")
) 

# Continuous colour scales

colour_ramp_hotcold = rev(c("#57084E",  "#AB1F59",  "#FF4746",  "#FF8748",  "#FFC94A",  "#FFFF4C",  "#CEFF4E",  "#91FF51",
                        "#54FF55",  "#58FF99",  "#5CFFDC",  "#60FFFF",  "#64DFFF") #https://hihayk.github.io/scale/#2/10/66/33/45/-200/50/50/FF4746/255/71/69/white
                        )
# Discrete colour scales
colour_ramp_change <- c('#0000a4', '#003b9f', '#007799', '#00b294', '#24d69c', '#6de4b3', '#b6f1c9', #'#ffffe0',  
                        '#ffdde9', '#ffbcf2', '#ff9afb', '#f376db', '#da4f92', '#c22749', '#a90000')

inferno_colour_ramp <- rev(viridis(9, option = "inferno"))

UEA <- image_read(paste0(ROOT, "../Scripts/OUTREACH_PLOTTING/UEA_2016_Logo.png"))

# New facet label names for Year variable
# years_to_plot = c(final_year)
# years_to_plot.label = list()
# for (year_to_plot in years_to_plot) {
#   years_to_plot.label = c(years_to_plot.label, paste0(ref_year+1, " to ", year_to_plot))
# }
# 
# year_labeller <- function(variable,value){
#   return(years_to_plot.label[value])
# }

#//////////////////////////////////////////////////////////////////////////////#
#### Test Map ####
#//////////////////////////////////////////////////////////////////////////////#

ggplot() + 
  ggtitle("TEST MAP: Countries") + 
  geom_sf(data = country_borders, fill = "grey90", size= 0.25) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

ggplot() + 
  ggtitle("TEST MAP: Countries") + 
  geom_sf(data = country_borders_EU27_merged, fill = "grey90", size= 0.25) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION % ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMSTcontribution %>%
  subset(Gas == "3-GHG" &
           Year == final_year &
           Component == "Total")

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  mutate(Data = Data * 100) %>% # to % from fraction
  subset(!is.na(Data))

scaling_factor = 1
breaks = c(0.5,seq(1,5),7.5,10,15) * scaling_factor
labels = breaks / scaling_factor

max(df_thisplot$Data)

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.1, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtext::geom_richtext(aes(x=-125, y=-25, 
                            label = "Includes emissions of<br>**CO<sub>2</sub>**, **CH<sub>4</sub>** and **N<sub>2</sub>O**,<br> from both **fossil**<br>and **land use** sources"),
                        colour = "grey50") +
  ggtitle("National Contributions to Global Warming since 1850") + 
  labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
  scale_fill_gradientn(colours = inferno_colour_ramp, 
                    trans = "pseudo_log", 
                    breaks = breaks,
                    labels = labels
                    ) +
  guides(
    fill = guide_colorsteps(
      title = "Contribution as a percentage of the warming caused by all global emissions (%)",
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

filename = paste0(OUTDIR, "PLOTS_Map_GMST_contributions_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 5.5, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}

#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION % - EU27 Merged ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMSTcontribution %>%
  subset(Gas == "3-GHG" &
           Year == final_year &
           Component == "Total")

df_thisplot <- country_borders_EU27_merged %>%
  left_join(df_thisplot, by = "ISO3") %>%
  mutate(Data = Data * 100) %>% # to % from fraction
  subset(!is.na(Data))

scaling_factor = 1
breaks = c(0.1,0.5,seq(1,5),10,15) * scaling_factor
labels = breaks / scaling_factor

max(df_thisplot$Data)

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.1, color = NA, aes(fill = Data * scaling_factor)) + 
  ggtext::geom_richtext(aes(x=-125, y=-25, 
                            label = "The contribution of the EU27 countries<br>is aggregated in this figure"),
                        colour = "grey50", size = 3) +
  geom_sf(data = country_borders, size = 0.1, color = "black", fill = NA) + 
  ggtitle("Contributions to Global Warming since 1850") + 
  labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
  scale_fill_gradientn(colours = inferno_colour_ramp, 
                       trans = modulus_trans(0.5), 
                       breaks = breaks,
                       labels = labels
  ) +
  guides(
    fill = guide_colorsteps(
      title = "Contribution as a percentage of the warming caused by all global emissions (%)",
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

thisplot

filename = paste0(OUTDIR, "PLOTS_Map_GMST_contributions_ref", ref_year, "_map_EU27merged")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 5.5, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}


#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION *C ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas == "3-GHG" &
           Year == final_year &
           Component == "Total")

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  subset(!is.na(Data))

scaling_factor = 100
breaks = c(0.001, seq(0.01, 0.05, 0.01), 0.075, 0.1, 0.2, 0.25) * scaling_factor
labels = breaks / scaling_factor

thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.1, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtext::geom_richtext(aes(x=-125, y=-25, 
                            label = "Includes emissions of<br>**CO<sub>2</sub>**, **CH<sub>4</sub>** and **N<sub>2</sub>O**,<br> from both **fossil**<br>and **land use** sources"),
                        colour = "grey50") +
  ggtitle("National Contributions to Global Warming since 1850") + 
  labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
  scale_fill_gradientn(colours = inferno_colour_ramp, 
                    trans = "pseudo_log", 
                    breaks = breaks,
                    labels = labels) +
  guides(
    fill = guide_colorsteps(
      title = "Effect of Historical Emissions on Global Mean Surface Temperature (°C)",
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

filename = paste0(OUTDIR, "PLOTS_Map_GMST_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 5.5, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}
 

#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: LULUCF CONTRIBUTION *C ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas == "3-GHG" &
           Year == final_year &
           Component == "LULUCF")

df_thisplot <- country_borders %>%
  left_join(df_thisplot, by = "ISO3") %>%
  subset(!is.na(Data))

scaling_factor = 100
breaks = c(0.001,0.005, seq(0.01, 0.06, 0.01)) * scaling_factor
labels = breaks / scaling_factor


thisplot <- ggplot() + 
  geom_sf(data = df_thisplot, size = 0.1, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtext::geom_richtext(aes(x=-125, y=-25, 
                            label = "Includes emissions of<br>**CO<sub>2</sub>**, **CH<sub>4</sub>** and **N<sub>2</sub>O** from<br>**land use, land use change and<br>forestry (LULUCF)** only"),
                        colour = "grey50", size = 3) +
  ggtitle("National Contributions to Global Warming through LULUCF emissions since 1850") + 
  labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
  scale_fill_gradientn(colours = inferno_colour_ramp, 
                       trans = "pseudo_log", 
                       breaks = breaks,
                       labels = labels) +
  guides(
    fill = guide_colorsteps(
      title = "Effect of Historical LULUCF Emissions on Global Mean Surface Temperature (°C)",
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

filename = paste0(OUTDIR, "PLOTS_Map_GMST_LULUCF_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 5.5, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}


#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION FROM CO2 ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas %in% c("CO[2]", "3-GHG") &
           Year %in% final_year &
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
  geom_sf(data = df_thisplot, size = 0.1, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtext::geom_richtext(aes(x=-125, y=-25, 
                            label = "The contribution of the non-CO<sub>2</sub> gases<br>(**CH<sub>4</sub>** and **N<sub>2</sub>O**) to total warming varies<br>with the level of industrialisation,<br>agricultural area, population density,<br>energy mix, and many other factors"),
                        colour = "grey50", size = 3) +
  ggtitle(NULL) + 
  labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
  scale_fill_gradientn(colours = inferno_colour_ramp, 
                       #trans = "pseudo_log", 
                       breaks = breaks,
                       labels = labels) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  guides(
    fill = guide_colorsteps(
      title = bquote("Portion of Country's Overall Warming Contribution Related to"~CO[2]~"Emissions (%)"),
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

filename = paste0(OUTDIR, "PLOTS_Map_GMST_CO2fraction_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 5.5, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}


#//////////////////////////////////////////////////////////////////////////////#
#### PLOT: CONTRIBUTION FROM Agriculture ####
#//////////////////////////////////////////////////////////////////////////////#

df_thisplot <- df_GMST %>%
  subset(Gas == "3-GHG" &
           Year %in% final_year &
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
  geom_sf(data = df_thisplot, size = 0.1, color = "black", aes(fill = Data * scaling_factor)) + 
  ggtext::geom_richtext(aes(x=-125, y=-25, 
                            label = "Around **half** of the world's countries<br>have contributed more to warming through<br>emissions from **Land Use, Land**<br>**Use Change and Forestry (LULUCF)** than<br> through **fossil emissions**"),
                        colour = "grey50", size = 3) +
  ggtitle(NULL) + 
  labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
  scale_fill_gradientn(colours = inferno_colour_ramp, 
                       #trans = "pseudo_log", 
                       breaks = breaks,
                       labels = labels) +
  xlab("Longitude (°)") + ylab("Latitude (°)") +
  guides(
    fill = guide_colorsteps(
      title = bquote("Portion of Country's Overall Warming Contribution Related to Land Use Emissions (%)"),
      title.position = "top",
      barwidth = unit(0.6, "npc"),
      even.steps = TRUE, 
      show.limits = NULL)) +
  coord_sf(crs = st_crs("ESRI:54030"),
           default_crs=4326, 
           datum=4326, 
           xlim = c(-180, 180), ylim = c(-60, 80)) +
  scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
  scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
  map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 

filename = paste0(OUTDIR, "PLOTS_Map_GMST_LULUCFfraction_ref", ref_year, "_map")
for (extension in c("pdf", "png")) {
  ggsave(paste0(filename, ".", extension),
         height = 5.5, width = 10,
         plot = thisplot, device = extension, dpi = 400)
}
  

# #//////////////////////////////////////////////////////////////////////////////#
# #### PLOT: CONTRIBUTION per capita *C ####
# #//////////////////////////////////////////////////////////////////////////////#
# 
# df_thisplot <- df_GMST_percap %>%
#   subset(Gas == "3-GHG" &
#            Year == final_year &
#            Component == "Total")
# 
# df_thisplot <- country_borders %>%
#   left_join(df_thisplot, by = "ISO3") %>%
#   subset(!is.na(Data))
# 
# min(df_thisplot$Data)
# 
# scaling_factor = 1
# labels = c(10, 20, 50, 100, 200, 500, 1000)
# breaks = labels * scaling_factor
# 
# thisplot <- ggplot() + 
#   geom_sf(data = country_borders, size = NA, color = NA, fill = "grey90") + 
#   geom_sf(data = df_thisplot, size = 0.1, color = "black", aes(fill = Data * scaling_factor)) + 
#   ggtext::geom_richtext(aes(x=-125, y=-25, 
#                             label = "Includes emissions of<br>**CO<sub>2</sub>**, **CH<sub>4</sub>** and **N<sub>2</sub>O**,<br> from both **fossil**<br>and **land use** sources"),
#                         colour = "grey50") +
#   ggtitle("National Per-Capita Contributions to Global Warming since 1850") + 
#   labs(caption="Jones et al. (2023) doi:10.1038/s41597-023-02041-1") +
#   scale_fill_gradientn(colours = inferno_colour_ramp, 
#                        trans = "pseudo_log", 
#                        breaks = breaks,
#                        labels = labels) +
#   guides(
#     fill = guide_colorsteps(
#       title = "Trillionths of a °C per capita",
#       title.position = "top",
#       barwidth = unit(0.6, "npc"),
#       even.steps = TRUE, 
#       show.limits = NULL)) +
#   coord_sf(crs = st_crs("ESRI:54030"),
#            default_crs=4326, 
#            datum=4326, 
#            xlim = c(-180, 180), ylim = c(-60, 80)) +
#   scale_x_continuous(breaks = c(-180, 180), expand=c(1e-2,1e-2)) +
#   scale_y_continuous(breaks = c(-60, 80), expand=c(1e-4,1e-4)) +
#   map_theme_robinson + theme(legend.box.margin=margin(-15,0,0,0)) 
# 
# filename = paste0(OUTDIR, "PLOTS_Map_GMST_per_capita_ref", ref_year, "_map")
# for (extension in c("pdf", "png")) {
#   ggsave(paste0(filename, ".", extension),
#          height = 5.5, width = 10,
#          plot = thisplot, device = extension, dpi = 400)
# }
