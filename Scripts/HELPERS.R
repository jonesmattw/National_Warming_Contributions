library(tidyr)
library(ggplot2)
library(lemon)
library(scales)
library(ggpubr)

fixed_ISO3s <- c('GLOBAL', 'ANNEXI', 'ANNEXII', 'OECD', 'EIT', 'BASIC', 'LDC', 'LMDC', 'USA', 'CHN', 'EU27', 'RUS', 'BRA', 'IND', 'IDN')

fixed_component_order <- c('Fossil', 'LULUCF' , 'Total')

# Add a "Gas_unit" to be used for labelling the plots
CO2_expression <- "CO[2]"
CH4_expression <- "CH[4]"
N2O_expression <- "N[2]*O"
GHG3_expression <- "3-GHG"
fixed_gas_order = c(CO2_expression, CH4_expression, N2O_expression, GHG3_expression)

# Add a "Gas_unit" to be used for labelling the plots
CO2_unit <- "Pg~CO[2]~year^-1"
CH4_unit <- "Tg~CH[4]~year^-1"
N2O_unit <- "Tg~N[2]*O~year^-1"
GHG3_unit <- "Pg~CO[2]*-e[100]~year^-1"
fixed_unit_order = c(CO2_unit, CH4_unit, N2O_unit, GHG3_unit)

CO2_unit_CUMULATIVE <- "Pg~CO[2]"
CH4_unit_CUMULATIVE <- "Tg~CH[4]"
N2O_unit_CUMULATIVE <- "Tg~N[2]*O"
GHG3_unit_CUMULATIVE <- "Pg~CO[2]*-e[100]"
fixed_unit_order_CUMULATIVE = c(CO2_unit_CUMULATIVE, CH4_unit_CUMULATIVE, N2O_unit_CUMULATIVE, GHG3_unit_CUMULATIVE)

CO2_unit_CUMULATIVE_CO2e <- "Pg~CO[2]"
CH4_unit_CUMULATIVE_CO2e <- "Pg~CO[2]*-e[100]"
N2O_unit_CUMULATIVE_CO2e <- "Pg~CO[2]*-e[100]"
GHG3_unit_CUMULATIVE_CO2e <- "Pg~CO[2]*-e[100]"
fixed_unit_order_CUMULATIVE_CO2e = c(CO2_unit_CUMULATIVE_CO2e, CH4_unit_CUMULATIVE_CO2e, N2O_unit_CUMULATIVE_CO2e, GHG3_unit_CUMULATIVE_CO2e)

df_UNITS <- as.data.frame(cbind(Gas = fixed_gas_order, 
                                Unit = fixed_unit_order,
                                Unit_CUMULATIVE = fixed_unit_order_CUMULATIVE,
                                Unit_CUMULATIVE_CO2e = fixed_unit_order_CUMULATIVE_CO2e))
df_UNITS$Gas_Unit <- paste0(df_UNITS$Gas, "~(", df_UNITS$Unit,")")
df_UNITS$Gas_Unit_CUMULATIVE <- paste0(df_UNITS$Gas, "~(", df_UNITS$Unit_CUMULATIVE,")")
df_UNITS$Gas_Unit_CUMULATIVE_CO2e <- paste0(df_UNITS$Gas, "~(", df_UNITS$Unit_CUMULATIVE_CO2e,")")

fixed_gas_unit_order = df_UNITS$Gas_Unit
fixed_gas_unit_order_CUMULATIVE = df_UNITS$Gas_Unit_CUMULATIVE
fixed_gas_unit_order_CUMULATIVE_CO2e = df_UNITS$Gas_Unit_CUMULATIVE_CO2e

theme <- theme_bw() + theme(
  axis.title = element_text(size = 12, colour = "black"),
  axis.text = element_text(size = 12, colour = "black"),
  axis.line = element_line(size = 0.25),
  
  panel.border = element_blank(),
  
  plot.margin = margin(t = 0.5, r = 0.75, b = 0.5, l = 0.25, unit = "cm"),
  
  legend.title=element_blank(),
  legend.text=element_text(size = 12),
  legend.position = "bottom",
  legend.box = "vertical", 
  legend.spacing.y = unit(0, "cm"),
  
  strip.placement = "outside",
  strip.background = element_blank(),
  strip.text = element_text(size = 12)
)

colour_custom = colorRampPalette(c(
  #'#e6194B', '#3cb44b', '#ffe119', '#4363d8', '#f58231', '#42d4f4', '#f032e6', '#fabed4', '#469990', '#dcbeff', '#9A6324', '#fffac8', '#800000', '#aaffc3', '#000075'
  '#800000', '#e6194B', '#f58231', '#808000', '#ffe119', '#bfef45', '#3cb44b', '#469990', '#42d4f4', '#000075', '#4363d8', '#911eb4', '#f032e6'
))(length(fixed_ISO3s)-1)
colour_custom = c('black', colour_custom)

# Component labeller
component_labels <- c(
  "Fossil" = "Fossil Sources",
  "LULUCF" = "LULUCF Sources",
  "Total" = "Total"
)

year_breaks <- function(y0,yN) {
  range = yN-y0
  if (range <= 10) {
    y0 <- plyr::round_any(y0, 2, floor)
    yN <- plyr::round_any(yN, 2, ceiling)
    breaks <- seq(y0, yN, 2)
  } else if (range <= 25) {
    y0 <- plyr::round_any(y0, 5, floor)
    yN <- plyr::round_any(yN, 5, ceiling)
    breaks <- seq(y0, yN, 5)
  } else if (range <= 50) {
    y0 <- plyr::round_any(y0, 10, floor)
    yN <- plyr::round_any(yN, 10, ceiling)
    breaks <- seq(y0, yN, 10)
  } else if (range <= 100) {
    y0 <- plyr::round_any(y0, 20, floor)
    yN <- plyr::round_any(yN, 20, ceiling)
    breaks <- seq(y0, yN, 20)
  } else if (range <= 250) {
    y0 <- plyr::round_any(y0,50, floor)
    yN <- plyr::round_any(yN, 50, ceiling)
    breaks <- seq(y0, yN, 50)
  } else if (range > 250) {
    y0 <- plyr::round_any(y0, 50, floor)
    yN <- plyr::round_any(yN, 50, ceiling)
    breaks <- seq(y0, yN, 50)
  }
  return(breaks)
}

year_limits <- function(y0,yN) {
  range = yN-y0
  if (range <= 10) {
    y0 <- plyr::round_any(y0, 1, floor)
    yN <- plyr::round_any(yN, 1, ceiling)
  } else if (range <= 25) {
    y0 <- plyr::round_any(y0, 1, floor)
    yN <- plyr::round_any(yN, 1, ceiling)
  } else if (range <= 50) {
    y0 <- plyr::round_any(y0, 5, floor)
    yN <- plyr::round_any(yN, 5, ceiling)
  } else if (range <= 100) {
    y0 <- plyr::round_any(y0, 10, floor)
    yN <- plyr::round_any(yN, 10, ceiling)
  } else if (range <= 250) {
    y0 <- plyr::round_any(y0, 25, floor)
    yN <- plyr::round_any(yN, 25, ceiling)
  } else if (range > 250) {
    y0 <- plyr::round_any(y0, 50, floor)
    yN <- plyr::round_any(yN, 50, ceiling)
  }
  return(c(y0,yN))
}


y_axis_upper <- function(max) {
  scalar <- 10^(ceiling(log10(max)))
  max <- max / scalar
  if (max >= 0.5) {
    upper <- plyr::round_any(max, 0.2, ceiling)
  } else if (max >= 0.3) {
    upper <- plyr::round_any(max, 0.1, ceiling) 
  } else if (max >= 0.2) {
    upper <- plyr::round_any(max, 0.05, ceiling) 
  } else if (max >= 0.12) {
    upper <- plyr::round_any(max, 0.05, ceiling)
  } else if (max >= 0.1) {
    upper <- plyr::round_any(max, 0.02, ceiling)
  } else {
    upper <- plyr::round_any(max, 0.01, ceiling) 
  }
  upper <- upper * scalar
  return(upper)
}

y_axis_lower <- function(max, min) {
  scalar <- 10^(ceiling(log10(max)))
  max <- max / scalar
  min <- min / scalar
  if (max >= 0.5) {
    lower <- plyr::round_any(min, 0.2, floor)
  } else if (max >= 0.3) {
    lower <- plyr::round_any(min, 0.1, floor)
  } else if (max >= 0.2) {
    lower <- plyr::round_any(min, 0.05, floor) 
  } else if (max >= 0.12) {
    lower <- plyr::round_any(min, 0.025, floor)
  } else if (max >= 0.1) {
    lower <- plyr::round_any(min, 0.02, floor) 
  } else {
    lower <- plyr::round_any(min, 0.01, floor) 
  } 
  lower <- lower * scalar
  return(lower)
}

 
