
### load packages
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(crstools)
library(ggforce)
library(ggtext)
library(scales)

### set working directory
setwd("S:/DSP_DOCUMENTS/RAs/GORMAN/Falk, Gene/Rethinking TANF")

limits = read_xlsx("Data/2018FPL_R.xlsx")

limits = limits[order(limits$PercentFPL, decreasing = TRUE),]

### create figure
png("Graphs/TANF_elig_limits_1.23.2024.png", width = 5, height = 6, res = 300, units = "in", type = 'cairo-png')

ggplot() +
  
  geom_bar(limits,
            mapping = aes(x = reorder(State, PercentFPL),
                          y = PercentFPL),
           stat = "identity",
           width = 0.25,
           color = NA,
           fill = crs_get_palette()$bluea3) +
  
  geom_segment(limits,
               mapping = aes(x = State,
                             xend = State,
                             y = PercentFPL - 0.08,
                             yend = PercentFPL),
               size = 3.15,
               color = crs_get_palette()$bluea3) +
  
  geom_point(limits,
             mapping = aes(x = State,
                             y = PercentFPL),
                             color = crs_get_palette()$bluea3,
             size = 2.4,
             show.legend = FALSE) +
  
  geom_point(limits,
             mapping = aes(x = State,
                           y = PercentFPL-0.08),
             size = 2.4,
             color = crs_get_palette()$bluea3,
             show.legend = FALSE) +
  
  geom_text(limits,
            mapping = aes(x = State,
                          y = PercentFPL-0.04,
                          label = label_dollar()(Limit)),
            hjust = 0.5,
            size = 1.675,
            color = crs_get_palette()$grey4,
            fontface = "bold") +
  
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 2, 0.25),
                     limit = c(0, 2)) +
  
  ylab("Percent of Federal Poverty Level") +
  
  coord_flip() +
  
  crs_theme(
    
    # axis labels
    
    axis.title.y = element_blank(),
    axis.title.x = element_text(size = 8.5),
    
    # axis text
    axis.text.y = element_text(size = 7.5, margin = margin(0,0,0,0)),
    axis.text.x = element_text(size = 7.5),
    
    # grids
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank())


dev.off()
