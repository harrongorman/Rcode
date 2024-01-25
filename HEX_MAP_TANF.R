

### load required packages ###

library(ggplot2)
library(dplyr)
library(readxl)
library(crstools)
library(ggtext)
library(generics)
library(rgeos)
library(geojsonio)
library(tidyverse)
library(readxl)
library(broom)
library(rgeos)
library(usdata)
library(tidycensus)
library(covidcast)
library(ggpubr)

### working directory ###

setwd("S:/DSP_DOCUMENTS/RAs/GORMAN/Falk, Gene/Rethinking TANF")

### download TANF eligibility data ###

TANF_REC = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Falk, Gene/Rethinking TANF/Data/1.24.2024.xlsx", sheet = 2)

TANF_REC = TANF_REC %>%
  mutate(fipsstatecode = ifelse(nchar(fipsstatecode) == 1, paste0(0,fipsstatecode), fipsstatecode)) %>%
  mutate(stateabb = covidcast::fips_to_abbr(as.character(fipsstatecode))) %>%
  mutate(statename = abbr2state(stateabb))



TANF_REC = TANF_REC[order(TANF_REC$stateabb),]

# HEX GEO JSON ------------------------------------------------------------

spdf <- geojson_read("Data/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name") 

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2)) 

centers = centers[order(centers$id),]

centers = centers %>%
  mutate(total = TANF_REC$rec)

spdf_fortified2 <- left_join(spdf_fortified,TANF_REC,by = c('id' = 'statename')) 

rm(spdf,spdf_fortified)



# PLOT HEX TANF ELIG ------------------------------------------------------

png("Graphs/TANF_amongunder200&TANFeligible_receipt_1.24.2024.png", width = 6, height = 4, res = 300, units = "in", type = 'cairo-png')

ggplot()+
  geom_polygon(data = spdf_fortified2, 
               aes(x = long, y = lat, group = group, fill = rec), color = crs_get_palette()$grey4)+
  
  scale_fill_gradient2(low = crs_get_palette()$olive1,
                       mid = crs_get_palette()$olive4,
                       high = crs_get_palette()$olive6,
                       midpoint = 23) +
  
  geom_text(data=centers, aes(x=x, y=y, label=paste0(id, "\n", total, "%")),
            color = "white",
            size = 9.2,
            lineheight = 0.28,
            fontface = 'bold',
            show.legend = F) +
  
  labs(fill = "Medicaid Coverage") +
  
  coord_map()+
  
  theme_void()+
  
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.2, size = 24),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.size = unit(0.81, 'cm'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 10,face = "bold"))

dev.off()

#########################################################
#### overall percent of receipt ###### ##################
#########################################################

TANF_UNIC_REC = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Falk, Gene/Rethinking TANF/Data/1.24.2024.xlsx", sheet = 3)

TANF_UNIC_REC = TANF_UNIC_REC %>%
  mutate(fipsstatecode = ifelse(nchar(fipsstatecode) == 1, paste0(0,fipsstatecode), fipsstatecode)) %>%
  mutate(stateabb = covidcast::fips_to_abbr(as.character(fipsstatecode))) %>%
  mutate(statename = abbr2state(stateabb))



TANF_UNIC_REC = TANF_UNIC_REC[order(TANF_UNIC_REC$stateabb),]

# HEX GEO JSON ------------------------------------------------------------

spdf <- geojson_read("Data/us_states_hexgrid.geojson",  what = "sp")

spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name") 

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2)) 

centers = centers[order(centers$id),]

centers = centers %>%
  mutate(total = TANF_UNIC_REC$rec)

spdf_fortified2 <- left_join(spdf_fortified,TANF_UNIC_REC,by = c('id' = 'statename')) 

rm(spdf,spdf_fortified)



# PLOT HEX TANF ELIG ------------------------------------------------------

png("Graphs/TANF_univ_receipt_1.24.2024.png", width = 6, height = 4, res = 300, units = "in", type = 'cairo-png')

ggplot()+
  geom_polygon(data = spdf_fortified2, 
               aes(x = long, y = lat, group = group, fill = rec), color = crs_get_palette()$grey4)+
  
  scale_fill_gradient2(low = crs_get_palette()$green1,
                       mid = crs_get_palette()$green4,
                       high = crs_get_palette()$green6,
                       midpoint = 6.2) +
  
  geom_text(data=centers, aes(x=x, y=y, label=paste0(id, "\n", total, "%")),
            color = "white",
            size = 9.2,
            lineheight = 0.28,
            fontface = 'bold',
            show.legend = F) +
  
  labs(fill = "Medicaid Coverage") +
  
  coord_map()+
  
  theme_void()+
  
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.2, size = 24),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.size = unit(0.81, 'cm'),
        legend.title.align = 0.5,
        legend.text = element_text(size = 10,face = "bold"))

dev.off()













