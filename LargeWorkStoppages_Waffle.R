# setup --------------------------------------------------------------------------------------------

## load packages  
library(ggplot)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(crstools)
library(waffle)
library(extrafont)
library(showtext)
library(hrbrthemes)
library(emojifont)
library(openxlsx)
require(waffle)

## working directory ----------------------------------------------------------------------------------------------------------

setwd("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus")

## load data

detailed_stoppages = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/monthly-listing-detailed.xlsx", sheet = 1) 
stoppages_2023 = read.xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/work-stoppages-2023.xlsx", sheet = 'Table_1') 



## edit stoppages 2023

  colnames(stoppages_2023) = c("Organizations involved",
                               "States",
                               "Areas",
                               "Ownership",
                               "Industry code",
                               "Union",
                               "Union acronym",
                               "Union local",
                               "Work stoppage beginning date",
                               "Work stoppage ending date",
                               "Number of workers",
                               "Number of workdays",
                               "Days idle, cumulative for this work stoppage",
                               "Notes")
  
  stoppages_2023 = stoppages_2023[-1,] %>%
    filter(!is.na(`Industry code`))
  
  stoppages_2023 = stoppages_2023 %>%
    mutate(`Work stoppage beginning date` = janitor::excel_numeric_to_date(as.numeric(`Work stoppage beginning date`)),
           `Work stoppage ending date` = janitor::excel_numeric_to_date(as.numeric(`Work stoppage ending date`))) %>%
    mutate(`Number of workers`= as.numeric(`Number of workers`),
           `Days idle, cumulative for this work stoppage` = as.numeric(`Days idle, cumulative for this work stoppage`)) 
  
## edit detailed stoppages
  
  colnames(detailed_stoppages) = c("Organizations involved",
                                   "States",
                                   "Areas",
                                   "Ownership",
                                   "Industry code",
                                   "Union",
                                   "Union acronym",
                                   "Union local",
                                   "Bargaining unit",
                                   "Work stoppage beginning date",
                                   "Work stoppage ending date",
                                   "Number of workers",
                                   "Days idle, cumulative for this work stoppage",
                                   "Notes")
  
  detailed_stoppages = detailed_stoppages[-1,] %>%
    filter(!is.na(`Industry code`))

detailed_stoppages = detailed_stoppages %>%
  mutate(`Work stoppage beginning date` = janitor::excel_numeric_to_date(as.numeric(`Work stoppage beginning date`)),
         `Work stoppage ending date` = janitor::excel_numeric_to_date(as.numeric(`Work stoppage ending date`)),
         `Number of workers`= as.numeric(`Number of workers`),
         `Days idle, cumulative for this work stoppage` = as.numeric(`Days idle, cumulative for this work stoppage`))
 

naics = read_csv("naics_codes.csv")

new_row = c(level = 1, code = 0, name = "Other", notes = NA)
naics = naics %>%
  rbind(new_row) %>%
  mutate(code = as.numeric(code))

## filter for 2022 strikes and create total ----

detailed_stoppages_base = plyr::rbind.fill(detailed_stoppages, stoppages_2023)

#### first plot and related ----------------------------------------------------------------------------------------

detailed_stoppages_filtered = detailed_stoppages_base %>%
  filter(`Work stoppage beginning date` > as.Date("2015-12-31")) %>%
  mutate(Year = as.numeric(format(`Work stoppage beginning date`,'%Y'))) %>%
  mutate(industrycode_major = substr(`Industry code`, 1, 2)) %>%
  mutate(industrycode_major = ifelse(industrycode_major %in% c(72, 62, 61, 56, 52, 51, 31, 23, 22, 21, 11), industrycode_major, 0)) %>%
  group_by(Year, industrycode_major) %>%
  summarize(totalblocks = round(sum(`Days idle, cumulative for this work stoppage`)/25000)) %>%
  mutate(industrycode_major = as.numeric(industrycode_major)) %>%
  inner_join(naics, 
             by = c("industrycode_major" = "code")) %>%
 arrange(desc(industrycode_major))


## plot by industry --------------------------------------------------------------------------------------------

png("Graphs/RecentIdleDays_Waffle_11.15.2023.png", width = 8, height = 4, res = 300, units = "in", type = 'cairo-png')

ggplot(detailed_stoppages_filtered,
       aes(values = totalblocks,
           fill = name)) +
  
  waffle::geom_waffle(n_rows = 10,
                      size = 0.3,
                      tile_shape = c('circle'),
                      colour = "white",
                      flip = TRUE,
                      show.legend = TRUE) +
  
  scale_fill_manual(values = c("Other" = crs_get_palette()$grey3,
    
                               "Agriculture, Forestry, Fishing and Hunting" = crs_get_palette()$blgray4,
                               
                               "Mining, Quarrying, and Oil and Gas Extraction" = crs_get_palette()$orange3,
                               "Utilities" = crs_get_palette()$orange4,
                               "Construction" = crs_get_palette()$orange5,
                               
                               "Information and Cultural Industries" = crs_get_palette()$green3,
                               "Finance and Insurance" = crs_get_palette()$green4,
                               "Administrative and Support, Waste Management and Remediation Services"= crs_get_palette()$green5,
                               
                               "Educational Services" = crs_get_palette()$bluea4,
                               "Health Care and Social Assistance" = crs_get_palette()$bluea5,
                               
                               "Accommodation and Food Services" = crs_get_palette()$purple4),
                    breaks = c("Other",
                               "Agriculture, Forestry, Fishing and Hunting",
                               
                               
                               "Mining, Quarrying, and Oil and Gas Extraction",
                               "Utilities",
                               "Construction",
                               
                               "Information and Cultural Industries",
                               "Finance and Insurance",
                               "Administrative and Support, Waste Management and Remediation Services",
                               
                               "Educational Services",
                               "Health Care and Social Assistance",
                               
                               "Accommodation and Food Services")) +
  
  # section of legend information ----
   # scale_fill_manual(values = c(# under 100 BLACK
   #                              "Health Care and Social Assistance" = crs_get_palette()$blgray4,
   #                              
   #                              # 100s ORANGE
   #                              "Forestry and Logging" = crs_get_palette()$orange4,
   #                              
   #                              # 200s  GREEN
   #                              "Mining and Quarrying (except Oil and Gas)" = crs_get_palette()$green3,
   #                              "Utilities" = crs_get_palette()$green4,
   #                              "Heavy and Civil Engineering Construction" = crs_get_palette()$green5,
   #                              "Specialty Trade Contractors" = crs_get_palette()$green6,
   #                              
   #                              # 300 YELLOW
   #                              "Food Manufacturing" = crs_get_palette()$yellow3,
   #                              "Fabricated Metal Product Manufacturing" = crs_get_palette()$yellow4,
   #                              "Machinery Manufacturing" = crs_get_palette()$yellow5,
   #                              "Transportation Equipment Manufacturing" = crs_get_palette()$yellow6,
   #                               
   #                              # 400s RED
   #                              "Motor Vehicle and Parts Dealers" = crs_get_palette()$red2,
   #                              "Food Services and Drinking Places" = crs_get_palette()$red3,
   #                              "Food and Beverage Stores" = crs_get_palette()$red4,
   #                              "Transit and Ground Passenger Transportation" = crs_get_palette()$red5,
   #                              "Support Activities for Transportation" = crs_get_palette()$red6,
   #                              
   #                              # 500s 
   #                              "Publishing Industries (except Internet)" = crs_get_palette()$purple3,
   #                              "Telecommunications" = crs_get_palette()$purple4,
   #                              "Administrative and Support Services" = crs_get_palette()$purple5,
   #                              
   #                              # 600s BLUE A
   #                              "Ambulatory Health Care Services" = crs_get_palette()$bluea2,
   #                              "Educational Services" = crs_get_palette()$bluea3,
   #                              "Hospitals" = crs_get_palette()$bluea4,
   # 
   #                              # 700s OLIVE
   #                              "Accommodation Services" = crs_get_palette()$olive3,
   #                              "Food Services and Drinking Places" = crs_get_palette()$olive4),
   #                   
   #                   breaks = c(# under 100 BLACK
   #                     "Health Care and Social Assistance" ,
   #                     
   #                     # 100s BLUE B
   #                     "Forestry and Logging",
   #                     
   #                     # 200s  GREEN
   #                     "Mining and Quarrying (except Oil and Gas)" ,
   #                     "Utilities" ,
   #                     "Heavy and Civil Engineering Construction",
   #                     "Specialty Trade Contractors",
   #                     
   #                     # 300 YELLOW
   #                     "Food Manufacturing" ,
   #                     "Fabricated Metal Product Manufacturing" ,
   #                     "Machinery Manufacturing" ,
   #                     "Transportation Equipment Manufacturing" ,
   #                     
   #                     # 400s RED
   #                     "Motor Vehicle and Parts Dealers" ,
   #                     "Food Services and Drinking Places" ,
   #                     "Food and Beverage Stores" ,
   #                     "Transit and Ground Passenger Transportation" ,
   #                     "Support Activities for Transportation",
   #                     
   #                     # 500s 
   #                     "Publishing Industries (except Internet)" ,
   #                     "Telecommunications" ,
   #                     "Administrative and Support Services" ,
   #                     
   #                     # 600s BLUE A
   #                     "Ambulatory Health Care Services" ,
   #                     "Educational Services" ,
   #                     "Hospitals",
   #                     
   #                     # 700s OLIVE
   #                     "Accommodation Services" ,
   #                     "Food Services and Drinking Places" )) +
  # post legend section ----
  
  #labs(xlab = "1 square = 10,000 idle hours")  +

  coord_equal() + 
  
  facet_wrap(~Year, nrow = 1) +
  
  geom_rect(data = detailed_stoppages_filtered %>% 
              filter(Year == 2020),
            aes(xmin=9.5,xmax=10.5,
                ymin=0.5,ymax=1.5),
            fill = NA,
            color = 'black',
            linewidth = 0.25,
            alpha = 0.2)+  
  
  geom_curve(data = detailed_stoppages_filtered %>% 
               filter(Year == 2020),
             mapping = aes(x = 8,
                           xend = 10,
                           y = -1.25,
                           yend = 0.1),
             curvature = 0.3,
             size = 0.25,
             arrow = arrow(length = unit(0.03, "npc"))) + 
  
  geom_text(data = detailed_stoppages_filtered %>% 
              filter(Year == 2020),
            x = 4, y = -2,
            label = '25,000 idle hours',
            size = 5,
            hjust = 0.35,
            fontface='bold')+
  
  theme(#plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.2, size = 24),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    # panel
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    # strip
    strip.background = element_blank(),
    strip.text = element_text(size = 26, face = "bold"),
    # axis lines
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    # legend
    legend.position = "right",
    legend.title = element_blank(),
    legend.key.size = unit(0.33, 'cm'),
    legend.title.align = 0.5,
    legend.text = element_text(size = 18))

dev.off()






## plot by large strikes -------------------------------------------------------------------------------------------------------------

blocksforallstrikes = detailed_stoppages_base %>%
  filter(`Work stoppage beginning date` > as.Date("2015/12/31", format = "%Y/%m/%d")) %>%
  mutate(Year = as.numeric(format(`Work stoppage beginning date`,'%Y'))) %>%
  mutate(totalblocks = round(`Days idle, cumulative for this work stoppage`/25000),
         uniquename = paste0(`Organizations involved`, `Union`)) %>%
  arrange(desc(totalblocks))

blocksforallstrikes = detailed_stoppages_base %>%
  filter(`Work stoppage beginning date` > as.Date("2015-12-31")) %>%
  mutate(Year = as.numeric(format(`Work stoppage beginning date`,'%Y'))) %>%
  mutate(uniquename = paste0(`Organizations involved`, `Union`)) %>%
  mutate(uniquename = ifelse(uniquename %in% c("Verizon Communications Inc.Communications Workers of America and International Brotherhood of Electrical Workers",
                                               "Charter Communications Inc.International Brotherhood of Electrical Workers",
                                               "Arizona State LegislatureArizona Education Association",
                                               "Oklahoma State LegislatureOklahoma Education Association",
                                               "General MotorsUnited Automobile Workers",
                                               "Chicago Public SchoolsChicago Teachers Union and Service Employees International Union",
                                               "University of CaliforniaUnited Auto Workers",
                                               "The Alliance of Motion Picture and Television ProducersScreen Actors Guild - American Federation of Television and Radio Artists ",
                                               "Alliance of Motion Picture and Television ProducersWriters Guild of America West, Writers Guild of America East",
                                               "Ford Motor Co., General Motors Co., Mack Trucks, and StellantisUnited Auto Workers"),
                             uniquename,
                             "Other")) %>%
  group_by(Year,
           uniquename) %>%
  summarize(`Days idle, cumulative for this work stoppage` = sum(`Days idle, cumulative for this work stoppage`)) %>%
  mutate(totalblocks = round(`Days idle, cumulative for this work stoppage`/25000)) %>%
  arrange(desc(totalblocks)) %>%
  arrange(str_detect(uniquename, 'Other'))

############################################

png("Graphs/LargeStrikes_1.24.2024.png", width = 8, height = 7, res = 300, units = "in", type = 'cairo-png')
  
  ggplot(blocksforallstrikes,
         aes(values = totalblocks,
             fill = uniquename)) +
  
  waffle::geom_waffle(n_rows = 10,
                      size = 0.4,
                      #tile_shape = c('circle'),
                      colour = "white",
                      flip = TRUE,
                      show.legend = TRUE) +
    
    geom_rect(data = blocksforallstrikes %>% 
                filter(Year == 2020),
              aes(xmin=9.5,xmax=10.5,
                  ymin=0.5,ymax=1.5),
              fill = NA,
              color = 'black',
              linewidth = 0.275,
              alpha = 0.2)+  
    
    geom_curve(data = blocksforallstrikes %>% 
                    filter(Year == 2020),
               mapping = aes(x = 8,
                             xend = 10,
                             y = -1.25,
                             yend = 0.1),
               curvature = 0.3,
               size = 0.375,
               arrow = arrow(length = unit(0.03, "npc"))) + 
    
    geom_text(data = blocksforallstrikes %>% 
                filter(Year == 2020),
              x = 4, y = -2.1,
              label = str_wrap('25,000 idle days',10),
              size = 12,
              hjust = 0.4,
              lineheight = 0.25,
              fontface='bold')+
    
  scale_fill_manual(values = c("Other" = crs_get_palette()$grey5,
                               "Charter Communications Inc.International Brotherhood of Electrical Workers" = crs_get_palette()$red4,
                               "General MotorsUnited Automobile Workers" = crs_get_palette()$orange4,
                               "University of CaliforniaUnited Auto Workers" = crs_get_palette()$green3,
                               "Verizon Communications Inc.Communications Workers of America and International Brotherhood of Electrical Workers" = crs_get_palette()$purple4,
                               "Arizona State LegislatureArizona Education Association" = crs_get_palette()$yellow5,
                               "The Alliance of Motion Picture and Television ProducersScreen Actors Guild - American Federation of Television and Radio Artists " = crs_get_palette()$blueb4,
                               "Alliance of Motion Picture and Television ProducersWriters Guild of America West, Writers Guild of America East" = "pink",
                               "Ford Motor Co., General Motors Co., Mack Trucks, and StellantisUnited Auto Workers" = crs_get_palette()$orange5,
                               "Oklahoma State LegislatureOklahoma Education Association" = crs_get_palette()$green6,
                               "Chicago Public SchoolsChicago Teachers Union and Service Employees International Union" = crs_get_palette()$purple5),
                    labels =  c("Other" = "Other",
                                "Charter Communications Inc.International Brotherhood of Electrical Workers" = "IBEW Charter Comms.",
                                "General MotorsUnited Automobile Workers" = "UAW General Motors",
                                "University of CaliforniaUnited Auto Workers" = "UAW Univ. of California",
                                "Verizon Communications Inc.Communications Workers of America and International Brotherhood of Electrical Workers" = "IBEW & CWA Verizon",
                                "Arizona State LegislatureArizona Education Association" = "Arizona Educ. Assc.",
                                "The Alliance of Motion Picture and Television ProducersScreen Actors Guild - American Federation of Television and Radio Artists " = "Actors & TV/Radio Artists",
                                "Alliance of Motion Picture and Television ProducersWriters Guild of America West, Writers Guild of America East" = "Writers Guild",
                                "Ford Motor Co., General Motors Co., Mack Trucks, and StellantisUnited Auto Workers" = "UAW Big-3 & Mack",
                                "Oklahoma State LegislatureOklahoma Education Association" = "Oklahoma Educ. Assc.",
                                "Chicago Public SchoolsChicago Teachers Union and Service Employees International Union" = "Chicago Teachers"),
                    breaks = c("Verizon Communications Inc.Communications Workers of America and International Brotherhood of Electrical Workers",
                               "Arizona State LegislatureArizona Education Association",
                               "General MotorsUnited Automobile Workers",
                               "University of CaliforniaUnited Auto Workers",
                               "Alliance of Motion Picture and Television ProducersWriters Guild of America West, Writers Guild of America East",
                               
                               
                               "Charter Communications Inc.International Brotherhood of Electrical Workers",
                               
                               
                               
                               "Oklahoma State LegislatureOklahoma Education Association",
                               
                               "Chicago Public SchoolsChicago Teachers Union and Service Employees International Union",
                               
                               "The Alliance of Motion Picture and Television ProducersScreen Actors Guild - American Federation of Television and Radio Artists ",
                               
                               "Ford Motor Co., General Motors Co., Mack Trucks, and StellantisUnited Auto Workers",
                               "Other")) +
    
    #labs(xlab = "1 square = 10,000 idle days")  +
    
    guides(fill = guide_legend(ncol = 5, byrow = TRUE)) +
    
    coord_equal() + 
    
    facet_wrap(~Year, nrow = 1) +
    
    theme(#plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.2, size = 24),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      # panel
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.background = element_blank(),
      # strip
      strip.background = element_blank(),
      strip.text = element_text(size = 36, face = "bold"),
      # axis lines
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      # legend
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = unit(0.33, 'cm'),
      legend.title.align = 0.5,
      legend.box.margin = margin(0, 0, 0, 0),
      legend.text = element_text(size = 29))
    
dev.off()


