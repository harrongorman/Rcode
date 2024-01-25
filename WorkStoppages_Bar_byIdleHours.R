
# setup --------------------------------------------------------------------------------------------

  ## load packages  
  library(ggplot2)
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(crstools)
  library(readxl)

  ## working directory

  setwd("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus")
  
  ## load data

  monthly_stoppages_history = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/annual-listing-workstoppages-1947-2022.xlsx", sheet = 1)
  
  union_density_1983 = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/uniondensity_1983.xlsx", sheet = 1)%>%
    mutate(line_scale = "CPS Union Density Estimates")
  
  union_density = read_xlsx("S:/DSP_DOCUMENTS/RAs/Former RAs/ROMERO/Bradley, David/Union Membership/Freeman Union Data.xlsx", sheet = 1) %>%
    mutate(line_scale = "Richard Freeman Union Density Estimates")
  
  stoppages_2023 = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/work-stoppages-2023.xlsx", sheet = 'Table_1') 
  
  union_density_percentages = union_density %>%
    mutate(percent = Members/`Nonag Employment`)
  
# clean history data --------------------------------------------------------------------------------------------

  monthly_stoppages_history_clean = monthly_stoppages_history[-c(2, seq(79,83,1)),]
  
  
  monthly_stoppages_history_2 = monthly_stoppages_history_clean %>%
    row_to_names(row_number = 1) %>%
    clean_names() %>%
    rename(Year = year,
           stoppages_beginning = number_of_work_stoppages_in_the_period,
           stoppages_ineffect = na,
           workers_beginning = number_of_workers_in_thousands_in_the_period_1,
           workers_ineffect = na_2,
           idleness_days =  days_of_idleness_2,
           percent_of_totalworkingtime = na_3)
  
  
  ## clean up stoppages 2023
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
  
  ### add 2023 data to the 
  
  stoppages_2023_sums = data.frame(Year = c(2023),
                                   stoppages_beginning = c(NA),
                                   stoppages_ineffect = c(NA),
                                   workers_beginning = c(NA),
                                   workers_ineffect = c(NA),
                                   idleness_days = c(16807),    ## stoppages 2023 total minus the one that started in 2022
                                   percent_of_totalworkingtime = c(NA))
  
  ### turn all [4] into NAs
  
  monthly_stoppages_history_2[monthly_stoppages_history_2 == "[4]"] = NA
  
  ### turn all [5] into 0.005
  
  monthly_stoppages_history_2[monthly_stoppages_history_2 == "[5]"] = "0.005"
  
  ## turn all values into numerics
  
  monthly_stoppages_history_2 <- monthly_stoppages_history_2 %>% 
    mutate_if(is.character, as.numeric)
  
  ## bind in 2023
  
  monthly_stoppages_history_2 = rbind(monthly_stoppages_history_2, stoppages_2023_sums)
  
  #monthly_stoppages_history_2[76,6] = 2194.5 + 187.2
  
# plot idle days --------------------------------------------------------------------------------------------
  
  png("Graphs/Idledays_1.18.2023.png", width = 6, height = 4, res = 300, units = "in", type = 'cairo-png')
  
    ggplot() +
      
    geom_col(monthly_stoppages_history_2,
            mapping = aes(x = Year,
                          y = idleness_days),
            width = 1,
            fill = crs_get_palette()$red4,
            color = NA) +
    
    geom_line(union_density_percentages %>% filter(Year > 1946),
              mapping = aes(x = Year,
              y = percent*150000,
              linetype = line_scale),
              show.legend = TRUE) +
      
    geom_line(union_density_1983,
                mapping = aes(x = as.numeric(Year),
                              y = as.numeric(Percent)*150000,
              linetype = line_scale),
              show.legend = TRUE) +
      
    geom_curve(mapping = aes(x = 1959,
               xend = 1980,
               y = 62000,
               yend = 50000),
               curvature = -0.3,
               arrow = arrow(length = unit(0.03, "npc"))) + 
      
    scale_linetype_manual(values = c("Richard Freeman Union Density Estimates" = "solid",
                                     "CPS Union Density Estimates" = "dashed"),
                          breaks = c("Richard Freeman Union Density Estimates",
                                     "CPS Union Density Estimates")) +
      
    geom_label(label = str_wrap("Idle days peaked in 1959 with 245 new work stoppages involving 1.38 million workers.", width = 50),
                  mapping = aes(x = 1980,
                  y = 45000),
              size = 10,
              hjust = 0,
              #border = NA,
              #fill = NA,
              label.size = NA,
              color = "black",
              lineheight = 0.45) + 
      
    scale_x_continuous(breaks = seq(1947, 2023, 4),
                         limits = c(1946, 2024),
                         position = "bottom") +
      
    scale_y_continuous(breaks = seq(0, 65000, 10000),
                         limits = c(0, 65000),
                         position = "left",
                         label = scales::number_format(scale = 0.001, suffix = "m"),
                       sec.axis = sec_axis(trans=~./150000,
                                           name="Union Density",
                                           label = scales::label_percent())) +
    
    ylab("Total Number of Idle Days") +
    xlab("Year") +
      
    theme(#plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.2, size = 24),
        axis.title.x = element_text(size = 28,color = "black", margin = margin(4,0,0,0)),
        axis.title.y = element_text(size = 28,color = "black"),
        axis.text.x = element_text(size = 22, angle = 45, vjust = 0.5,color = "black"),
        axis.text.y = element_text(size = 22,color = "black"),
        # panel
        panel.grid.major.y = element_line(color = "grey", size = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        # axis lines
        axis.line = element_line(color = "black", size = 0.4),
        # legend
        legend.margin=margin(0,0,0,0),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.justification = "top",
        legend.box.just = "top",
        legend.title.align = 0.5,
        legend.text = element_text(size = 22, margin = margin(0,0,0,0)))
  
  
  dev.off()
  

  