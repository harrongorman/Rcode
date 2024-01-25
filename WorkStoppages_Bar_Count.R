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
library(stringr)


### download data and set wd

setwd("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus")
newstops = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/stoppages_new.xlsx", sheet = 1) %>%
  mutate(Year = as.numeric(Year),
         Cat = "Orange") %>%
  clean_names()

union_density_1983 = read_xlsx("S:/DSP_DOCUMENTS/RAs/GORMAN/Whittacker, Julie/WorkStoppagesInfocus/uniondensity_1983.xlsx", sheet = 1)%>%
  mutate(line_scale = "CPS Union Density Estimates")

union_density = read_xlsx("S:/DSP_DOCUMENTS/RAs/Former RAs/ROMERO/Bradley, David/Union Membership/Freeman Union Data.xlsx", sheet = 1) %>%
  mutate(line_scale = "Richard Freeman Union Density Estimates")

union_density_percentages = union_density %>%
  mutate(percent = Members/`Nonag Employment`)

newstops_2023_ = data.frame(year = c(2023),
                            beginning_stoppages = c(34),
                            cat = c("Orange"))

newstops = plyr::rbind.fill(newstops, newstops_2023_)

duplicate_rows <- function(year, samples) {
  expanded_samples <- paste0(year, "-", "s", 1:samples)
  repeated_rows <- data.frame("Year" = year, "Count" = expanded_samples)
  
  repeated_rows
}

expanded_rows <- Map(f = duplicate_rows, newstops$Year, newstops$`Beginning Stoppages`)

new_df <- do.call(rbind, expanded_rows) %>%
  mutate(Count2 = sub('.*-s', '', Count))

#### area plot of new stoppages #####

png("Graphs/newstoppages_1.18.2023.png", width = 6, height = 4, res = 300, units = "in", type = 'cairo-png')

ggplot() +
  
  geom_col(newstops,
            mapping = aes(x = year,
                          y = beginning_stoppages),
            fill = crs_get_palette()$orange4,
            color = crs_get_palette()$orange4) +

  scale_x_continuous(breaks = seq(1947, 2023, 4),
                   limits = c(1946.5, 2023.5),
                   position = "bottom") +
  
  scale_y_continuous(breaks = seq(0, 500, 50),
                     limits = c(0, 500),
                     position = "left",
                     sec.axis = sec_axis(trans=~./1000,
                                         name="Union Density",
                                         label = scales::label_percent())) +
  # ## AIR TRAFFIC CONTROLLERS
  # geom_curve(mapping = aes(x = 2000,
  #                          xend = 1982.5,
  #                          y = 265,
  #                          yend = 145),
  #            curvature = -0.1,
  #            arrow = arrow(length = unit(0.03, "npc"))) +
  # 
  # geom_label(label = str_wrap("In 1981, President Reagan fired striking Air Traffic Controllers.", width = 35),
  #            mapping = aes(x = 1990,
  #                          y = 300),
  #            size = 10,
  #            hjust = 0,
  #            #border = NA,
  #            #fill = NA,
  #            label.size = NA,
  #            color = "black",
  #            lineheight = 0.45) +

  # ## LINDEN LUMBER
  # geom_curve(mapping = aes(x = 1984,
  #                          xend = 1975,
  #                          y = 475,
  #                          yend = 428),
  #            curvature = 0.2,
  #            arrow = arrow(length = unit(0.03, "npc"))) +
  # 
  # geom_label(label = str_wrap("In 1974, the Supreme Court ruled that unions needed to win NLRB elections rather than meet `majority signup` requirements to be recognized by employers.", width = 35),
  #            mapping = aes(x = 1985,
  #                          y = 450),
  #            size = 6,
  #            hjust = 0,
  #            #border = NA,
  #            #fill = NA,
  #            label.size = NA,
  #            color = "black",
  #            lineheight = 0.5) +


  # ## STEEL WORKERS
  # geom_curve(mapping = aes(x = 1979,
  #                          xend = 1958,
  #                          y = 440,
  #                          yend = 340),
  #            curvature = 0.45,
  #            arrow = arrow(length = unit(0.03, "npc"))) +
  # 
  # geom_label(label = str_wrap("In 1959, the Supreme Court forced 500,000 steel workers back to work.", width = 40),
  #            mapping = aes(x = 1980,
  #                          y = 435),
  #            size = 10,
  #            hjust = 0,
  #            #border = NA,
  #            #fill = NA,
  #            label.size = NA,
  #            color = "black",
  #            lineheight = 0.45) +

geom_line(union_density_percentages %>% filter(Year > 1946),
          mapping = aes(x = Year,
                        y = percent*1000,
                        linetype = line_scale),
          show.legend = TRUE) +
  
  geom_line(union_density_1983,
            mapping = aes(x = as.numeric(Year),
                          y = as.numeric(Percent)*1000,
                          linetype = line_scale),
            show.legend = TRUE) +
  
  scale_linetype_manual(values = c("Richard Freeman Union Density Estimates" = "solid",
                                   "CPS Union Density Estimates" = "dashed"),
                        breaks = c("Richard Freeman Union Density Estimates",
                                   "CPS Union Density Estimates")) +

  ylab("Total New Work Stoppages") +
  xlab("Year") +
  
  theme(#plot.title = element_text(face = "bold", hjust = 0.5, vjust = 0.2, size = 24),
    axis.title.x = element_text(size = 32,color = "black"),
    axis.title.y = element_text(size = 32,color = "black"),
    axis.text.x = element_text(size = 24, angle = 45, vjust = 0.5,color = "black"),
    axis.text.y = element_text(size = 24,color = "black"),
    # panel
    panel.grid.major.y = element_line(color = "grey", size = 0.1),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_blank(),
    # axis lines
    axis.line = element_line(color = "black", size = 0.4),
    # legend
    legend.position = "bottom",
    legend.margin=margin(0,0,0,0),
    legend.title = element_blank(),
    legend.key.size = unit(0.6, 'cm'),
    legend.title.align = 0.5,
    legend.text = element_text(size = 22))

dev.off()