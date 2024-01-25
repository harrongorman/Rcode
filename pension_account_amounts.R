

library(knitr)
library(tidyverse)
library(fredr)
library(lubridate)
library(ggplot2)
library(gt)
library(here)
library(kableExtra)
library(formattable)
library(markdown)
library(pandoc)
library(crstools)
library(pander)
library(Rfast)


pension = read.csv("S:/DSP_DOCUMENTS/RAs/GORMAN/Topoleski, John/In_Focus_Chart/pension_financial_accounts.csv") %>%
  mutate(Value = as.numeric(Value),
         Cumvalue = as.numeric(Cumvalue))

totaly = sum(pension$Value)



totals = pension %>%
  group_by(Type, Plan) %>%
  summarize(typetot = sum(Value),
            minval = mean(minimum))


pension = pension %>% 
  mutate(Value = 200*Value/totaly,
          Cumvalue = 200*Cumvalue/totaly) %>%
  mutate(Account = factor(Account, levels = c("Short-term Securities", "Corporate Equities", "Debt", "Mutual Funds", "Other Financial Assets", "DB Claims on Sponsor")))

pension2 = pension

pension = pension

################################################################

png("S:/DSP_DOCUMENTS/RAs/GORMAN/Topoleski, John/In_Focus_Chart/pension_financial_accounts_grey2_IRAendof2022.png", width = 6.5, height = 4.3, res = 300, units = "in", type = 'cairo-png')

ggplot(data = pension) +
  
  scale_x_continuous(limits = c(0,100)) +
  
  scale_y_continuous(limits = c(10,92.5)) +
  
  
  ### trillions lines
  
  geom_segment(y = 8.5, yend= 89, x = 30, xend = 30, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*100000/totaly, xend = 30 + 2*100000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*200000/totaly, xend = 30 + 2*200000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*300000/totaly, xend = 30 + 2*300000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*400000/totaly, xend = 30 + 2*400000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*500000/totaly, xend = 30 + 2*500000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*600000/totaly, xend = 30 + 2*600000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*700000/totaly, xend = 30 + 2*700000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*800000/totaly, xend = 30 + 2*800000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*900000/totaly, xend = 30 + 2*900000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*1000000/totaly, xend = 30 + 2*1000000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*1100000/totaly, xend = 30 + 2*1100000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  geom_segment(y = 8.5, yend= 89, x = 30 + 2*1200000/totaly, xend = 30 + 2*1200000/totaly, linewidth = 0.25, color = crs_get_palette()$grey2) +
  
  ### labels for section titles
  
  geom_text(data = pension,
            aes(x = 2.5,
                y= 90),
            size = 4.5,
            label = "Private Sector",
            fontface = "bold",
            color = "black",
            hjust = "left",
            vjust = "bottom") +
  
  geom_text(data = pension,
            aes(x = 2.5,
                y= 65),
            size = 4.5,
            label = "State and Local Government",
            fontface = "bold",
            color = "black",
            hjust = "left",
            vjust = "bottom") +
  
  geom_text(data = pension,
            aes(x = 2.5,
                y= 40),
            size = 4.5,
            label = "Federal Government",
            fontface = "bold",
            color = "black",
            hjust = "left",
            vjust = "bottom") +
  
  geom_text(data = pension,
            aes(x = 2.5,
                y= 17.5),
            size = 4.5,
            label = "IRAs",
            fontface = "bold",
            color = "black",
            hjust = "left",
            vjust = "bottom") +
  
  geom_text(data = pension,
            aes(x = 80,
                y= 90),
            size = 4.5,
            label = "$ in trillions",
            color = "black",
            hjust = "left",
            vjust = "bottom",
            fontface = "bold",
            show.legend = FALSE) +
  
  
  ### rectangles
  ### I use (Total[nrow(pension)]/3 as the extent of the x axis for each bar
  
  
  geom_rect(data = pension,
            aes(ymin = minimum,
                ymax = minimum + 5,
                xmin = case_when(Account == "Short-term Securities" ~ Cumvalue + 30,
                                 Account != "Short-term Securities" ~ Cumvalue + 30),
                xmax = case_when(Account == "Short-term Securities" ~ Cumvalue + 30 + Value,
                                 Account != "Short-term Securities" ~ Cumvalue + Value + 30),
                fill = Account)) +
                  
                  
                scale_fill_manual(values = c("Short-term Securities" = crs_get_palette()$orange5,
                  "Debt" = crs_get_palette()$blueb5,
                  "Corporate Equities" = crs_get_palette()$green5,
                  "Mutual Funds" = crs_get_palette()$yellow5,
                  "Other Financial Assets" = crs_get_palette()$purple5,
                  "DB Claims on Sponsor" = crs_get_palette()$bluea5)) + 
  
  
  geom_rect(data = totals,
            aes(ymin = minval,
                ymax = minval + 5.1,
                xmin = 31 + 200*typetot/totaly,
                xmax = 40 + 200*typetot/totaly),
            fill = crs_get_palette()$grey4,
            color = crs_get_palette()$grey3,
            ) +
  
  geom_rect(
            ymin = 10.5,
                ymax = 10.5 + 5.1,
                xmin = 31 + 200*11484/totaly,
                xmax = 40 + 200*11484/totaly,
            fill = crs_get_palette()$grey4,
            color = crs_get_palette()$grey3,
  ) +

  geom_text(data = totals,
                       aes(y = minval + 2.5,
                           x = 35.5 + 200*typetot/totaly,
                           label = paste0('$', round(typetot/1000,2))),
                       color = "black",
                       hjust = "center",
                       vjust = "center",
                       size = 4,
                       show.legend = FALSE) +
  

  # # ## value labels
  # 
  # geom_text(data = graphdf,
  #           aes(y = 13.25, x = 2+30+60*IRA[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",IRA[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # 
  # geom_text(data = graphdf,
  #           aes(y = 27.6, x = 2+30+60*DB_Federal_Pension[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",DB_Federal_Pension[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # 
  # geom_text(data = graphdf,
  #           aes(y = 34.6, x = 2+30+60*DC_Federal_Pension[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",DC_Federal_Pension[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # 
  # geom_text(data = graphdf,
  #           aes(y = 52.6, x = 2+30+60*DB_SL_Pension[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",DB_SL_Pension[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # 
  # geom_text(data = graphdf,
  #           aes(y = 59.8, x = 2+30+60*DC_SL_Pension[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",DC_SL_Pension[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # 
  # geom_text(data = graphdf,
  #           aes(y = 77.5, x = 2+30+60*DB_Private_Pension[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",DB_Private_Pension[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # 
  # geom_text(data = graphdf,
  #           aes(y = 84.9, x = 2+30+60*DC_Private_Pension[nrow(graphdf)]/(Total[nrow(graphdf)]/3),
  #               label = paste0("$",DC_Private_Pension[nrow(graphdf)])),
  #           color = "black",
  #           hjust = "left",
  #           vjust = "center",
  #           size = 4,
  #           show.legend = FALSE) +
  # #

  #### geom segment to split up each of the sets of bars

  geom_segment(data = pension,
               x = 2.5, xend = 97.5,
               y = 22.5, yend = 22.5,
               linewidth = 0.5,
               color = "black") +

  geom_segment(data = pension,
               x = 2.5, xend = 97.5,
               y = 47.5, yend = 47.5,
               linewidth = 0.5,
               color = "black") +

  geom_segment(data = pension,
               x = 2.5, xend = 97.5,
               y = 72.5, yend = 72.5,
               linewidth = 0.5,
               color = "black") +


  ###### benefit versus contribution labels

  geom_text(data = pension,
            aes(y = 27.5, x = 28),
            label = "Defined Benefit",
            color = "black",
            hjust = "right",
            vjust = "center",
            size = 4,
            show.legend = FALSE) +

  geom_text(data = pension,
            aes(y = 35, x = 28),
            label = "Defined Contribution",
            color = "black",
            hjust = "right",
            vjust = "center",
            size = 4,
            show.legend = FALSE) +

  geom_text(data = pension,
            aes(y = 52.5, x = 28),
            label = "Defined Benefit",
            color = "black",
            hjust = "right",
            vjust = "center",
            size = 4,
            show.legend = FALSE) +

  geom_text(data = pension,
            aes(y = 60, x = 28),
            label = "Defined Contribution",
            color = "black",
            hjust = "right",
            vjust = "center",
            size = 4,
            show.legend = FALSE) +

  geom_text(data = pension,
            aes(y = 77.5, x = 28),
            label = "Defined Benefit",
            color = "black",
            hjust = "right",
            vjust = "center",
            size = 4,
            show.legend = FALSE) +

  geom_text(data = pension,
            aes(y = 85, x = 28),
            label = "Defined Contribution",
            color = "black",
            hjust = "right",
            vjust = "center",
            size = 4,
            show.legend = FALSE) +

  

  
  ### theme functions
  
  crs_theme(axis.text = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.x = element_blank(),
            strip.text.x = element_blank(),
            axis.line.x = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            strip.background = element_blank(),
            panel.background = element_blank(),
            strip.text.y = element_blank(),
            axis.text.y = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, size = 12),
            panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
            legend.title = element_blank(),
            legend.position = "bottom",
            legend.text = element_text(size = 10))



dev.off()

