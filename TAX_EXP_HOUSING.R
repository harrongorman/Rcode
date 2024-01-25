### housing budget outlays


install.packages("treemapify")
install.packages("gridExtra")
#import packages

library(tidyverse)
library(readxl)
library(scales)
library(crstools)
library(ggtext)
library(ggplot2)
library(treemapify)
library(gridExtra)
library(cowplot)

install.packages("treemap")
library(treemap)


## #Set Directory
setwd("S:/DSP_DOCUMENTS/RAs/GORMAN/McCarty, Maggie/Housing Budget Presentation")

## download file
Tax_Exp <- read_excel('Tax Expenditures.xlsx', sheet = 'Sheet1')
Budget_Outlays <- read_excel('outlays_fy2023_filteredhousing.xlsx', sheet = 'Filtered Sheet')

# correcting layout and naming of variables in budget outlays
Budget_Outlays = Budget_Outlays %>%
  gather(Year, Value, -'Agency Name', -'Bureau Name', -'Account Name', -'Subfunction Title', -'BEA Category') %>%
  
  
  mutate(Year = as.numeric(Year)) %>%

  
  ### removes negatives
  
  mutate(Value1 = case_when(Value >= 0 ~ Value,
                            Value < 0 ~ 0)) 

# re-title column names

names(Budget_Outlays)[3] = 'AccountName' 
names(Budget_Outlays)[2] = 'BEACategory'
names(Budget_Outlays)[5] = 'BureauName'
names(Budget_Outlays)[1] = 'AgencyName'
names(Budget_Outlays)[4] = 'Subfunction'



### this is an additional step just for the 2021 buget outlays treemap!!!!!!!
Budget_Outlays = Budget_Outlays %>%
  arrange(desc(Subfunction)) %>%
  group_by(AccountName, Subfunction, Year) %>%
  summarize(Value1 = sum(Value1, na.rm = TRUE))
### this is just for the tree map!!!!!!!

### for just 2021 budget outlays small graph 

Budget_Outlays_1 = Budget_Outlays %>%
  group_by(Year, Subfunction) %>%
  summarize(Subfunction_Total = sum(Value1)) %>%
  filter(Year == 2021)



##############################################################################
### graph ### 2017 tax exp ###################################################
##############################################################################

p = ggplot(data = filter(Tax_Exp, Year == 2017),
       aes(area = Amount, 
           fill = Class,
           label = paste0(Name, "\n", "$", paste0(round(Amount, 1), 'b')),
           show.legend = TRUE,
           fixed = TRUE)) +
  
  geom_treemap(show.legend = FALSE) + 
  
  scale_fill_manual(values=c(crs_get_palette()$orange4, crs_get_palette()$yellow4)) +   #, labels = c("Owner Tax Expenditures", "Rental Tax Expenditures"), name = "") + 
  
  ## geom_treemap_subgroup_text(color = "white") + 
  
  geom_treemap_text(mapping = NULL, data = NULL, stat = "identity",
                    colour = crs_get_palette()$grey1,
                    position = "identity", na.rm = FALSE, show.legend = FALSE,
                    inherit.aes = TRUE, padding.x = grid::unit(1, "mm"),
                    padding.y = grid::unit(1, "mm"), place = "middle", min.size = 2,
                    grow = F, reflow = T) + 
  
  #ggtitle("$209.6 billion FY2017") +
  
  #geom_treemap_subgroup_border(colour = "white", size = 2) + 
  
  coord_cartesian(clip = "off") + 
  
  crs_theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text = element_text(size = 10), legend.text = element_text(size = 10),
            legend.position = "bottom")
p

##############################################################################
### 2021 tax exp #############################################################
##############################################################################

g = ggplot(data = filter(Tax_Exp, Year == 2021),
           aes(area = Amount, 
               fill = Class,
               label = paste0(Name, "\n", "$", paste0(round(Amount, 1), 'b')),
               show.legend = TRUE,
               fixed = TRUE)) +
  
  geom_treemap(show.legend = FALSE) + 
  
  scale_fill_manual(values=c(crs_get_palette()$orange4, crs_get_palette()$yellow4)) +  #, labels = c("Owner Tax Expenditures", "Rental Tax Expenditures"), name = "") + 
  
  ## geom_treemap_subgroup_text(color = "white") + 
  
  geom_treemap_text(mapping = NULL, data = NULL, stat = "identity",
                    colour = crs_get_palette()$grey1,
                    position = "identity", na.rm = FALSE, show.legend = FALSE,
                    inherit.aes = TRUE, padding.x = grid::unit(1, "mm"),
                    padding.y = grid::unit(1, "mm"), place = "middle", min.size = 2,
                    grow = F, reflow = T) + 
  
 # ggtitle("$91.1 billion FY2021") +
  
  #geom_treemap_subgroup_border(colour = "white", size = 2) + 
  
  coord_cartesian(clip = "off") + 
  
  crs_theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text = element_text(size = 10), legend.text = element_text(size = 10),
            legend.position = "bottom")

g

ggdraw() + 
  draw_plot (g, 0.05, 0.01, 0.9, 0.9) +
  draw_text("2021 Tax Expenditures", size = 22, x = 0.5, y = 0.965, fontface = 'bold') +
  draw_text("$91.1 billion", size = 18, x = 0.5, y = 0.925, fontface = 'bold')

### comb graph of 2021 tax (g) and 2021 buget (k) ##############################################################

f = ggdraw() +
 draw_plot (g, 0.02, 0.28875, 0.4225, 0.4225) + 
  draw_plot (k, 0.4425, 0.24125, 0.5175, 0.5175) +
  
  draw_text("2021 Tax Expenditures", size = 14, x = 0.22, y = 0.75, fontface = 'bold') +
  draw_text("$91.1 billion", size = 12, x = 0.22, y = 0.72, fontface = 'bold') +
  draw_text("2021 Budget Outlays", size = 14, x = 0.72125, y = 0.80, fontface = 'bold') +
  draw_text("$100.03 billion", size = 12, x = 0.72125, y = 0.77, fontface = 'bold')
  
f
 

######## comb graph of 2021 tax (g) and 2017 tax(p) ##############################################################

f = ggdraw() +
  draw_plot (p, 0.02, 0.2165, 0.567, 0.567) + 
  draw_plot (g, 0.607, 0.3135, 0.373, 0.373) +
  
  draw_text("2017 Tax Expenditures", size = 14, x = 0.3035, y = 0.83, fontface = 'bold') +
  draw_text("$209.6 billion", size = 12, x = 0.3035, y = 0.80, fontface = 'bold') +
  draw_text("2021 Tax Expenditures", size = 14, x = 0.7935, y = 0.73, fontface = 'bold') +
  draw_text("$91.1 billion", size = 12, x = 0.7935, y = 0.70, fontface = 'bold')

f


#####################################################################################################################
####################################### create new budget outlays graph  ############################################
#####################################################################################################################

k = ggplot(data = filter(Budget_Outlays, Year == 2021),
           aes(area = Value1,
               fill = Subfunction,
               label = paste0(AccountName, "\n", "$", paste0(round(Value1 / 1000000, 1), 'b')),
               layout = "squarified",
               
               subgroup = Subfunction,
               fixed = TRUE)) +
  
  geom_treemap() + 
  
  scale_fill_manual(values=c("Housing assistance" = crs_get_palette()$blueb4, 
                            "Community development" = crs_get_palette()$bluea4)) + 
  
  # geom_treemap_subgroup_text(color = "white") + 
  
  geom_treemap_text(mapping = NULL, data = NULL, stat = "identity",
                    position = "identity", na.rm = FALSE, show.legend = FALSE,
                    inherit.aes = TRUE, padding.x = grid::unit(1, "mm"),
                    padding.y = grid::unit(1, "mm"), place = "middle", min.size = 6,
                    grow = F, reflow = T) + 
  
  ##geom_treemap_subgroup_border(colour = crs_get_palette()$blgray5, size = 1) + 

  
  coord_cartesian(clip = "off") + 
  
  crs_theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text = element_text(size = 8), legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.position = "none")

k
 

##############################

# reattempt - for small treemap

k = ggplot(data = filter(Budget_Outlays_1, Year == 2021),
           aes(area = Subfunction_Total,
               fill = Subfunction,
               label = paste0(Subfunction, "\n", "$", paste0(round(Subfunction_Total / 1000000, 1), 'b')),
               layout = "squarified",
               show.legend = FALSE,
               ##subgroup = AccountName,
               fixed = TRUE)) +
  
  geom_treemap() + 
  
  scale_fill_manual(values=c("Housing assistance" = crs_get_palette()$blueb4, 
                             "Community development" = crs_get_palette()$bluea4)) + 
  
  # geom_treemap_subgroup_text(color = "white") + 
  
  geom_treemap_text(mapping = NULL, data = NULL, stat = "identity",
                    colour = crs_get_palette()$grey1,
                    position = "identity", na.rm = FALSE, show.legend = FALSE,
                    inherit.aes = TRUE, padding.x = grid::unit(1, "mm"),
                    padding.y = grid::unit(1, "mm"), place = "middle", min.size = 6,
                    grow = F, reflow = T) + 
  
  ##geom_treemap_subgroup_border(colour = crs_get_palette()$blgray5, size = 1) + 
  
  
  coord_cartesian(clip = "off") + 
  
  crs_theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text = element_text(size = 8), legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.position = "none")

k


#####################
### attempting base R 
#####################


### create palette

crscol = c(crs_get_palette()$blueb4, crs_get_palette()$bluea4)

Budget_Outlays_basetreemap = Budget_Outlays %>%
  mutate(Value2 = ifelse(AccountName == "Native American Programs",
                         " Native American ProgramNative American ProgramNative American Program", 
                                                                          
                                                                             paste0(AccountName, "\n","$", paste0(round(Value1 / 1000000, 1), 'b'))
                                                                             
                         )
         
         ) %>%
  filter(Year == 2021)



## create treemap

png("S:/DSP_DOCUMENTS/RAs/GORMAN/McCarty, Maggie/Housing Budget Presentation/Fourth Round of Charts/changes/2021_budget_outlays_4_labels.png", width = 6.05, height = 5.9, res = 300, units = "in", type = 'cairo-png')

treemap(filter(Budget_Outlays_basetreemap, Year == 2021), index = c("Subfunction", "Value2"),
        vSize = "Value1",
        type = "categorical",
        border.col = c("black", "black"),
        border.lwds=c(0.75,0.75),
        bg.labels= 255,
        inflate.labels=F,
        vColor = "Subfunction",
        fontsize.labels = c(0, 11),
        lowerbound.cex.labels = 0.64,
        position.legend = "none",
        title.legend = " ",
        reverse.legend = T,
        fontsize.title = 14,
        force.print.labels=F,
      
        palette = crscol,
        fontcolor.labels = c("white", "white"),
        fontsize.legend = 14,
        fontface.labels = c("bold", "bold"),
       fontfamily.legend = "sans",
        title = "")

dev.off()


#####################################################################################################################
# creating the combined 
#####################################################################################################################

######## comb graph of 2017 tax (p) and 2017 tax(g) and 2021 outlays(k)##############################################################

f = ggdraw() +
  draw_plot (p, 0.02, 0.305, 0.391, 0.391) + 
  draw_plot (g, 0.431, 0.371, 0.258, 0.258) +
  draw_plot (k, 0.709, 0.365, 0.27, 0.27) +
  
  draw_text("$209.6 billion FY2017 Tax Expenditures", size = 9, x = 0.1365, y = 0.704, fontface = 'bold') +
  #draw_text("$209.6 billion", size = 12, x = 0.2155, y = 0.714, fontface = 'bold') +
  draw_text("$91.1 billion FY2021 Tax Expenditures", size = 9, x = 0.546, y = 0.637, fontface = 'bold') +
  #draw_text("$91.1 billion", size = 12, x = 0.56, y = 0.647, fontface = 'bold') +
  draw_text("$100 billion FY2021 Budget Outlays", size = 9, x = 0.816, y = 0.643, fontface = 'bold') 
  #draw_text("$100 billion", size = 12, x = 0.844, y = 0.659, fontface = 'bold')

f

#############################
## create a custom legend ###
#############################

fake_df = data.frame(Name = c ("Ownership Tax Expenditures",
                               "Rental Tax Expenditures",
                               "Housing Assistance",
                               "Community Development"
                               ),
                     Value = c(1,1,1,1))

d = ggplot() +
  geom_col(data = fake_df,
           aes(x = Name,
               y = Value,
               fill = Name),
           show.legend = TRUE) +
  
  scale_fill_manual(values=c( "Ownership Tax Expenditures" = crs_get_palette()$orange4, 
                                "Rental Tax Expenditures" = crs_get_palette()$yellow4,
                              "Housing Assistance" = crs_get_palette()$blueb4, 
                             "Community Development" = crs_get_palette()$bluea4

                             
  )) +
  
  guides(fill = guide_legend(title = "Tax/Budget Category")) +
  
  crs_theme(legend.title = element_text(face = "bold"),
            legend.position="bottom",
            legend.text = element_text(size = 12))
  
d







 
