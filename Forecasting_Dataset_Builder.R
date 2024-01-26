# SETUP -------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(vroom)
library(stringr)
library(tidyr)
library(forcats)
library(lubridate)
library(crstools)
library(ggtext)
library(DT) #for the interactive tables
library(plotly)
library(tibble)
library(lubridate)
library(crosstalk) #for grouped data in spaghetti charts
library(grid) #for facet spacing
library(htmlwidgets) # for forecast selection
library(shiny) # for forecast selection
library(knitr)
library(forecast)

#Setting up working directory 
setwd('S:/TEAMS/POVERTYETC/COVID Unemployment Situation Visualizations/Data Work')

###Data

##The data base
file_cutdown <- vroom('Data/working_bls_dataset.csv')

# Forecasting -----------------------------------------------------------------------------------------------------------

monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
lt$year*12 + lt$mon } 
# compute a month difference as a difference between two monnb's
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

total_forecast_file_cutdown = file_cutdown %>%
  mutate(expected = "Real") %>%
  mutate(date = as.Date(date))

tempfile = file_cutdown %>% group_by(VarName) %>% slice(which.min(date)) %>% filter(date == as.Date("2003-01-01"))

listinclude = tempfile$VarName

file_cutdown = file_cutdown %>%
  filter(VarName %in% listinclude)


## list of unique vars looping through forcasting  -------------------------------------------------------------------------

for (i in unique(file_cutdown$VarName)) {
  
  #### beginning of post COVID using 2010 to 2019 training data
  
      # selects training data
      temphold = file_cutdown %>%
        filter(VarName == i,
               date < as.Date("2020-01-01"),
               date > as.Date("2009-12-01")) %>%
        arrange(date)
      
      # if already seasonally adjusted then no seasonality is required
      if(substr(i, 1, 6) == "(Seas)") {
        fit = auto.arima(ts(temphold$value, frequency = 12), seasonal = TRUE)
      } else {
        fit = auto.arima(ts(temphold$value, frequency = 12), D = 1)
      }
      
      ## forecast of fit
      fc = forecast(fit, h = mondf(as.Date("2020-02-01"), today()))
      
      # check fit
      t = checkresiduals(fc)
      
      if(t$p.value >= 0.05) {
        
        # creates new dataset out of forecasted data 
        forecasted_df = data.frame(value = fc$mean) %>%
          rownames_to_column() %>%
          mutate(date = as.Date("2020-01-01") %m+% months(as.numeric(rowname)-1),
                 VarName = i,
                 expected = "Forecast Post-COVID - 2010 to 2019 Training Data")
        
        # joins new forecasted data with real data
        total_forecast_file_cutdown = plyr::rbind.fill(total_forecast_file_cutdown , forecasted_df)
        
      }
    
    ### end of post COVID using 2003 to 2019 training data
        
    #### beginning of post COVID using 2010 to 2019 training data
        
        # selects training data
        temphold = file_cutdown %>%
          filter(VarName == i,
                 date < as.Date("2020-01-01"),
                 date > as.Date("2002-12-01")) %>%
          arrange(date)
        
        # if already seasonally adjusted then no seasonality is required
        if(substr(i, 1, 6) == "(Seas)") {
          fit = auto.arima(ts(temphold$value, frequency = 12), seasonal = TRUE)
        } else {
          fit = auto.arima(ts(temphold$value, frequency = 12), D = 1)
        }
        
        ## forecast of fit
        fc = forecast(fit, h = mondf(as.Date("2020-02-01"), today()))
        
        # check fit
        t = checkresiduals(fc)
        
        if(t$p.value >= 0.05) {
          
          # creates new dataset out of forecasted data 
          forecasted_df = data.frame(value = fc$mean) %>%
            rownames_to_column() %>%
            mutate(date = as.Date("2020-01-01") %m+% months(as.numeric(rowname)-1),
                   VarName = i,
                   expected = "Forecast post COVID - 2003 to 2019 Training Data")
          
          # joins new forecasted data with real data
          total_forecast_file_cutdown = plyr::rbind.fill(total_forecast_file_cutdown , forecasted_df)
          
        }
          
    ### end of post COVID using 2003 to 2019 training data

    #### beginning of post recession using 2003 to 2019 training data
          
          # selects training data
          temphold = file_cutdown %>%
            filter(VarName == i,
                   date < as.Date("2007-12-01"),
                   date > as.Date("2002-12-01")) %>%
            arrange(date)
          
          # if already seasonally adjusted then no seasonality is required
          if(substr(i, 1, 6) == "(Seas)") {
            fit = auto.arima(ts(temphold$value, frequency = 12), seasonal = TRUE)
          } else {
            fit = auto.arima(ts(temphold$value, frequency = 12), D = 1)
          }
          
          ## forecast of fit
          fc = forecast(fit, h = mondf(as.Date("2008-01-01"), today()))
          
          # check fit
          t = checkresiduals(fc)
          
          if(t$p.value >= 0.05) {
            
            # creates new dataset out of forecasted data 
            forecasted_df = data.frame(value = fc$mean) %>%
              rownames_to_column() %>%
              mutate(date = as.Date("2007-12-01") %m+% months(as.numeric(rowname)-1),
                     VarName = i,
                     expected = "Forecast post Recession - 2003 to Nov. 2007 Training Data")
            
            # joins new forecasted data with real data
            total_forecast_file_cutdown = plyr::rbind.fill(total_forecast_file_cutdown , forecasted_df)
            
          }
            
    ### end of post recession using 2003 to 2007 training data      
          
  
  print(which(unique(file_cutdown$VarName) == i))
  
}

### downloaded updated forecast file & remove not apps

    ## these represent all of the variables that were forecasted
    list_of_vars = total_forecast_file_cutdown %>%
      filter(expected != "Real")
    
    list_of_vars = list_of_vars$VarName

    # filter real & expected to just those variables that were forecasted
    total_forecast_file_cutdown_2 = total_forecast_file_cutdown %>%
      filter(VarName %in% list_of_vars)

### write data set to be used in R markdown file later

readr::write_csv(total_forecast_file_cutdown_2,
                 'Data/total_forecast_file.csv')

## finished



