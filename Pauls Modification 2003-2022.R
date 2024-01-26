
# beginning of main file

library(devtools)
library(blsR)
library(blscrapeR)
library(tidyverse)
library(lubridate)
library(summarytools)
library(readr)

# Setup -------------------------------------------------------------------

#Working directory
setwd('S:/TEAMS/POVERTYETC/COVID Unemployment Situation Visualizations/Data Work')

neededseriesid = c(
  
  ### A1 table
  "(Unadj) Population Level" = 'LNU00000000',
  "(Seas) Civilian Labor Force Level" =  'LNS11000000',
  "(Seas) Labor Force Participation Rate" = 'LNS11300000',
  "(Seas) Employment Level" = 'LNS12000000',
  "(Seas) Employment-Population Ratio" = 'LNS12300000',
  "(Seas) Unemployment Level" = 'LNS13000000',
  "(Seas) Unemployment Rate" = 'LNS14000000',
  "(Seas) Not in Labor Force" = 'LNS15000000',
  "(Seas) Not in labor force - Want a job now"= 'LNS15026639',
  
  "(Unadj) Civilian Labor Force Level" =  'LNU01000000',
  "(Unadj) Labor Force Participation Rate" = 'LNU01300000',
  "(Unadj) Employment Level" = 'LNU02000000',
  "(Unadj) Employment-Population Ratio" = 'LNU02300000',
  "(Unadj) Unemployment Level" = 'LNU03000000',
  "(Unadj) Unemployment Rate" = 'LNU04000000',
  "(Unadj) Not in Labor Force" = 'LNU05000000',
  "(Unadj) Not in labor force and want a job now"= 'LNU05026639',
  
  
  #### table A2
  
  "(Seas) Civilian Labor Force Level - White" = 'LNS11000003',
  "(Seas) Labor Force Participation Rate - White" = 'LNS11300003',
  "(Seas) Employment Level - White" = 'LNS12000003',
  "(Seas) Employment-Population Ratio - White" = 'LNS12300003',
  "(Seas) Unemployment Level - White" = 'LNS13000003',
  "(Seas) Unemployment Rate - White" = 'LNS14000003',
  "(Seas) Not in Labor Force, White" = 'LNS15000003',
  "(Seas) Civilian Labor Force Level - 20 yrs. & over, White Men" = 'LNS11000028',
  "(Seas) Labor Force Participation Rate - 20 yrs. & over, White Men" = 'LNS11300028',
  "(Seas) Employment Level - 20 yrs. & over, White Men" = 'LNS12000028',
  "(Seas) Employment-Population Ratio - 20 yrs. & over, White Men" = 'LNS12300028',
  "(Seas) Unemployment Level - 20 yrs. & over, White Men" = 'LNS13000028',
  "(Seas) Unemployment Rate - 20 yrs. & over, White Men" = 'LNS14000028',
  "(Seas) Civilian Labor Force Level - 20 yrs. & over, White Women" = 'LNS11000029',
  "(Seas) Labor Force Participation Rate - 20 yrs. & over, White Women" = 'LNS11300029',
  "(Seas) Employment Level - 20 yrs. & over, White Women" = 'LNS12000029',
  "(Seas) Employment-Population Ratio - 20 yrs. & over, White Women" = 'LNS12300029',
  "(Seas) Unemployment Level - 20 yrs. & over, White Women" = 'LNS13000029',
  "(Seas) Unemployment Rate - 20 yrs. & over, White Women" = 'LNS14000029',
  "(Seas) Civilian Labor Force Level - 16-19 yrs., White" = 'LNS11000015',
  "(Seas) Labor Force Participation Rate - 16-19 yrs., White" = 'LNS11300015',
  "(Seas) Employment Level - 16-19 yrs., White" = 'LNS12000015',
  "(Seas) Employment-Population Ratio - 16-19 yrs., White" = 'LNS12300015',
  "(Seas) Unemployment Level - 16-19 yrs., White" = 'LNS13000015',
  "(Seas) Unemployment Rate - 16-19 yrs., White" = 'LNS14000015',
  "(Seas) Civilian Labor Force Level - Black or African American" = 'LNS11000006',
  "(Seas) Labor Force Participation Rate - Black or African American" = 'LNS11300006',
  "(Seas) Employment Level - Black or African American" = 'LNS12000006',
  "(Seas) Employment-Population Ratio - Black or African American" = 'LNS12300006',
  "(Seas) Unemployment Level - Black or African American" = 'LNS13000006',
  "(Seas) Unemployment Rate - Black or African American" = 'LNS14000006',
  "(Seas) Not in Labor Force Black or African American" = 'LNS15000006',
  "(Seas) Civilian Labor Force Level - 20 yrs. & over, Black or African American Men" = 'LNS11000031',
  "(Seas) Labor Force Participation Rate - 20 yrs. & over, Black or African American Men" = 'LNS11300031',
  "(Seas) Employment Level - 20 yrs. & over, Black or African American Men" = 'LNS12000031',
  "(Seas) Employment-Population Ratio - 20 yrs. & over, Black or African American Men" = 'LNS12300031',
  "(Seas) Unemployment Level - 20 yrs. & over, Black or African American Men" = 'LNS13000031',
  "(Seas) Unemployment Rate - 20 yrs. & over, Black or African American Men" = 'LNS14000031',
  "(Seas) Civilian Labor Force Level - 20 yrs. & over, Black or African American Women" = 'LNS11000032',
  "(Seas) Labor Force Participation Rate - 20 yrs. & over, Black or African American Women" = 'LNS11300032',
  "(Seas) Employment Level - 20 yrs. & over, Black or African American Women" = 'LNS12000032',
  "(Seas) Employment-Population Ratio - 20 yrs. & over, Black or African American Women" = 'LNS12300032',
  "(Seas) Unemployment Level - 20 yrs. & over, Black or African American Women" = 'LNS13000032',
  "(Seas) Unemployment Rate - 20 yrs. & over, Black or African American Women" = 'LNS14000032',
  "(Seas) Civilian Labor Force Level - 16-19 yrs., Black or African American" = 'LNS11000018',
  "(Seas) Labor Force Participation Rate - 16-19 yrs., Black or African American" = 'LNS11300018',
  "(Seas) Employment Level - 16-19 yrs., Black or African American" = 'LNS12000018',
  "(Seas) Employment-Population Ratio - 16-19 yrs., Black or African American" = 'LNS12300018',
  "(Seas) Unemployment Level - 16-19 yrs., Black or African American" = 'LNS13000018',
  "(Seas) Unemployment Rate - 16-19 yrs., Black or African American" = 'LNS14000018',
  "(Seas) Civilian Labor Force Level - Asian" = 'LNS11032183',
  "(Seas) Labor Force Participation Rate - Asian" = 'LNS11332183',
  "(Seas) Employment Level - Asian" = 'LNS12032183',
  "(Seas) Employment Population Ratio - Asian" = 'LNS12332183',
  "(Seas) Unemployment Level - Asian" = 'LNS13032183',
  "(Seas) Unemployment Rate - Asian" = 'LNS14032183',
  "(Seas) Not in Labor Force - Asian" = 'LNS15032183',
  
  # Table A3
  
  "(Seas) Civilian Labor Force Level - Hispanic or Latino" = 'LNS11000009',
  "(Seas) Labor Force Participation Rate - Hispanic or Latino" = 'LNS11300009',
  "(Seas) Employment Level - Hispanic or Latino" = 'LNS12000009',
  "(Seas) Employment-Population Ratio - Hispanic or Latino" = 'LNS12300009',
  "(Seas) Unemployment Level - Hispanic or Latino" = 'LNS13000009',
  "(Seas) Unemployment Rate - Hispanic or Latino" = 'LNS14000009',
  "(Seas) Not in Labor Force - Hispanic or Latino" = 'LNS15000009',
  "(Seas) Civilian Labor Force Level - 20 yrs. & over, Hispanic or Latino, Men" = 'LNS11000034',
  "(Seas) Labor Force Participation Rate - 20 yrs. & over, Hispanic or Latino, Men" = 'LNS11300034',
  "(Seas) Employment Level - 20 yrs. & over, Hispanic or Latino, Men" = 'LNS12000034',
  "(Seas) Employment Population Ratio - 20 and over, Hispanic or Latino, Men" = 'LNS12300034',
  "(Seas) Unemployment Level - 20 yrs. & over, Hispanic or Latino, Men" = 'LNS13000034',
  "(Seas) Unemployment Rate - 20 yrs. & over, Hispanic or Latino, Men" = 'LNS14000034',
  "(Seas) Civilian Labor Force Level - 20 yrs. & over, Hispanic or Latino, Women" = 'LNS11000035',
  "(Seas) Labor Force Participation Rate - 20 yrs. & over, Hispanic or Latino, Women" = 'LNS11300035',
  "(Seas) Employment Level - 20 yrs. & over, Hispanic or Latino, Women" = 'LNS12000035',
  "(Seas) Employment Population Ratio - 20 and over, Hispanic or Latino, Women" = 'LNS12300035',
  "(Seas) Unemployment Level - 20 yrs. & over, Hispanic or Latino, Women" = 'LNS13000035',
  "(Seas) Unemployment Rate - 20 yrs. & over, Hispanic or Latino, Women" = 'LNS14000035',
  "(Seas) Civilian Labor Force Level - 16-19 yrs., Hispanic or Latino" = 'LNS11000021',
  "(Seas) Labor Force Participation Rate - 16-19 yrs., Hispanic or Latino" = 'LNS11300021',
  "(Seas) Employment Level - 16-19 yrs., Hispanic or Latino" = 'LNS12000021',
  "(Seas) Employment-Population Ratio - 16-19 yrs. Hispanic or Latino" = 'LNS12300021',
  "(Seas) Unemployment Level - 16-19 yrs., Hispanic or Latino" = 'LNS13000021',
  "(Seas) Unemployment Rate - 16-19 yrs., Hispanic or Latino" = 'LNS14000021',
  
  # Table A4
  
  "(Seas) Civilian Labor Force Level - Less than a High School Diploma, 25 yrs. & over" = 'LNS11027659',
  "(Seas) Labor Force Participation Rate - Less than a High School Diploma, 25 yrs. & over" = 'LNS11327659',
  "(Seas) Employment Level - Less than a High School Diploma, 25 yrs. & over" = 'LNS12027659',
  "(Seas) Employment-Population Ratio - Less than a High School Diploma, 25 yrs. & over" = 'LNS12327659',
  "(Seas) Unemployment Level - Less than a High School Diploma, 25 yrs. & over" = 'LNS13027659',
  "(Seas) Unemployment Rate - Less than a High School Diploma, 25 yrs. & over" = 'LNS14027659',
  "(Seas) Civilian Labor Force Level - High School Graduates, No College, 25 yrs. & over" = 'LNS11027660',
  "(Seas) Labor Force Participation Rate - High School Graduates, No College, 25 yrs. & over" = 'LNS11327660',
  "(Seas) Employment Level - High School Graduates, No College, 25 yrs. & over" = 'LNS12027660',
  "(Seas) Employment-Population Ratio - High School Graduates, No College, 25 yrs. & over" = 'LNS12327660',
  "(Seas) Unemployment Level - High School Graduates, No College, 25 yrs. & over" = 'LNS13027660',
  "(Seas) Unemployment Rate - High School Graduates, No College, 25 yrs. & over" = 'LNS14027660',
  "(Seas) Civilian Labor Force Level - Some College or Associate Degree, 25 yrs. & over" = 'LNS11027689',
  "(Seas) Labor Force Participation Rate - Some College or Associate Degree, 25 yrs. & over" = 'LNS11327689',
  "(Seas) Employment Level - Some College or Associate Degree, 25 yrs. & over" = 'LNS12027689',
  "(Seas) Employment-Population Ratio - Some College or Associate Degree, 25 yrs. & over" = 'LNS12327689',
  "(Seas) Unemployment Level - Some College or Associate Degree, 25 yrs. & over" = 'LNS13027689',
  "(Seas) Unemployment Rate - Some College or Associate Degree, 25 yrs. & over" = 'LNS14027689',
  "(Seas) Civilian Labor Force Level - Bachelor's degree and higher, 25 yrs. & over" = 'LNS11027662',
  "(Seas) Labor Force Participation Rate - Bachelor's degree and higher, 25 yrs. & over" = 'LNS11327662',
  "(Seas) Employment Level - Bachelor's degree and higher, 25 yrs. & over" = 'LNS12027662',
  "(Seas) Employment-Population Ratio - Bachelor's degree and higher, 25 yrs. & over" = 'LNS12327662',
  "(Seas) Unemployment Level - Bachelor's degree and higher, 25 yrs. & over" = 'LNS13027662',
  "(Seas) Unemployment Rate - Bachelor's degree and higher, 25 yrs. & over" = 'LNS14027662',
  
  # Table A7
  
  "(Unadj) Population Level - Foreign born" = 'LNU00073395',
  "(Unadj) Civilian Labor Force Level - Foreign born" = 'LNU01073395',
  "(Unadj) Labor Force Participation Rate - Foreign born" = 'LNU01373395',
  "(Unadj) Employment Level - Foreign born" = 'LNU02073395',
  "(Unadj) Employment-Population Ratio - Foreign born" = 'LNU02373395',
  "(Unadj) Unemployment Level - Foreign born" = 'LNU03073395',
  "(Unadj) Unemployment Rate - Foreign born" = 'LNU04073395',
  "(Unadj) Not in Labor Force - Foreign born" = 'LNU05073395',
  "(Unadj) Population Level - Native born" = 'LNU00073413',
  "(Unadj) Civilian Labor Force Level - Native born" = 'LNU01073413',
  "(Unadj) Labor Force Participation Rate - Native born" = 'LNU01373413',
  "(Unadj) Employment Level - Native born" = 'LNU02073413',
  "(Unadj) Employment-Population Ratio - Native born" = 'LNU02373413',
  "(Unadj) Unemployment Level - Native born" = 'LNU03073413',
  "(Unadj) Unemployment Rate - Native born" = 'LNU04073413',
  "(Unadj) Not in Labor Force - Native born" = 'LNU05073413',
  
  "(Unadj) Population Level - Foreign born, Men" = 'LNU00073396',
  "(Unadj) Civilian Labor Force Level - Foreign born, Men" = 'LNU01073396',
  "(Unadj) Labor Force Participation Rate - Foreign born, Men" = 'LNU01373396',
  "(Unadj) Employment Level - Foreign born, Men" = 'LNU02073396',
  "(Unadj) Employment-Population Ratio - Foreign born, Men" = 'LNU02373396',
  "(Unadj) Unemployment Level - Foreign born, Men" = 'LNU03073396',
  "(Unadj) Unemployment Rate - Foreign born, Men" = 'LNU04073396',
  "(Unadj) Not in Labor Force - Foreign born, Men" = 'LNU05073396',
  "(Unadj) Population Level - Native born, Men" = 'LNU00073414',
  "(Unadj) Civilian Labor Force Level - Native born, Men" = 'LNU01073414',
  "(Unadj) Labor Force Participation Rate - Native born, Men" = 'LNU01373414',
  "(Unadj) Employment Level - Native born, Men" = 'LNU02073414',
  "(Unadj) Employment-Population Ratio - Native born, Men" = 'LNU02373414',
  "(Unadj) Unemployment Level - Native born, Men" = 'LNU03073414',
  "(Unadj) Unemployment Rate - Native born, Men" = 'LNU04073414',
  "(Unadj) Not in Labor Force - Native born, Men" = 'LNU05073414',  
  
  "(Unadj) Population Level - Foreign born, Women" = 'LNU00073397',
  "(Unadj) Civilian Labor Force Level - Foreign born, Women" = 'LNU01073397',
  "(Unadj) Labor Force Participation Rate - Foreign born, Women" = 'LNU01373397',
  "(Unadj) Employment Level - Foreign born, Women" = 'LNU02073397',
  "(Unadj) Employment-Population Ratio - Foreign born, Women" = 'LNU02373397',
  "(Unadj) Unemployment Level - Foreign born, Women" = 'LNU03073397',
  "(Unadj) Unemployment Rate - Foreign born, Women" = 'LNU04073397',
  "(Unadj) Not in Labor Force - Foreign born, Women" = 'LNU05073397',
  "(Unadj) Population Level - Native born, Women" = 'LNU00073415',
  "(Unadj) Civilian Labor Force Level - Native born, Women" = 'LNU01073415',
  "(Unadj) Labor Force Participation Rate - Native born, Women" = 'LNU01373415',
  "(Unadj) Employment Level - Native born, Women" = 'LNU02073415',
  "(Unadj) Employment-Population Ratio - Native born, Women" = 'LNU02373415',
  "(Unadj) Unemployment Level - Native born, Women" = 'LNU03073415',
  "(Unadj) Unemployment Rate - Native born, Women" = 'LNU04073415',
  "(Unadj) Not in Labor Force - Native born, Women" = 'LNU05073415',  
  
  
  ### Table A8 Everything under the header "Persons at Work Part Time" 
  
  "(Seas) Employment Level - Part Time for Economic Reasons, All Industries" = 'LNS12032194',
  "(Seas) Employment Level - Part Time for Economic Reasons, Slack Work or Business Conditions, All Industries" = 'LNS12032195',
  "(Seas) Employment Level - Part Time for Economic Reasons, Could Only Find Part-Time Work, All Industries" = 'LNS12032196',
  "(Seas) Number At Work 1 to 34 Hours, Usually Work Part Time Noneconomic Reasons" = 'LNS12005977',
  "(Seas) Employment Level - Part Time for Economic Reasons, Nonagricultural Industries" = 'LNS12032197',
  "(Seas) Employment Level - Part Time for Economic Reasons, Slack Work or Business Conditions, Nonagricultural Industries" = 'LNS12032198',
  "(Seas) Employment Level - Part Time for Economic Reasons, Could Only Find Part-Time Work, Nonagricultural Industries" = 'LNS12032199',
  "(Seas) Employment Level - Part Time for Noneconomic Reasons, Nonagricultural Industries" = 'LNS12032200',
  
  
  # Table A9
  
  "(Seas) Employed, Usually Work Full Time" = 'LNS12500000',
  "(Seas) Employed, Usually Work Part Time" = 'LNS12600000',
  "(Seas) Multiple Jobholders" = 'LNS12026619',
  "(Seas) Multiple Jobholders as a Percent of Employed" = 'LNS12026620',
  "(Unadj) Employment Level - Incorporated self employed" = 'LNU02048984',
  "(Seas) Employment Level - Unincorporated Self employed" = 'LNS12027714',
  
  ## Table A10
  
  "(Seas) Unemployment Rate - 16-19 yrs." = 'LNS14000012',
  "(Seas) Unemployment Rate - 16-17 yrs." = 'LNS14000086',
  "(Seas) Unemployment Rate - 18-19 yrs." = 'LNS14000088',
  "(Seas) Unemployment Rate - 20-24 yrs." = 'LNS14000036',
  "(Seas) Unemployment Rate - 25-54 yrs." = 'LNS14000060',
  "(Seas) Unemployment Rate - 25-34 yrs." = 'LNS14000089',
  "(Seas) Unemployment Rate - 35-44 yrs." = 'LNS14000091',
  "(Seas) Unemployment Rate - 45-54 yrs." = 'LNS14000093',
  "(Seas) Unemployment Rate - 55 yrs. & over" = 'LNS14024230',
  "(Seas) Unemployment Rate - 16-19 yrs., Men" = 'LNS14000013',
  "(Seas) Unemployment Rate - 16-17 yrs., Men" = 'LNS14000152',
  "(Seas) Unemployment Rate - 18-19 yrs., Men" = 'LNS14000154',
  "(Seas) Unemployment Rate - 20-24 yrs., Men" = 'LNS14000037',
  "(Seas) Unemployment Rate - 25-54 yrs., Men" = 'LNS14000061',
  "(Seas) Unemployment Rate - 25-34 yrs., Men" = 'LNS14000164',
  "(Seas) Unemployment Rate - 35-44 yrs., Men" = 'LNS14000173',
  "(Seas) Unemployment Rate - 45-54 yrs., Men" = 'LNS14000182',
  "(Seas) Unemployment Rate - 55 yrs. & over, Men" = 'LNS14024231',
  "(Seas) Unemployment Rate - 16-19 yrs., Women" = 'LNS14000014',
  "(Seas) Unemployment Rate - 16-17 yrs., Women" = 'LNS14000317',
  "(Seas) Unemployment Rate - 18-19 yrs., Women" = 'LNS14000319',
  "(Seas) Unemployment Rate - 20-24 yrs., Women" = 'LNS14000038',
  "(Seas) Unemployment Rate - 25-54 yrs., Women" = 'LNS14000062',
  "(Seas) Unemployment Rate - 25-34 yrs., Women" = 'LNS14000327',
  "(Seas) Unemployment Rate - 35-44 yrs., Women" = 'LNS14000334',
  "(Seas) Unemployment Rate - 45-54 yrs., Women" = 'LNS14000341',
  "(Seas) Unemployment Rate - 55 yrs. & over, Women" = 'LNS14024232',
  "(Seas) Unemployment Rate - Married Men" = 'LNS14000150',
  "(Seas) Unemployment Rate - Married Women" = 'LNS14000315',
  "(Unadj) Unemployment Rate - Women who Maintain Families" = 'LNU04000313',
  "(Seas) Unemployment Rate - Full Time Workers" = 'LNS14100000',
  "(Seas) Unemployment Rate - Part Time Workers" = 'LNS14200000',
  
  # Table A11
  
  ####   "(Seas) Unemployment Level" = 'LNS13000000',   ### this variable is already found above in Table A-1
  "(Seas) Unemployment Level - Job Losers" = 'LNS13023621',
  "(Seas) Unemployment Level - Job Losers on Layoff" = 'LNS13023653',
  "(Seas) Unemployment Level - Job Losers Not on Layoff" = 'LNS13025699',
  "(Seas) Unemployment Level - Permanent Job Losers" = 'LNS13026638',
  "(Seas) Unemployment Level - Persons who Completed Temporary Jobs" = 'LNS13026637',
  "(Seas) Unemployment Level - Job Leavers" = 'LNS13023705',
  "(Seas) Unemployment Level - Reentrants to Labor Force" = 'LNS13023557',
  "(Seas) Unemployment Level - New Entrants" = 'LNS13023569',
  
  # Table A12
  
  ###    "(Seas) Unemployment Level" = 'LNS13000000',    ### this variable is already found above in Table A-1
  "(Seas) Number Unemployed - Less than 5 Weeks" = 'LNS13008396',
  "(Seas) Number Unemployed - 5-14 Weeks" = 'LNS13008756',
  "(Seas) Number Unemployed - 15 Weeks & over" = 'LNS13008516',
  "(Seas) Number Unemployed - 15-26 Weeks" = 'LNS13008876',
  "(Seas) Number Unemployed - 27 Weeks & over" = 'LNS13008636',
  "(Seas) Average Weeks Unemployed" = 'LNS13008275',
  "(Seas) Median Weeks Unemployed" = 'LNS13008276',
  
  
  # Table A13
  
  "(Unadj) Employment Level - Management Professional and Related Occupations" = 'LNU02032201' ,
  "(Unadj) Unemployment Level - Management Professional and Related Occupations" = 'LNU03032215' ,
  "(Unadj) Unemployment Rate - Management Professional and Related Occupations" = 'LNU04032215' ,
  "(Unadj) Employment Level - Management Business and Financial Operations Occupations" = 'LNU02032202' ,
  "(Unadj) Unemployment Level - Management Business and Financial Operations Occupations" = 'LNU03032216' ,
  "(Unadj) Unemployment Rate - Management  Business  and Financial Operations Occupations" = 'LNU04032216' ,
  "(Unadj) Employment Level - Professional and Related Occupations" = 'LNU02032203' ,
  "(Unadj) Unemployment Level - Professional and Related Occupations" = 'LNU03032217' ,
  "(Unadj) Unemployment Rate - Professional and Related Occupations" = 'LNU04032217' ,
  "(Unadj) Employment Level - Service Occupations" = 'LNU02032204' ,
  "(Unadj) Unemployment Level - Service Occupations" = 'LNU03032218' ,
  "(Unadj) Unemployment Rate - Service Occupations" = 'LNU04032218' ,
  "(Unadj) Employment Level - Sales and Office Occupations" = 'LNU02032205' ,
  "(Unadj) Unemployment Level - Sales and Office Occupations" = 'LNU03032219' ,
  "(Unadj) Unemployment Rate - Sales and Office Occupations" = 'LNU04032219' ,
  "(Unadj) Employment Level - Sales and Related Occupations" = 'LNU02032206' ,
  "(Unadj) Unemployment Level - Sales and Related Occupations" = 'LNU03032220' ,
  "(Unadj) Unemployment Rate - Sales and Related Occupations" = 'LNU04032220' ,
  "(Unadj) Employment Level - Office and Administrative Support Occupations" = 'LNU02032207' ,
  "(Unadj) Unemployment Level - Office and Administrative Support Occupations" = 'LNU03032221' ,
  "(Unadj) Unemployment Rate - Office and Administrative Support Occupations" = 'LNU04032221' ,
  "(Unadj) Employment Level - Natural Resources  Construction  and Maintenance Occupations" = 'LNU02032208' ,
  "(Unadj) Unemployment Level - Natural Resources  Construction  and Maintenance Occupations" = 'LNU03032222' ,
  "(Unadj) Unemployment Rate - Natural Resources  Construction  and Maintenance Occupations" = 'LNU04032222' ,
  "(Unadj) Employment Level - Farming  Fishing  and Forestry Occupations" = 'LNU02032209' ,
  "(Unadj) Unemployment Level - Farming  Fishing  and Forestry Occupations" = 'LNU03032223' ,
  "(Unadj) Unemployment Rate - Farming  Fishing  and Forestry Occupations" = 'LNU04032223' ,
  "(Unadj) Employment Level - Construction and Extraction Occupations" = 'LNU02032210' ,
  "(Unadj) Unemployment Level - Construction and Extraction Occupations" = 'LNU03032224' ,
  "(Unadj) Unemployment Rate - Construction and Extraction Occupations" = 'LNU04032224'  ,
  "(Unadj) Employment Level - Installation  Maintenance  and Repair Occupations" = 'LNU02032211' ,
  "(Unadj) Unemployment Level - Installation  Maintenance  and Repair Occupations" = 'LNU03032225' ,
  "(Unadj) Unemployment Rate - Installation  Maintenance  and Repair Occupations" = 'LNU04032225' ,
  "(Unadj) Employment Level - Production  Transportation and Material Moving Occupations" = 'LNU02032212', 
  "(Unadj) Unemployment Level - Production  Transportation and Material Moving Occupations" = 'LNU03032226', 
  "(Unadj) Unemployment Rate - Production  Transportation and Material Moving Occupations" = 'LNU04032226',   
  "(Unadj) Employment Level - Production Occupations" = 'LNU02032213', 
  "(Unadj) Unemployment Level - Production Occupations" = 'LNU03032227', 
  "(Unadj) Unemployment Rate - Production Occupations" = 'LNU04032227',  
  "(Unadj) Employment Level - Transportation and Material Moving Occupations" = 'LNU02032214', 
  "(Unadj) Unemployment Level - Transportation and Material Moving Occupations" = 'LNU03032228', 
  "(Unadj) Unemployment Rate - Transportation and Material Moving Occupations" = 'LNU04032228',   
  
  
  # Table A14 
  
  "(Unadj) Unemployment Level - Nonagriculture, Private Wage and Salary Workers" = 'LNU03032229',
  "(Unadj) Unemployment Rate - Nonagriculture, Private Wage and Salary Workers" = 'LNU04032229',
  "(Unadj) Unemployment Level - Mining, quarrying, and oil and gas extraction, Nonagricultural Private Wage and Salary Workers" = 'LNU03032230',
  "(Unadj) Unemployment Rate - Mining, quarrying, and oil and gas extraction, Nonagricultural Private Wage and Salary Workers" = 'LNU04032230',
  "(Unadj) Unemployment Level - Construction Industry, Private Wage and Salary Workers" = 'LNU03032231',
  "(Unadj) Unemployment Rate - Construction Industry, Private Wage and Salary Workers" = 'LNU04032231',
  "(Unadj) Unemployment Level - Manufacturing Industry, Private Wage and Salary Workers" = 'LNU03032232',
  "(Unadj) Unemployment Rate - Manufacturing Industry, Private Wage and Salary Workers" = 'LNU04032232',
  "(Unadj) Unemployment Level - Wholesale and Retail Trade, Private Wage and Salary Workers" = 'LNU03032235',
  "(Unadj) Unemployment Rate - Wholesale and Retail Trade, Private Wage and Salary Workers" = 'LNU04032235',
  "(Unadj) Unemployment Level - Transportation and Utilities Industry, Private Wage and Salary Workers" = 'LNU03032236',
  "(Unadj) Unemployment Rate - Transportation and Utilities Industry, Private Wage and Salary Workers" = 'LNU04032236',
  "(Unadj) Unemployment Level - Information Industry, Private Wage and Salary Workers" = 'LNU03032237',
  "(Unadj) Unemployment Rate - Information Industry, Private Wage and Salary Workers" = 'LNU04032237',
  "(Unadj) Unemployment Level - Financial Activities Industry, Private Wage and Salary Workers" = 'LNU03032238',
  "(Unadj) Unemployment Rate - Financial Activities Industry, Private Wage and Salary Workers" = 'LNU04032238',
  "(Unadj) Unemployment Level - Professional and Business Services Industry, Private Wage and Salary Workers" = 'LNU03032239',
  "(Unadj) Unemployment Rate - Professional and Business Services Industry, Private Wage and Salary Workers" = 'LNU04032239',
  "(Unadj) Unemployment Level - Education and Health Services, Private Wage and Salary Workers" = 'LNU03032240',
  "(Unadj) Unemployment Rate - Education and Health Services, Private Wage and Salary Workers" = 'LNU04032240',
  "(Unadj) Unemployment Level - Leisure and Hospitality, Private Wage and Salary Workers" = 'LNU03032241',
  "(Unadj) Unemployment Rate - Leisure and Hospitality, Private Wage and Salary Workers" = 'LNU04032241',
  "(Unadj) Unemployment Level - Other Services Industry, Private Wage and Salary Workers" = 'LNU03032242',
  "(Unadj) Unemployment Rate - Other Services Industry, Private Wage and Salary Workers" = 'LNU04032242',
  "(Unadj) Unemployment Level - Agricultural and Related Private Wage and Salary Workers" = 'LNU03035109',
  "(Unadj) Unemployment Rate - Agricultural and Related Private Wage and Salary Workers" = 'LNU04035109',
  "(Unadj) Unemployment Level All Industries Government Wage & Salary Workers" = 'LNU03028615',
  "(Unadj) Unemployment Rate All Industries Government Wage & Salary Workers" = 'LNU04028615',
  "(Unadj) Unemployment Level - All Industries, Self-employed, Unincorporated, and Unpaid family workers" = 'LNU03035181',
  "(Unadj) Unemployment Rate - All Industries, Self-employed, Unincorporated, and Unpaid family workers" = 'LNU04035181',
  
  
  #### B1 Table
  
  '(Seas) Total nonfarm employment' = 'CES0000000001',
  '(Seas) Total private employment' = 'CES0500000001',
  '(Seas) Goods-producing employment' = 'CES0600000001',
  '(Seas) Mining and logging employmnet' = 'CES1000000001',
  '(Seas) Logging employment' = 'CES1011330001',
  '(Seas) Mining, quarrying, and oil and gas extraction employment' = 'CES1021000001',
  '(Seas) Oil and gas extraction' = 'CES1021100001',
  '(Seas) Mining except oil and gas' = 'CES1021200001',
  '(Seas) Support activities for mining' = 'CES1021300001',
  '(Seas) Construction' = 'CES2000000001',
  '(Seas) Construction of buildings' = 'CES2023600001',
  '(Seas) Residential building construction' = 'CES2023610001',
  '(Seas) Nonresidential building construction' = 'CES2023620001',
  '(Seas) Heavy and civil engineering construction' = 'CES2023700001',
  '(Seas) Specialty trade contractors' = 'CES2023800001',
  '(Seas) Residential specialty trade contractors' = 'CES2023800101',
  '(Seas) Nonresidential specialty contractors' = 'CES2023800201',
  '(Seas) Manufacturing' = 'CES3000000001',
  '(Seas) Manufacturing durable goods' = 'CES3100000001',
  '(Seas) Wood product manufacturing' = 'CES3132100001',
  '(Seas) Nonmetallic mineral product manufacturing' = 'CES3132700001',
  '(Seas) Primary metal manufacturing' = 'CES3133100001',
  '(Seas) Fabricated metal product manufacturing' = 'CES3133200001',
  '(Seas) Machinery manufacturing' = 'CES3133300001',
  '(Seas) Computer and electronic product manufacturing' = 'CES3133400001',
  '(Seas) Electrical equipment, appliance, and component manufacturing' = 'CES3133500001',
  '(Seas) Transportation equipment manufacturing' = 'CES3133600001',
  '(Seas) Motor vehicles and parts' = 'CES3133600101',
  '(Seas) Furniture and related product manufacturing' = 'CES3133700001',
  '(Seas) Miscellaneous manufacturing' = 'CES3133900001',
  '(Seas) Manufacturing nondurable goods' = 'CES3200000001',
  '(Seas) Food manufacturing' = 'CES3231100001',
  '(Seas) Textile mills' = 'CES3231300001',
  '(Seas) Textile product mills' = 'CES3231400001',
  '(Seas) Apparel manufacturing' = 'CES3231500001',
  '(Seas) Paper manufacturing' = 'CES3232200001',
  '(Seas) Printing and related support activities' = 'CES3232300001',
  '(Seas) Petroleum and coal products manufacturing' = 'CES3232400001',
  '(Seas) Chemical manufacturing' = 'CES3232500001',
  '(Seas) Plastics and rubber products manufacturing' = 'CES3232600001',
  '(Seas) Beverage, tobacco, and leather and allied product manufacturing' = 'CES3232900001',
  '(Seas) Private service-providing' = 'CES0800000001',
  '(Seas) Trade, transportation, and utilities' = 'CES4000000001',
  '(Seas) Wholesale trade' = 'CES4142000001',
  '(Seas) Retail trade' = 'CES4200000001',
  '(Seas) Motor vehicle and parts dealers' = 'CES4244100001',
  '(Seas) Building material and garden equipment and supplies dealers' = 'CES4244400001',
  '(Seas) Food and beverage retailers' = 'CES4244500001',
  '(Seas) Furniture, home furnishings, electronics, and appliance retailers' = 'CES4244900001',
  '(Seas) Furniture and home furnishings retailers' = 'CES4244910001',
  '(Seas) Electronics and appliance retailers' = 'CES4244920001',
  '(Seas) General merchandise retailers' = 'CES4245500001',
  '(Seas) Department stores' = 'CES4245510001',
  '(Seas) Warehouse clubs, supercenters, and other general merchandise retailers' = 'CES4245520001',
  '(Seas) Health and personal care retailers' = 'CES4245600001',
  '(Seas) Gasoline stations and fuel dealers' = 'CES4245700001',
  '(Seas) Clothing, clothing accessories, shoe, and jewelry retailers' = 'CES4245800001',
  '(Seas) Sporting goods, hobby, musical instrument, book, and miscellaneous retailers' = 'CES4245900001',
  '(Seas) Transportation and warehousing' = 'CES4300000001',
  '(Seas) Air transportation' = 'CES4348100001',
  '(Seas) Rail transportation' = 'CES4348200001',
  '(Seas) Water transportation' = 'CES4348300001',
  '(Seas) Truck transportation' = 'CES4348400001',
  '(Seas) Transit and ground passenger transportation' = 'CES4348500001',
  '(Seas) Pipeline transportation' = 'CES4348600001',
  '(Seas) Scenic and sightseeing transportation' = 'CES4348700001',
  '(Seas) Support activities for transportation' = 'CES4348800001',
  '(Seas) Couriers and messengers' = 'CES4349200001',
  '(Seas) Warehousing and storage' = 'CES4349300001',
  '(Seas) Utilities' = 'CES4422000001',
  '(Seas) Information' = 'CES5000000001',
  '(Seas) Motion picture and sound recording industries' = 'CES5051200001',
  '(Seas) Publishing industries' = 'CES5051300001',
  '(Seas) Broadcasting and content providers' = 'CES5051600001',
  '(Seas) Telecommunications' = 'CES5051700001',
  '(Seas) Computing infrastructure providers, data processing, web hosting, and related services' = 'CES5051800001',
  '(Seas) Web search portals, libraries, archives, and other information services' = 'CES5051900001',
  '(Seas) Financial activities' = 'CES5500000001',
  '(Seas) Finance and insurance' = 'CES5552000001',
  '(Seas) Monetary authorities-central bank' = 'CES5552100001',
  '(Seas) Credit intermediation and related activities' = 'CES5552200001',
  '(Seas) Securities, commodity contracts, funds, trusts, and other financial vehicles, investments, and related activities' = 'CES5552300001',
  '(Seas) Insurance carriers and related activities' = 'CES5552400001',
  '(Seas) Real estate and rental and leasing' = 'CES5553000001',
  '(Seas) Professional and business services' = 'CES6000000001',
  '(Seas) Professional, scientific, and technical services' = 'CES6054000001',
  '(Seas) Legal services' = 'CES6054110001',
  '(Seas) Accounting, tax preparation, bookkeeping, and payroll services' = 'CES6054120001',
  '(Seas) Architectural, engineering, and related services' = 'CES6054130001',
  '(Seas) Specialized design services' = 'CES6054140001',
  '(Seas) Computer systems design and related services' = 'CES6054150001',
  '(Seas) Management, scientific, and technical consulting services' = 'CES6054160001',
  '(Seas) Scientific research and development services' = 'CES6054170001',
  '(Seas) Advertising, public relations, and related services' = 'CES6054180001',
  '(Seas) Other professional, scientific, and technical services' = 'CES6054190001',
  '(Seas) Management of companies and enterprises' = 'CES6055000001',
  '(Seas) Administrative and support and waste management and remediation services' = 'CES6056000001',
  '(Seas) Administrative and support services' = 'CES6056100001',
  '(Seas) Office administrative services' = 'CES6056110001',
  '(Seas) Facilities support services' = 'CES6056120001',
  '(Seas) Employment services' = 'CES6056130001',
  '(Seas) Temporary help services' = 'CES6056132001',
  '(Seas) Business support services' = 'CES6056140001',
  '(Seas) Travel arrangement and reservation services' = 'CES6056150001',
  '(Seas) Investigation and security services' = 'CES6056160001',
  '(Seas) Services to buildings and dwellings' = 'CES6056170001',
  '(Seas) Other support services' = 'CES6056190001',
  '(Seas) Waste management and remediation services' = 'CES6056200001',
  '(Seas) Private education and health services' = 'CES6500000001',
  '(Seas) Private educational services' = 'CES6561000001',
  '(Seas) Health care and social assistance' = 'CES6562000001',
  '(Seas) Health care' = 'CES6562000101',
  '(Seas) Ambulatory health care services' = 'CES6562100001',
  '(Seas) Outpatient care centers' = 'CES6562140001',
  '(Seas) Medical and diagnostic laboratories' = 'CES6562150001',
  '(Seas) Home health care services' = 'CES6562160001',
  '(Seas) Other ambulatory health care services' = 'CES6562190001',
  '(Seas) Hospitals' = 'CES6562200001',
  '(Seas) Nursing and residential care facilities' = 'CES6562300001',
  '(Seas) Skilled nursing care facilities' = 'CES6562310001',
  '(Seas) Residential intellectual and developmental disability, mental health, and substance abuse facilities' = 'CES6562320001',
  '(Seas) Continuing care retirement communities and assisted living facilities for the elderly' = 'CES6562330001',
  '(Seas) Other residential care facilities' = 'CES6562390001',
  '(Seas) Social assistance' = 'CES6562400001',
  '(Seas) Individual and family services' = 'CES6562410001',
  '(Seas) Community food and housing, and emergency and other relief services' = 'CES6562420001',
  '(Seas) Vocational rehabilitation services' = 'CES6562430001',
  '(Seas) Child care services' = 'CES6562440001',
  '(Seas) Leisure and hospitality' = 'CES7000000001',
  '(Seas) Arts, entertainment, and recreation' = 'CES7071000001',
  '(Seas) Performing arts, spectator sports, and related industries' = 'CES7071100001',
  '(Seas) Museums, historical sites, and similar institutions' = 'CES7071200001',
  '(Seas) Amusement, gambling, and recreation industries' = 'CES7071300001',
  '(Seas) Accommodation and food services' = 'CES7072000001',
  '(Seas) Accommodation' = 'CES7072100001',
  '(Seas) Food services and drinking places' = 'CES7072200001',
  '(Seas) Other services' = 'CES8000000001',
  '(Seas) Repair and maintenance' = 'CES8081100001',
  '(Seas) Personal and laundry services' = 'CES8081200001',
  '(Seas) Religious, grantmaking, civic, professional, and similar organizations' = 'CES8081300001',
  '(Seas) Government' = 'CES9000000001',
  '(Seas) Federal government' = 'CES9091000001',
  '(Seas) Federal government, except U.S. Postal Service' = 'CES9091100001',
  '(Seas) U.S. Postal Service' = 'CES9091912001',
  '(Seas) State government' = 'CES9092000001',
  '(Seas) State government education' = 'CES9092161101',
  '(Seas) State government, excluding education' = 'CES9092200001',
  '(Seas) Local government' = 'CES9093000001',
  '(Seas) Local government education' = 'CES9093161101',
  '(Seas) Local government, excluding education' = 'CES9093200001')

neededseriesid2 = unname(neededseriesid)

blskey <- 'b66e64c70206434db398b61c1f9edd8a'

# Setup for data extraction -----------------------------------------------

master <- list()

apiseq <- seq(1,length(neededseriesid),by=50)

lower <- 1
upper <- 2

# Extracting data for 2003 to 2022 ----------------------------------------

while(upper < (length(apiseq) + 2)){
  
  if(upper<=length(apiseq)){
    
    master[[paste0('1st Pull #',lower)]] <- bls_api(neededseriesid2[apiseq[lower]:(apiseq[upper]-1)],
                                            startyear = 2003,
                                            endyear = 2023,
                                            registrationKey = blskey)
    
  }else if(upper>length(apiseq) & upper < (length(apiseq) + 2)){
    
    master[[paste0('1st Pull #',lower)]] <- bls_api(neededseriesid2[apiseq[lower]:length(neededseriesid2)],
                                            startyear = 2003,
                                            endyear = 2023,
                                            registrationKey = blskey)
    
  }else{
    
    print(paste0('All done with ', lower, ' intervals!'))
    
  }
  
  lower <- lower + 1
  upper <- upper + 1
  
}

# Extracting data for 2023 onward -----------------------------------------

lower <- 1 
upper <- 2

while(upper < (length(apiseq) + 2)){
  
  if(upper<=length(apiseq)){
    
    master[[paste0('2nd Pull #',lower)]] <- bls_api(neededseriesid2[apiseq[lower]:(apiseq[upper]-1)],
                                                    startyear = 2023,
                                                    endyear = year(Sys.Date()),
                                                    registrationKey = blskey)
    
  }else if(upper>length(apiseq) & upper < (length(apiseq) + 2)){
    
    master[[paste0('2nd Pull #',lower)]] <- bls_api(neededseriesid2[apiseq[lower]:length(neededseriesid2)],
                                                    startyear = 2023,
                                                    endyear = year(Sys.Date()),
                                                    registrationKey = blskey)
    
  }else{
    
    print(paste0('All done with ', lower, ' intervals!'))
    
  }
  
  lower <- lower + 1
  upper <- upper + 1
  
}

# Binding Rows Together and Applying Variable Names -----------------------

blspull <- bind_rows(master)

#Data frame containing names of each variable
namedataframe <- as.data.frame(neededseriesid) %>% 
  tibble::rownames_to_column(.,'VarName')

blspull2 <- blspull %>% 
  left_join(namedataframe,
            by = c('seriesID' = 'neededseriesid')) %>% 
  mutate(date = readr::parse_date(paste0(year,' ',periodName),'%Y %B')) %>% 
  select(-c(footnotes,latest))

# Writing file to csv -----------------------------------------------------

readr::write_csv(blspull2,
                 'Data/working_bls_dataset.csv')
