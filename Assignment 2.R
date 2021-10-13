## Calling required libraries 
library(rvest)
library(tidyverse)


#List of the states for which data have been pulled on

states<-c("ohio", "kentucky","indiana","pennsylvania","west-virginia")

for(i in states){
  #Specifying the URL for desired website to be scraped
  url.1 <- "https://www.nytimes.com/elections/2016/results/"
  url<-paste0(url.1,i)
  webpage <- read_html(url)
  tables<-webpage %>%
    html_nodes("table") #This pulls out all the "table" nodes in the HTML code
  results<-tables[2] %>%
    html_table(fill=TRUE,header=TRUE) %>%
    as.data.frame() %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton" = (Clinton)/(Clinton+Trump),
           "pctTrump" = Trump/(Clinton+Trump))
  assign(i,results)
}

## Adding state name to each of the state's dataframe

df_IN <- data.frame(append(indiana, c(state='INDIANA'), after=1))
df_KY <- data.frame(append(kentucky, c(state='KENTUCKY'), after=1))
df_OH <- data.frame(append(ohio, c(state='OHIO'), after=1))
df_PY <- data.frame(append(pennsylvania, c(state='PENNSYLVANIA'), after=1))
df_WV <- data.frame(append(`west-virginia` , c(state='WEST-VIRGINIA'), after=1))



## Creating a single dataframe called VOTES which contains required information

df_list <- list(df_IN,df_KY,df_OH,df_PY,df_WV)
VOTES <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list, accumulate=FALSE)
save(VOTES, file = "C:/Users/user/Documents/Github/econ691mnn/Build/code/ECON691mnn/VOTES1.RData") 

# Alternatively: this will produce a single dataframe state by state
#VOTES1 <- do.call("rbind", list(df_IN,df_KY,df_OH,df_PY,df_WV))


### Part 02 of HW2

require(tidyverse)
require(dplyr)
require(rvest)
require(cowplot)
require(ggplot2)
library(tidycensus)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002",
        "B02001_003","B05001_001","B05001_006","B07001_001",
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")

#State IDs: 18,21,39,42,54
#Command to pull data from ACS for 2016 


census_api_key("e9f020c5be95751b6d18d6942c684cf924110885",overwrite=T,install = TRUE)
readRenviron("~/.Renviron")

#Data file and calculation for Indiana----

CENSUS_IN <- get_acs(geography = "county", #defines geography level of data
variables = vars, #specifics the data we want
state = 18, # 18 Indiana 
year = 2016, #denotes the year
geometry = TRUE) #downloads the TIGER shape file data

IN2016.acs<-CENSUS_IN %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

IN2016_PCT<- IN2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_IN19 <- get_acs(geography = "county", #defines geography level of data
                     variables = vars, #specifics the data we want
                     state = 18, # 18 Indiana 
                     year = 2019, #denotes the year
                     geometry = TRUE) #downloads the TIGER shape file data

IN2019.acs<-CENSUS_IN19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

IN2019_PCT<- IN2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")

#Data file and calculation for Ohio----

CENSUS_OH <- get_acs(geography = "county", #defines geography level of data
                     variables = vars, #specifics the data we want
                     state = 21, # 21 Ohio 
                     year = 2016, #denotes the year
                     geometry = TRUE) #downloads the TIGER shape file data

OH2016.acs<-CENSUS_OH %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

OH2016_PCT<- OH2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_OH19 <- get_acs(geography = "county", #defines geography level of data
                       variables = vars, #specifics the data we want
                       state = 21, # 21 Ohio 
                       year = 2019, #denotes the year
                       geometry = TRUE) #downloads the TIGER shape file data

OH2019.acs<-CENSUS_OH19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

OH2019_PCT<- OH2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")



#Data file and calculation for Kentucky----

CENSUS_KN <- get_acs(geography = "county", #defines geography level of data
                     variables = vars, #specifics the data we want
                     state = 39, # 39 Kentucky 
                     year = 2016, #denotes the year
                     geometry = TRUE) #downloads the TIGER shape file data

KN2016.acs<-CENSUS_KN %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

KN2016_PCT<- KN2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_KN19 <- get_acs(geography = "county", #defines geography level of data
                       variables = vars, #specifics the data we want
                       state = 39, # 39 Kentucky  
                       year = 2019, #denotes the year
                       geometry = TRUE) #downloads the TIGER shape file data

KN2019.acs<-CENSUS_KN19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

KN2019_PCT<- KN2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")



#Data file and calculation for Pennsylvania----

CENSUS_PA <- get_acs(geography = "county", #defines geography level of data
                     variables = vars, #specifics the data we want
                     state = 42, # 42 Pennsylvania 
                     year = 2016, #denotes the year
                     geometry = TRUE) #downloads the TIGER shape file data

PA2016.acs<-CENSUS_PA %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

PA2016_PCT<- PA2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_PA19 <- get_acs(geography = "county", #defines geography level of data
                       variables = vars, #specifics the data we want
                       state = 42, # 42 Pennsylvania 
                       year = 2019, #denotes the year
                       geometry = TRUE) #downloads the TIGER shape file data

PA2019.acs<-CENSUS_PA19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

PA2019_PCT<- PA2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")



#Data file and calculation for West-Virginia----

CENSUS_WV <- get_acs(geography = "county", #defines geography level of data
                     variables = vars, #specifics the data we want
                     state = 54, # 54 west-Virginia 
                     year = 2016, #denotes the year
                     geometry = TRUE) #downloads the TIGER shape file data

WV2016.acs<-CENSUS_WV %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

WV2016_PCT<- WV2016.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


CENSUS_WV19 <- get_acs(geography = "county", #defines geography level of data
                       variables = vars, #specifics the data we want
                       state = 54, # 54 West-Virginia 
                       year = 2019, #denotes the year
                       geometry = TRUE) #downloads the TIGER shape file data

WV2019.acs<-CENSUS_WV19 %>%
  mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                               variable=="B01001_002" ~ "Male",
                               variable=="B02001_001" ~ "TotRace",
                               variable=="B02001_002" ~ "White",
                               variable=="B02001_003" ~ "Black",
                               variable=="B05001_001" ~ "TotCit",
                               variable=="B05001_006" ~ "NonCit",
                               variable=="B07001_001" ~ "TotMob",
                               variable=="B07001_017" ~ "Stay",
                               variable=="B07001_033" ~ "SameCounty",
                               variable=="B07001_049" ~ "SameSt",
                               variable=="B07001_065" ~ "OthState",
                               variable=="B07001_081" ~ "Abroad",
                               TRUE ~ "other")) %>%
  select(!c(moe,variable)) %>%
  spread(key=variable2, value=estimate)

WV2019_PCT<- WV2019.acs %>% 
  mutate(perMale = Male/TotPop,
         perWhite = White/TotPop,
         perBlack = Black/TotPop,
         perCit = 1-(NonCit/TotCit),
         perStay = Stay/TotMob,
         perSameCounty = SameCounty/TotMob,
         perSameSt = SameSt/TotMob,
         perOthState = OthState/TotMob,
         perAbroad = Abroad/TotMob) %>%
  select("GEOID",starts_with("per"),"geometry")


IN2016_PCT$name <- IN2016.acs$NAME
IN2019_PCT$name <- IN2019.acs$NAME
OH2016_PCT$name <- OH2016.acs$NAME
OH2019_PCT$name <- OH2019.acs$NAME
KN2016_PCT$name <- KN2016.acs$NAME
KN2019_PCT$name <- KN2019.acs$NAME
PA2016_PCT$name <- PA2016.acs$NAME
PA2019_PCT$name <- PA2019.acs$NAME
WV2016_PCT$name <- WV2016.acs$NAME
WV2019_PCT$name <- WV2019.acs$NAME


CENSUS.1 <- do.call("rbind", list(IN2016_PCT, OH2016_PCT, KN2016_PCT, PA2016_PCT, WV2016_PCT))
save(list = "CENSUS.1", file = "C:/Users/user/Documents/Github/econ691mnn/Build/Code/ECON691mnn/CENSUS.1.RData")
                                            

CENSUS.2 <- do.call("rbind", list(IN2019_PCT, OH2019_PCT, KN2019_PCT, PA2019_PCT, WV2019_PCT))
save(list = "CENSUS.2", file = "C:/Users/user/Documents/Github/econ691mnn/Build/Code/ECON691mnn/CENSUS.2.RData")



### Creating CENSUS 3 data set----

library(rvest) 
library(tidyverse)
library(sf)
library(tidycensus)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003","B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )
states<-c("Indiana", "Kentucky","Ohio","Pennsylvania","West Virginia")
fips<-c(18, 21, 39, 42, 54)

#API Command


k<-1
for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars,
               state = i,
               year = 2016,
               geometry = TRUE)
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  assign(paste0(states[k],"census"),temp)
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  assign(paste0(states[k],"map"),map)
  k<-k+1
  rm(temp, map)
}


Indianacensus$County<-trimws(gsub(" County, Indiana","",Indianacensus$NAME))
Kentuckycensus$County<-trimws(gsub(" County, Kentucky","",Kentuckycensus$NAME))
Ohiocensus$County<-trimws(gsub(" County, Ohio","",Ohiocensus$NAME))
Pennsylvaniacensus$County<-trimws(gsub(" County, Pennsylvania","",Pennsylvaniacensus$NAME))
`West Virginiacensus`$County<-trimws(gsub(" County, West Virginia","",`West Virginiacensus`$NAME))

CENSUS.1 <- rbind(Indianacensus,Kentuckycensus,Ohiocensus,Pennsylvaniacensus,`West Virginiacensus`)

## Now pulling out 2019 data file

k<-1
for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars,
               state = i,
               year = 2019,
               geometry = TRUE)
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  assign(paste0(states[k],"census"),temp)
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  assign(paste0(states[k],"map"),map)
  k<-k+1
  rm(temp, map)
}


Indianacensus$County<-trimws(gsub(" County, Indiana","",Indianacensus$NAME))
Kentuckycensus$County<-trimws(gsub(" County, Kentucky","",Kentuckycensus$NAME))
Ohiocensus$County<-trimws(gsub(" County, Ohio","",Ohiocensus$NAME))
Pennsylvaniacensus$County<-trimws(gsub(" County, Pennsylvania","",Pennsylvaniacensus$NAME))
`West Virginiacensus`$County<-trimws(gsub(" County, West Virginia","",`West Virginiacensus`$NAME))

CENSUS.2 <- rbind(Indianacensus,Kentuckycensus,Ohiocensus,Pennsylvaniacensus,`West Virginiacensus`)



gamma <- function(x,y){
  temp<-(x-y)/(y)
  return(temp)
}


CENSUS.3 <- CENSUS.1 %>%
  mutate(PC_perMale=gamma(CENSUS.1$perMale,CENSUS.2$perMale),
         PC_perWhite=gamma(CENSUS.1$perWhite,CENSUS.2$perWhite),
         PC_perBlack=gamma(CENSUS.1$perBlack,CENSUS.2$perBlack),
         PC_perCit=gamma(CENSUS.1$perCit,CENSUS.2$perCit),
         PC_perStay=gamma(CENSUS.1$perStay,CENSUS.2$perStay),
         PC_perSameCounty=gamma(CENSUS.1$perSameCounty,CENSUS.2$perSameCounty),
         PC_perSameSt=gamma(CENSUS.1$perSameSt,CENSUS.2$perSameSt),
         PC_perOthState=gamma(CENSUS.1$perOthState,CENSUS.2$perOthState),
         PC_perAbroad=gamma(CENSUS.1$perAbroad,CENSUS.2$perAbroad)) %>%
  select("GEOID",starts_with("PC"),"County","state","geometry")


#CENSUS3 <- write.csv(CENSUS.3,"CENSUS3.csv")
save(list = "CENSUS.3", file = "C:/Users/user/Documents/Github/econ691mnn/Build/Code/ECON691mnn/CENSUS.3.RData")



## part 03

library(ggplot2)
require(cowplot)
p1_IN<-ggplot(IN2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (IN)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_IN<-ggplot(IN2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (IN)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_IN,p2_IN)

p1_OH<-ggplot(OH2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (OH)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_OH<-ggplot(OH2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (OH)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_OH,p2_OH)

p1_KY<-ggplot(KN2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (KY)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_KY<-ggplot(KN2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (KY)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_KY,p2_KY)

p1_PA<-ggplot(PA2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (PA)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_PA<-ggplot(PA2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (PA)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_PA,p2_PA)

p1_WV<-ggplot(WV2016_PCT)+
  geom_sf(aes(fill = perMale))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton (WV)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2_WV<-ggplot(WV2019_PCT)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White (WV)"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1_WV,p2_WV)


plot_grid(p1_IN,p2_IN,p1_OH,p2_OH,p1_KY,p2_KY,p1_PA,p2_PA,p1_WV,p2_WV, nrow = 3)
map1 <- plot_grid(p1_IN,p1_OH,p1_KY,p1_PA,p1_WV,nrow=5)


map2 <- plot_grid(p2_IN,p2_OH,p2_KY,p2_PA,p2_WV,nrow=5)
plot_grid(map1, map2,ncol = 2)

