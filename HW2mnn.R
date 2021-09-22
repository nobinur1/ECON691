## Calling required libraries 
rm(list=ls())
library(rvest) 
library(tidyverse)


#List of the states the data will be pulled for states

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


#Alternatively 
# indiana <- data.frame(indiana)
# statevector1 <- "state"
# indiana[ , statevector1] <- "IN"


## Creating a single dataframe called VOTES which contains required information

df_list <- list(df_IN,df_KY,df_OH,df_PY,df_WV)
VOTES <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list, accumulate=FALSE)
 
# Alternatively: this will produce a single dataframe state by state
VOTES1 <- do.call("rbind", list(df_IN,df_KY,df_OH,df_PY,df_WV))

show(VOTES)    #end of HW2(part1)

#ACS data Second part of the HW2
# ACS Key: e9f020c5be95751b6d18d6942c684cf924110885

library(tidycensus)
v<-load_variables(2016,"acs5")

vars<- c("B01001_001","B01001_002","B02001_001","B02001_002",
        "B02001_003","B05001_001","B05001_006","B07001_001",
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081”,)

#Command to pull data from ACS

my_states <- c("IN", "KY", "OH","PY", "WV")
#command to pull data from ACS
acs <- get_acs(geography = "county"
               variables = vars,     
               states = 18","21","39","42","54",
        year = "2016",        
        geometry=TRUE)


#acs <-acs(geography = "county",  #defines geography level of data
              variables = vars,       #specifics the data we want
              state = 18,             #denotes the specific state
              year = 2016,            #denotes the year
              geometry = TRUE)        #downloads the TIGER shapefile data#


               
head(get_acs)

states.acs<-get_acs %>%
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
                               TRUE=="other")) %>%

print()
  
select(!c(moe,variable2)) %>%
spread(key=variable2, value=estimate)
  
#FIPS code: Indiana 18, Ohio 39, Kentucky 21, Pennsylvenia 42, West virginia 54
  


