## Hw 03: Mohammad Nur Nobi

## first step to re-generate census1 and census02 data set.

rm(list=ls())

#PART 1

library(rvest) #rvest is used to scrape the New York Times website for the needed data.
library(tidyverse)
library(sf)
library(tidycensus)

#PART 2

#Generate data from Census API
#Pre-defining variables to be used in loop
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
`West Virginiacensus`$County<-trimws(gsub(" County, Utah","",`West Virginiacensus`$NAME))

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
`West Virginiacensus`$County<-trimws(gsub(" County, Utah","",`West Virginiacensus`$NAME))

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



#PART 1

countypres_2000_2020 <- read.csv(file.choose())
#write.csv(df,"countypres_2000_2020.csv")

df_clean <- countypres_2000_2020 %>%
  select(year,state,county_name,party,candidatevotes) %>%
  group_by(year,state,county_name,party) %>%
  summarise(TotalVotes = sum(candidatevotes))

df_mystates_2020 <- df_clean %>%
  spread(party,TotalVotes) %>%
  filter(year == 2020, state %in% c("INDIANA", "KENTUCKY","OHIO","PENNSYLVANIA","WEST VIRGINIA"))%>%
  rename("DEM20" = "DEMOCRAT",
         "REP20" = "REPUBLICAN")
df_mystates_2016 <- df_clean %>%
  spread(party,TotalVotes) %>%
  filter(year == 2016, state %in% c("INDIANA", "KENTUCKY","OHIO","PENNSYLVANIA","WEST VIRGINIA"))%>%
  rename("DEM16" = "DEMOCRAT",
         "REP16" = "REPUBLICAN")
votes_20_16 <- cbind(df_mystates_2016,df_mystates_2020)

vdiff <- function(x,y){
  differ<-((x-y)/(x+y))
  return(differ)
}

D_VOTES <- votes_20_16 %>%
  mutate(D_DEM = vdiff(DEM20,DEM16),
         D_REP = vdiff(REP20,REP16))%>%
  select("state...2","county_name...3","D_DEM","D_REP")%>%
  rename("state" = "state...2","county_name"="county_name...3")



#PART 2

#2.I

CENSUS.2 <- CENSUS.2 %>%
  mutate(county_name = toupper(CENSUS.2$County))


D_VOTES<-D_VOTES[order(D_VOTES$county_name),]
CENSUS.2<-CENSUS.2[order(CENSUS.2$county_name),]
D_VOTES$county_name==CENSUS.2$county_name

df_merged<-merge(CENSUS.2,D_VOTES,by.x=c("NAME","state"), by.y=c("county_name","state"),all=TRUE)

merged <- merge(D_VOTES,CENSUS.2,by = "county_name", all = TRUE)

merged_1 <- cbind(CENSUS.3,D_VOTES)%>%
  select(!c(state.1,County))


#2.II
library(ggplot2)
require(cowplot)

### Vote change Mapping for the state of INDIANA

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

df_vdiff_IN <- D_VOTES %>%
  filter(state == "INDIANA")

IN_VDIFF <- IN2016_PCT
IN_VDIFF$dem <- df_vdiff_IN$D_DEM 
IN_VDIFF$rep <- df_vdiff_IN$D_REP 
IN_dem<-ggplot(IN_VDIFF)+
  geom_sf(aes(fill = dem))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Vote Change (IN-DEM)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


IN_rep<-ggplot(IN_VDIFF)+
  geom_sf(aes(fill = rep))+
  scale_fill_gradient(low="cornflowerblue",high="chocolate2",limits=c(0,1),aes(name="Percent
Vote Change (IN-REP)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


plot_grid(IN_dem,IN_rep)



### Vote change Mapping for the state of OHIO

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

df_vdiff_OH <- D_VOTES %>%
  filter(state == "OHIO")

OH_VDIFF <- OH2016_PCT[1:88,]
OH_VDIFF$dem <- df_vdiff_OH$D_DEM 
OH_VDIFF$rep <- df_vdiff_OH$D_REP 
OH_dem<-ggplot(OH_VDIFF)+
  geom_sf(aes(fill = dem))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Vote Change (OH-DEM)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


OH_rep<-ggplot(OH_VDIFF)+
  geom_sf(aes(fill = rep))+
  scale_fill_gradient(low="cornflowerblue",high="chocolate2",limits=c(0,1),aes(name="Percent
Vote Change (OH-REP)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


plot_grid(OH_dem,OH_rep)


### Vote change Mapping for the state of KENTUCKY


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

KY2016_PCT<- KN2016.acs %>% 
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


df_vdiff_KY <- D_VOTES %>%
  filter(state == "KENTUCKY")
df_vdiff_KY <- df_vdiff_KY[1:88,]
KY_VDIFF <- KY2016_PCT
KY_VDIFF$dem <- df_vdiff_KY$D_DEM 
KY_VDIFF$rep <- df_vdiff_KY$D_REP 
KY_dem<-ggplot(KY_VDIFF)+
  geom_sf(aes(fill = dem))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Vote Change (KY-DEM)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


KY_rep<-ggplot(KY_VDIFF)+
  geom_sf(aes(fill = rep))+
  scale_fill_gradient(low="cornflowerblue",high="chocolate2",limits=c(0,1),aes(name="Percent
Vote Change (KY-REP)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


plot_grid(KY_dem,KY_rep)


### Vote change Mapping for the state of Pennsylvania

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


df_vdiff_PA <- D_VOTES %>%
  filter(state == "PENNSYLVANIA")

PA_VDIFF <- PA2016_PCT
PA_VDIFF$dem <- df_vdiff_PA$D_DEM 
PA_VDIFF$rep <- df_vdiff_PA$D_REP 
PA_dem<-ggplot(PA_VDIFF)+
  geom_sf(aes(fill = dem))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Vote Change (PA-DEM)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


PA_rep<-ggplot(PA_VDIFF)+
  geom_sf(aes(fill = rep))+
  scale_fill_gradient(low="cornflowerblue",high="chocolate2",limits=c(0,1),aes(name="Percent
Vote Change (PA-REP)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


plot_grid(PA_dem,PA_rep)



### Vote change Mapping for the state of West Virginia

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



df_vdiff_WV <- D_VOTES %>%
  filter(state == "WEST VIRGINIA")

WV_VDIFF <- WV2016_PCT
WV_VDIFF$dem <- df_vdiff_WV$D_DEM 
WV_VDIFF$rep <- df_vdiff_WV$D_REP 
WV_dem<-ggplot(WV_VDIFF)+
  geom_sf(aes(fill = dem))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Vote Change (WV-DEM)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


WV_rep<-ggplot(WV_VDIFF)+
  geom_sf(aes(fill = rep))+
  scale_fill_gradient(low="cornflowerblue",high="chocolate2",limits=c(0,1),aes(name="Percent
Vote Change (WV-REP)"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


plot_grid(WV_dem,WV_rep)



### Part 03 (Regression models and combined results for Beamer presentation as
#LaTeX document class for creating presentation slides)


mod1<-lm(D_DEM~perMale+perWhite, data=merged)
summary(mod1)
mod2<-lm(D_REP~perMale+perWhite, data=merged)
summary(mod2)
mod3<-lm(D_DEM~PC_perMale+PC_perWhite, data=merged_1)
summary(mod3)
mod4<-lm(D_REP~PC_perMale+PC_perWhite, data=merged_1)
summary(mod4)

require(stargazer)
stargazer(mod1,mod2,mod3,mod4, type = "html",out="C:/Users/user/Documents/Github/econ691mnn/Build/Code/ECON691mnn/finalreg_1.html")

                                               \ECON691mnn
###adding state level fixed effects

mod5<-lm(D_DEM~perMale+perWhite+factor(state.x), data=merged)
summary(mod5)
mod6<-lm(D_REP~perMale+perWhite+factor(state.x), data=merged)
summary(mod6)
mod7<-lm(D_DEM~PC_perMale+PC_perWhite+factor(state), data=merged_1)
summary(mod7)
mod8<-lm(D_REP~PC_perMale+PC_perWhite+factor(state), data=merged_1)
summary(mod8)

stargazer(mod5,mod6,mod7,mod8, type = "html",out="C:/Users/user/Documents/Github/econ691mnn/Build/Code/ECON691mnn/finalreg_2.html")
                                                                                                               
                                                                          