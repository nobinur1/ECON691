#First script to manipulate data
#Economics 691
#Mohammad Nur Nobi
#09/05/2021

#

x<-"Hello, my name is Mohammad Nur Nobi"
x

y<- -3
y

#This is a script to introduce the idea of scripts 
#created by J. R. Groves on August 30, 2021

rm(list=ls()) #clearing the global environment
install.packages("tidyverse")
library(tidyverse) #tells R to load the package tidyverse

delta <- function(x){
  temp<-(x-lag(x))/(lag(x))
  return(temp)
}

covidIL <- read.csv("./ILCovid19.csv")
head(covidIL)
covidIL <- covidIL %>%
  mutate(pc_test=delta(Tests),
         pc_cases=delta(Cases),
         pc_deaths=delta(Deaths))
covidIL$pc_test<-delta(covidIL$Tests)
covidIL$pc_cases<-delta(covidIL$Cases)
summary(covidIL)


#summ<-function(x){
#        return(summary(x))
#}
#object<-summ(x)

#xd<-runif(10,0,1) #random variable under uniform distribution
#summ(xd)

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)] <- NA
covidIL$pc_deaths[is.nan(covidIL$pc_deaths)] <- NA #converted NaN to NA
covidIL$Date1 = as.Date(covidIL$Date, "%m/%d/%Y") #created new column because it keeps saying <NA> when I try to overwrite Date column
plot(covidIL$Date1, covidIL$pc_deaths)
plot(covidIL$Date1, covidIL$pc_cases,
     main="% change of COVID cases",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")




head(covidIL)

#Assignment 1 
#function for DIF

DIF <- function(x){
  temp<-(x-lag(x))
  return(temp)
}
covidIL <- covidIL %>%
  mutate(dif_test=DIF(Tests),
         dif_cases=DIF(Cases),
         dif_deaths=DIF(Deaths))
head(covidIL)

covidIL <- covidIL %>%
  mutate(pc_dif_test=delta(dif_test),
         pc_dif_cases=delta(dif_cases),
         pc_dif_deaths=delta(dif_deaths))
covidIL$pc_dif_deaths[is.nan(covidIL$pc_dif_deaths)] <- NA #converted NaN to NA
head(covidIL)

plot(covidIL$Date1,covidIL$pc_dif_cases,
     main="% change of daily cases of COVID",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")
plot(covidIL$Date1,covidIL$pc_dif_test,
     main="% change of daily number of tests of COVID",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")
plot(covidIL$Date1,covidIL$pc_dif_deaths,
     main="% change of number of daily deaths of COVID",
     xlab="Date",
     ylab="",
     type="l",
     col="blue")
