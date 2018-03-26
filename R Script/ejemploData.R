
library(sqldf)
library(haven)
library(ggplot2)

pth <- "C:/Users/CAraya/Documents/CARAYA/"
AA_2013 <- read_sav(paste0(pth,"DailyFxCalendar_2013.sav",collapse=""),user_na = TRUE)

AA <- as.data.frame(rbind( AA_2013
            ,AA_2014
            ,AA_2015
            ,AA_2016
            ,AA_2017
            ,AA_2018))

AA <- AA_2013

IND <- as.data.frame(table(AA$nameInd))
P <- as.data.frame(table(AA$parID))

CPI_Tokio <- dplyr::filter(AA,regexpr("JPY",parID)>0 & regexpr("Tokyo",nameInd)>0
                     ,regexpr("Consumer Price Index",nameInd)>0 & regexpr("Ex-Fresh Food",nameInd)>0)

CPI_Japon <- dplyr::filter(AA,regexpr("JPY",parID)>0 & regexpr("National",nameInd)>0
                     ,regexpr("Consumer Price Index",nameInd)>0 & regexpr("Ex-Fresh Food",nameInd)>0)


## Graficamos ----
ggplot(CPI_Tokio,aes(x=Date,y=actual,col="CPI Tokio actual"))+geom_line()+
  geom_line(aes(y=forecast,col="CPI Tokio forecast"))

ggplot(CPI_Japon,aes(x=Date,y=actual,col="actual"))+geom_line()+
  geom_line(aes(y=forecast,col="forecast"))

