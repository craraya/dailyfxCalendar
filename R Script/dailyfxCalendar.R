
##
## Extrae el calendario económico de la pagina
## https://www.dailyfx.com

library(RCurl)
library(XML)
library(RODBC)
library(lubridate)
library(dplyr)
library(stringr)
library(rvest) 

base_date <- ymd("2018-03-25") # el siguiente domingo del mes
end_date  <- ymd("2018-01-01") # la fecha a la cual queremos llegar
fecha <- ymd()

cnt <- 1
while(base_date >= end_date){
  if(wday(base_date)!=1) {
    print("Elegir un día Domingo")
    break
  }
  base_date  <- base_date - 7
  fecha[cnt] <- base_date
  print(fecha[cnt])
  cnt <- cnt + 1
}

week <- paste0(substr(fecha,1,4),"/",substr(fecha,6,7),substr(fecha,9,10))
tz <- "-3" ## Time Zone

###############################################
###############################################
###############################################
for(z in 1:length(week)){
  
  url_01   <- "https://www.dailyfx.com/calendar"
  url_02   <- "?previous=true"
  url_week <- "&week="
  url_tz   <- "&tz="
  url_F    <- paste0(url_01,url_02,url_week,week[z],url_tz,tz)
  
  page <- read_html(url_F)
  
  TAB <- html_nodes(page,"table")[[4]] %>% ## encabezados
    html_table()
  
  time <- html_nodes(page,".calendar-td") %>%
    html_nodes(".f16") %>%
    html_text()
  
  importance <- html_nodes(page,".calendar-td") %>%
    html_nodes(".hidden-xs") %>%
    html_text()
  
  ## nombre del Indicador
  nameInd <- c()
  actual <- c()
  forecast <- c()
  previous <- c()
  for(i in 1:length(time)){
    nameInd[i]  <- html_nodes(page,".calendar-td")[3+11*(i-1)]%>%
      html_text()
    actual[i]   <- html_nodes(page,".calendar-td")[5+11*(i-1)]%>%
      html_text()
    forecast[i] <- html_nodes(page,".calendar-td")[6+11*(i-1)]%>%
      html_text()
    previous[i] <- html_nodes(page,".calendar-td")[7+11*(i-1)]%>%
      html_text()
  }
  
  TT <- as.data.frame(cbind(time,importance,nameInd,actual,forecast,previous))
  
  #write.table(TT,"clipboard",sep="\t")
  
  let <- substr(TT$nameInd,1,1)
  let <- !is.na(as.numeric(let))
  
  ## sacamos el par
  parID <- c()
  parID[let] <- substr(TT$nameInd[let],6,8)
  TT <- mutate(TT,parID = parID)
  
  ## Limpiamos el nombre del indicador
  nameInd_ <- c()
  nameInd_[let] <- substr(TT$nameInd[let],10,999)
  nameInd_[!let] <- as.character(TT$nameInd[!let])
  TT <- mutate(TT,nameInd = nameInd_)
  
  ## Si es MoM: Month over Month
  TT <- mutate(TT,MoM = if_else(regexpr("MoM",nameInd)>0,1,0))
  ## Si es YoY: Year over Year
  TT <- mutate(TT,YoY = if_else(regexpr("YoY",nameInd)>0,1,0))
  
  ## Sacamos la Fecha por id
  ## Un poco bruto, asumimos que a lo mas hay 50 indicadores diarios
  aux <- c()
  IndTimeStamp <- c()
  cnt <- 1
  for(i in 0:10){
    for(j in 0:50){
      aux <- paste0("#date",i,"_",j)
      aux2 <- html_nodes(page,aux) %>% html_text()
      if(is.na(aux2[1])) next
      IndTimeStamp[cnt] <- html_nodes(page,aux) %>% html_text()
      cnt <- cnt + 1
    }
  }
  
  ## Timestamp
  TT <- mutate(TT,TimeStamp = ymd_hms(IndTimeStamp))
  
  ## Fecha
  TT <- mutate(TT, Date = ymd(substr(TT$TimeStamp,1,10)))
  
  ## Hora
  TT <- mutate(TT,Hora = hms(substr(TT$TimeStamp,12,19)))
  
  if(z==1) { TF <- TT }
  else TF <- rbind(TF,TT)
  
}

write.table(dplyr::filter(TF,parID=="USD"),"clipboard",sep="\t")

write.table(dplyr::filter(TF,regexpr("Consumer Price Index",nameInd)>0),"clipboard",sep="\t")

TF <- mutate(TF,Type = "")
TF$Type[regexpr("¥",TF$actual)>0] <- "¥"
TF$Type[regexpr("%",TF$actual)>0] <- "%"
TF$Type[regexpr("b",TF$actual)>0] <- "billions"
TF$Type[regexpr("m",TF$actual)>0] <- "millions"
TF$Type[regexpr("k",TF$actual)>0] <- "miles"




