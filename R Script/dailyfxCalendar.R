
##
## Extrae el calendario económico de la pagina
## https://www.dailyfx.com

library(RCurl)      ## para el formato de la url
library(XML)        ## complemento de rvest
library(lubridate)  ## para las fechas
library(dplyr)      ## para manipulación de los datos
library(stringr)    ## para manejo de los strings
library(rvest)      ## para scraping
library(haven)      ## para la administración de los archivos
library(lettercase) ## para el uso y no uso de mayusculas en el html

## Algunas funciones ----
clean <- function(v){
  aux <- v
  aux <- gsub("¥","",aux)
  aux <- gsub("%","",aux)
  aux <- gsub("\\$","",aux)
  aux <- gsub("a","",aux)
  aux <- gsub("A","",aux)
  aux <- gsub("b","",aux)
  aux <- gsub("B","",aux)
  aux <- gsub("m","",aux)
  aux <- gsub("M","",aux)
  aux <- gsub("k","",aux)
  aux <- gsub("K","",aux)
  aux <- gsub("t","",aux)
  aux <- gsub("T","",aux)
  aux <- gsub(",","",aux)
  return(aux)
}

## Editamos las Fechas ----
## comprobador de fechas
wday(ymd("2015-01-04")) #==1 ## Es Domingo?

base_date <- ymd("2015-01-04") # el siguiente domingo del mes (primera semana del mes)
end_date  <- ymd("2013-12-30") # la fecha a la cual queremos llegar (ultima semana del mes)
fecha <- ymd()

cnt <- 1
while(base_date >= end_date){
  if(wday(base_date)!=1) {
    print("Elegir un día Domingo")
    break
  }
  fecha[cnt] <- base_date
  print(fecha[cnt])
  base_date  <- base_date - 7
  cnt <- cnt + 1
}

week <- paste0(substr(fecha,1,4),"/",substr(fecha,6,7),substr(fecha,9,10))
tz <- "-3" ## Time Zone 0

## Iteramos (demora 1 hr por año aprox) ----
system.time(
for(z in 1:length(week)){
  
  url_01   <- "https://www.dailyfx.com/calendar"
  url_02   <- "?previous=true"
  url_week <- "&week="
  url_tz   <- "&tz="
  url_F    <- paste0(url_01,url_02,url_week,week[z],url_tz,tz)
  
  htmlDoc <- read_html(url_F)
  #exists(htmlDoc, envir = myEnvir)
  
  #is.na(htmlDoc)
  
  TAB <- html_nodes(htmlDoc,"table")[[4]] %>% ## encabezados
    html_table()
  
  time <- html_nodes(htmlDoc,".calendar-td") %>%
    html_nodes(".f16") %>%
    html_text()
  
  importance <- html_nodes(htmlDoc,".calendar-td") %>%
    html_nodes(".hidden-xs") %>%
    html_text()
  
  ## nombre del Indicador
  nameInd <- c()
  actual <- c()
  forecast <- c()
  previous <- c()
  for(i in 1:length(time)){
    nameInd[i]  <- html_nodes(htmlDoc,".calendar-td")[3+11*(i-1)]%>%
      html_text()
    actual[i]   <- html_nodes(htmlDoc,".calendar-td")[5+11*(i-1)]%>%
      html_text()
    forecast[i] <- html_nodes(htmlDoc,".calendar-td")[6+11*(i-1)]%>%
      html_text()
    previous[i] <- html_nodes(htmlDoc,".calendar-td")[7+11*(i-1)]%>%
      html_text()
  }
  
  TT <- as.data.frame(cbind(time,importance,nameInd,actual,forecast,previous))
  
  ## Sacamos la Fecha por id
  ## Un poco bruto, asumimos que a lo mas hay 100 indicadores diarios
  aux <- c()
  IndTimeStamp <- c()
  cnt <- 1
  for(i in 0:10){
    for(j in 0:100){
      aux <- paste0("#date",i,"_",j)
      aux2 <- html_nodes(htmlDoc,aux) %>% html_text()
      if(is.na(aux2[1])) next
      IndTimeStamp[cnt] <- html_nodes(htmlDoc,aux) %>% html_text()
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
  
})## time

## Otros calculos ----
let1 <- substr(TF$nameInd,3,3) ## la 3ra letras
let  <- let1==":" ## tienen hora antes del indicador
let2 <- substr(TF$nameInd,6,8) ## letras de los pares
TF <- mutate(TF,parID = let2)
TF$parID[!parID %in% c("EUR","USD","GBP","CAD","JPY","AUD","CNY","CHF","NZD")] <- NA

## Limpiamos el nombre del indicador, sacamos la hora ----
nameInd_ <- c()
nameInd_[let] <- substr(TF$nameInd[let],6,999)
nameInd_[!let] <- as.character(TF$nameInd[!let])
TF <- mutate(TF,nameInd = nameInd_)

## Si es MoM: Month over Month ----
TF <- mutate(TF,MoM = if_else(regexpr("MoM",nameInd)>0,1,0))
## Si es YoY: Year over Year ----
TF <- mutate(TF,YoY = if_else(regexpr("YoY",nameInd)>0,1,0))

## El tipo de dato del indicador ----
TF <- mutate(TF,Type = "")
TF$Type[regexpr("\\$",TF$actual)>0 | regexpr("\\$",TF$forecast)>0 | regexpr("\\$",TF$previous)>0] <- "$"
TF$Type[regexpr("¥",TF$actual)>0   | regexpr("¥",TF$forecast)>0   | regexpr("¥",TF$previous)>0] <- "¥"
TF$Type[regexpr("%",TF$actual)>0   | regexpr("%",TF$forecast)>0   | regexpr("%",TF$previous)>0] <- "%"

## Cambiamos a numerico el indicador ----
TF <- mutate(TF,actual_=trimws(actual),forecast_=trimws(forecast),previous_=trimws(previous))
TF$actual   <- as.numeric(clean(TF$actual_))
TF$forecast <- as.numeric(clean(TF$forecast_))
TF$previous <- as.numeric(clean(TF$previous_))

## Seleccionamos el año
#TF <- dplyr::filter(TF,TF$Date >= ymd("2013-01-01") & TF$Date <= ymd("2013-12-31"))
TF <- dplyr::filter(TF,TF$Date >= ymd("2014-01-01") & TF$Date <= ymd("2014-12-31"))

## Guardasmos como .sav ----
pth <- "C:/Users/CAraya/Documents/CARAYA/"
write.table(TF,paste(pth,"DailyFxCalendar_2014.txt",collapse=""),sep="|", quote = FALSE)

#write_sav(TF,paste(pth,"DailyFxCalendar_2014.sav",sep=""))

