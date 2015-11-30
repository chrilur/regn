### Henter værdata fra Meteorologisk institutt og beregner årsnedbør 2015
### for Bergen, målestasjon Florida

library(XML)
library(RCurl)
regn <- function(){
  url <- "http://www.yr.no/sted/Norge/Hordaland/Bergen/Bergen/detaljert_statistikk.html"
  tbl <- readHTMLTable(url, stringsAsFactors=FALSE)
  tbl <- tbl[2]
  
  #Observasjoner fra 1. oktober 2014 (numerisk: 16344)
  # 1. januar 2015 = numerisk: 16436
  idag <- as.numeric(Sys.Date())
  rows <- idag - 16344
  iår <- idag - 16436
  igjen <- 16800-idag
  
  df <- data.frame(matrix(unlist(tbl), nrow=rows))
  regn <- df[,6]
  regn <- gsub(" mm", "", regn)
  regn <- gsub(",", ".", regn)
  regn <- as.numeric(regn)
  regn <- regn[1:iår]
  tot <- sum(regn)
  med <- median(regn)
  snitt <- mean(regn)
  rekord <- 3195
  til.rekord <- rekord - tot
  
  dates <- c(16436:(idag-1))
  dager <- length(regn)
  regnkron <- regn[dager:1]
  regnsum <- NULL
  for (i in 1:dager) {
    regnsum <- c(regnsum, sum(regnkron[1:i]))
  }
  tbl15 <- data.frame(cbind(dates, regnkron, regnsum))
  tbl15$dates <- as.Date(tbl15$dates, origin="1970-01-01")
  
  cat("Årsnedbør:", tot, "mm\n")
  cat("Mediannedbør:", med, "mm\n")
  cat("Gjennomsnittlig nedbør:", snitt, "mm\n")
  cat("Det må regne", til.rekord/(igjen+1), "mm hver dag om rekorden skal slås i år.\n\n")
  return(tbl15)
}

#Funksjon for å lage tekststrenger av data
get.txt <- function(x){
  txt <- character()
  for (i in 1:length(x)) {
    txt <- paste0(txt, x[i], ", ")
  }
  return(txt)
}

tbl <- regn()

regndata <- t(tbl[,3])
dato <- t(tbl[,1])

#Lage jsonfil i rett format av dataene
spldato <- tbl[,1]
spldato <- as.character(spldato)
splår <- substring(spldato, 1,4)
splmnd <- substring(spldato, 6,7)
spldag <- substring(spldato, 9,10)
jsondato <- paste0(splår, ",", splmnd, ",", spldag)
jsondato <- paste0("[Date.UTC(", jsondato,"),")
jsondato <- paste0(jsondato, tbl[,3],"],")
jsondato <- paste(jsondato, collapse="")
jsondato <- paste0("([", jsondato, "]);")
write.table(jsondato, 'data/jsondato.json', sep="", row.names=FALSE, col.names=FALSE)

#Klistre sammen javascript-fil som rommer regndata
jsfil <- paste(
        'var regn = { name: ',"'",'regn 2015', "',",' data: [',get.txt(regndata),']};',
        'var dato = { name: ',"'",'Dato', "',",' data: [',get.txt(dato),']};')
write.table(jsfil, "data/regndata.js")