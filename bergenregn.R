### Henter værdata fra Meteorologisk institutt og beregner årsnedbør 2015
### for Bergen, målestasjon Florida

library(XML)
library(RCurl)

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
  write.table(tbl15, "data/regntabell.csv", sep=",", row.names=FALSE)


#Funksjon for å lage tekststrenger av data
get.json <- function(x){
  txt <- character()
  lgth <- length(x[,1])
  for (i in 1:lgth) {
    txt <- paste0(txt, "['",x[i,1], "',", x[i,2],"],")
  }
  txt <- paste0("[", txt, "]", collapse="")
  return(txt)
}

tbl <- tbl15

tbl2 <- cbind(tbl[,1], tbl[,3])
colnames(tbl2) <- c("Dato", "Nedbør")
tbl2 <- as.data.frame(tbl2)
tbl2$Dato <- as.Date(tbl2$Dato, origin = '1970-01-01')

jsondata <- get.json(tbl2)
write(jsondata, file="data/data.JSON")
