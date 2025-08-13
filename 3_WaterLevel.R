##############################
## Script to download and 
## ProcesS ANA Data 
## Shar Siddiqui 5/1/2019
##############################

## Load Libraries
library(httr)
library(xml2)
library(reshape2)
library(readxl)
library(sf)

## Import metadata
amazon_boundary <- read_sf('C:/Users/shars/Documents/ANA/cuencas2024/Cuencas.shp')
amazon_boundary <- amazon_boundary[amazon_boundary$nombrecuen != 'Plata' & amazon_boundary$nombrecuen != 'Costa Atlántica',]

meta <- read_sf('C:/Users/shars/Documents/ANA/geoft_estacao_hidrometeorologica.gpkg')
meta <- meta[meta$TipoEstacao == 'Fluviométrica',]
meta <- meta[amazon_boundary,] ## select intersecting stations

stationCodes <- meta$CodigoEstacao

## new try
for (i in 140:length(stationCodes)) {
  codEstacao = stationCodes[i]
  bodyPOST <- paste0("codEstacao=", codEstacao,"&dataInicio=01/01/2020", "&dataFim=31/12/2024",
    "&tipoDados=1",                # 1 = cotas
    "&nivelConsistencia=1"         # raw
  )
  
  url <- "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica"
  
  res <- POST(url, body = bodyPOST, content_type("application/x-www-form-urlencoded"))
  xml_txt <- content(res, as = "text", encoding = "UTF-8")
  doc <- read_xml(xml_txt)
  doc <- xml_ns_strip(doc)  # Remove namespaces for easier querying
  
  # Find all monthly records (SerieHistorica or HidroSerieHistorica)
  records <- xml_find_all(doc, ".//SerieHistorica")
  if (length(records) == 0) next
  # For each record (month), extract the date and all CotaXX values
  daily_data <- map_df(records, function(record) {
    # Extract the month start date
    month_start <- xml_text(xml_find_first(record, "./DataHora"))
    month_start <- as.Date(substr(month_start, 1, 10))  # Keep only date portion
    
    # Extract all CotaXX tags for days 1 to 31
    cotas <- map_dbl(1:31, function(day_num) {
      node <- xml_find_first(record, paste0("./Cota", str_pad(day_num, 2, pad = "0")))
      if (is.na(node)) return(NA_real_)
      val <- xml_text(node)
      if (val == "") return(NA_real_)
      as.numeric(val)})
    
    # Create a data frame of daily records for this month
    tibble(Date = month_start + 0:30,
      Cota = cotas) %>%
      filter(!is.na(Cota))  # Remove days with no data (e.g., April 31)
  })
  outName = paste0("ANA_",codEstacao,".csv")
  write.csv(daily_data,outName)
  print(i)
}



