install.packages(c("devtools", "usethis", "roxygen2"))
library(devtools)
library(usethis)

#' Get ANA station metadata (cached locally)
#'
#' @param path optional custom file path
#' @return sf object
#' @export
get_ana_metadata <- function() {
  url <- "https://raw.githubusercontent.com/sharsid94/downloadANA/main/data-raw/geoft_estacao_hidrometeorologica.gpkg"
  path <- file.path(tempdir(), "ana_meta.gpkg")
  if (!file.exists(path)) {
    httr::GET(url, httr::write_disk(path, overwrite = TRUE))}
  meta <- sf::st_read(path, quiet = TRUE)
  return(meta)
}

#' Select ANA stations by basin and spatial intersection
#'
#' @param meta sf object of station metadata
#' @param boundary sf object of basin boundaries
#' @param basins character vector of basin names (e.g., c("RIO AMAZONAS"))
#'
#' @return sf object of filtered stations
#' @export
#'
select_stations <- function(meta, basins) {
  # Filter stations
  meta <- meta[meta$TipoEstacao == "Fluviométrica", ]
  meta <- meta[meta$Bacia %in% basins, ]
  return(meta)
}

#' Reformat monthly ANA data to readable format
#'
#' @param month_xml
#' @param station
#' @param date_raw
#'

parse_ana_month <- function(month_xml, station, date_raw) {

  vals_raw <- month_xml[16:46]
  vals_raw <- vals_raw[!sapply(vals_raw, is.null)]

  if (length(vals_raw) == 0) return(NULL)

  vals <- unlist(vals_raw, use.names = TRUE)

  if (is.null(names(vals)) || length(vals) == 0) return(NULL)

  day <- suppressWarnings(as.numeric(gsub("Vazao", "", names(vals))))

  flow_char <- as.character(vals)
  flow_char[flow_char == ""] <- NA
  flow <- suppressWarnings(as.numeric(flow_char))

  ok <- !is.na(day) & !is.na(flow)

  day <- day[ok]
  flow <- flow[ok]

  if (length(day) == 0) return(NULL)

  data.frame(
    station = station,
    day = day,
    flow = flow,
    date_raw = date_raw,
    stringsAsFactors = FALSE
  )
}

#' Download ANA flow data for stations
#'
#' @param station_codes vector of station IDs
#' @param start_date character "dd/mm/yyyy"
#' @param end_date character "dd/mm/yyyy"
#' @param out_dir directory to save CSV files
#'
#' @return NULL (writes CSV files)
#' @export
download_ana_flow <- function(station_codes, start_date, end_date) {

  library(httr)
  library(xml2)

  base_url <- "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica"

  all_results <- list()

  for (i in seq_along(station_codes)) {
    codEstacao <- gsub("X", "", station_codes[i])
    bodyPOST <- paste0(
      "codEstacao=", codEstacao,
      "&dataInicio=", start_date,
      "&dataFim=", end_date,
      "&tipoDados=3&nivelConsistencia=")

    res <- POST(base_url, body = bodyPOST,
      content_type("application/x-www-form-urlencoded") )

    xml.doc <- content(res)
    xml.list <- xml2::as_list(xml.doc)

    if (is.null(xml.list$DataTable$diffgram)) next

    xml.data <- xml.list$DataTable$diffgram$DocumentElement

    if (length(xml.data) == 0) next

    station_chunks <- list()

    for (j in seq_along(xml.data)) {

      month <- xml.data[[j]]
      month_date <- as.character(month$DataHora)

      vals_raw <- month[16:46]
      vals_raw <- vals_raw[!sapply(vals_raw, is.null)]

      if (length(vals_raw) == 0) next

      vals <- unlist(vals_raw, use.names = TRUE)

      if (is.null(names(vals)) || length(vals) == 0) next

      day <- suppressWarnings(as.numeric(gsub("Vazao", "", names(vals))))
      flow <- suppressWarnings(as.numeric(as.character(vals)))

      ok <- !is.na(day) & !is.na(flow)

      day <- day[ok]
      flow <- flow[ok]

      if (length(day) == 0) next

      tmp <- data.frame(station = codEstacao,
        day = day,flow = flow,
        month_date = month_date,
        stringsAsFactors = FALSE)

      station_chunks[[length(station_chunks) + 1]] <- tmp}

    if (length(station_chunks) == 0) next

    station_df <- do.call(rbind, station_chunks)

    station_df$date <- as.Date(
      paste(
        station_df$day,
        substr(station_df$month_date, 6, 7),
        substr(station_df$month_date, 1, 4),
        sep = "-" ),
      format = "%d-%m-%Y")

    station_df <- station_df[, c("station", "date", "flow")]
    all_results[[length(all_results) + 1]] <- station_df
    message("Downloaded: ", codEstacao)}

  result <- do.call(rbind, all_results)

  if (is.null(result)) return(data.frame())

  result <- result[order(result$station, result$date), ]
  return(result)
}

meta <- get_ana_metadata()
meta_filtered <- select_stations(meta, c("RIO AMAZONAS", "RIO TOCANTINS"))
station_codes <- unique(meta_filtered$CodigoEstacao[140:145])
df <- download_ana_flow(station_codes,start_date = "01/01/2025", end_date = "31/12/2025")


devtools::load_all()
