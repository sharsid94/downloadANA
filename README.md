**downloadANA is an R package intended to download historical hydrological records from the Brazilian Agencia Nacional das Aguas into a clean, easy to analyze, dataframe format.**

To download, run the following commands:

install.packages("remotes")
remotes::install_github("sharsid94/downloadANA", ref = "master")

Once loaded, the following functions can be run:

meta <- get_ana_metadata()
meta_filtered <- select_stations(meta, c("RIO AMAZONAS", "RIO TOCANTINS"))

station_codes <- unique(meta_filtered$CodigoEstacao[140:145]) ## as an example, note: not all ANA stations will have data

df <- download_ana_flow(station_codes,start_date = "01/01/2025", end_date = "31/12/2025")

**If you get a CRAN mirror error, run:**
options(repos = c(CRAN = "https://cloud.r-project.org"))
