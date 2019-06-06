# Load required packages
if (!("pacman" %in% installed.packages())) install.packages("pacman", quiet = TRUE)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("nrsmisc", quietly = TRUE)) devtools::install_github("adamdsmith/nrsmisc")
pacman::p_load(ggplot2, viridis, readr, readxl, dplyr, tidyr, lubridate, timetk, xts, dygraphs,
               htmlwidgets, manipulateWidget)
adj_lt <- function(x, na.rm = FALSE) {
  if (is.numeric(x)) return(x)
  has_lt <- grepl("<", x)
  mod_x <- as.numeric(gsub("<", "", x))
  mod_x <- mod_x * (1 - (0.5 * has_lt))
  mod_x
}
# Water quality metrics from grab samples at USGS gages on Lake Mattamuskeet NWR.

# Historic samples (1981 - 2017)
old <-  readxl::read_xlsx("Data/DWR_1981-2017_Surface main and qaqc.xlsx", sheet = "DWRfinal") %>%
  # Put all samples at noon...
  mutate(date = ymd_hm(paste(Date, "12:00"), tz = "Etc/GMT-5"),
         TP = TP * 1000) %>% # convert to micrograms/L
  select(date, basin = `Lake side`, chla = starts_with("Chl"),
         TKN, NH3, NOx, TN, res_susp_total = `Suspended Residue`,
         res_susp_vol = `Suspended Residue_Volatile`,
         res_susp_fixed = `Suspended Residue_Fixed`,
         res_total = `Total Residue`,  res_total_vol = `Total Residue_Volatile`,
         res_total_fixed = `Total Residue_Fixed`, TP, turbidity = Turbidity) %>%
  # Fix "less than, <" values
  mutate_at(vars(chla:turbidity), adj_lt) %>%
  # Denote replicate measurements in a basin on a given date
  group_by(date, basin) %>% mutate(rep = 1:n())

# New ~ annual output format from ISB lab
# Will need to generalize this when multiple spreadsheets become available...
new_lab <- readxl::read_xlsx("Data/2018 Lake Mattamuskeet Data from ISB Lab.xlsx") %>%
  select(date = CDAT, basin = LCOD, variable = ACOD, value = ACOM) %>%
  mutate(basin = ifelse(basin == "0208458892", "W", "E"),
         date = ymd_hm(paste(date, "12:00"), tz = "Etc/GMT-5"),
         variable = ifelse(grepl("CHLORO", variable), "chla", variable),
         # Remove "less than signs" (return half of value) and coerce to numeric
         # Some records had values like 'X3', etc.
         value = adj_lt(value),
         # Convert TP to micrograms/L
         value = ifelse(variable == "PHOSTOTP_LIQ", value * 1000, value)) %>%
  group_by(date, basin, variable) %>%
  mutate(rep = 1:n()) %>%
  spread(variable, value) %>%
  mutate(TN = TKNN_LIQ + `NO2&NO3_LIQ`) %>%
  select(date, basin, chla, TKN = TKNN_LIQ, NH3 = NH3N_LIQ, NOx = `NO2&NO3_LIQ`,
         TN, res_susp_total = RESSUS_WET, res_susp_vol = RESSUSVOL_WET,
         res_susp_fixed = RESSUSFIX_WET, res_total = RESTOT_WET,
         res_total_vol = RESTOTVOL_WET, res_total_fixed = RESTOTFIX_WET,
         TP = PHOSTOTP_LIQ, turbidity = TURBIDITY, rep)

# Attempt to flag potentially problematic dates
source("R/flag_observations.R")
check_labs(new_lab)

wq <- bind_rows(old, new_lab) %>%
  arrange(date, basin)

# Create standards and/or label data.frame
poi_standards <- data.frame(
  variable = c("chla", "TN", "TP", "res_susp_total", "turbidity",
               "TKN", "NH3", "NOx", "res_susp_vol", "res_susp_fixed",
               "res_total", "res_total_vol", "res_total_fixed"),
  axis = c("Chlorophyll<i><sub>a</sub></i><br>(&mu;g/L)",
           "Total nitrogen<br>(mg/L)",
           "Total phosphorus<br>(&mu;g/L)",
           "Susp. solids - total<br>(mg/L)",
           "Turbidity<br>(NTUs)",
           "Total Kjeldahl nitrogen<br>(mg/L)",
           "Ammonia<br>(mg/L)",
           "Nitrites/Nitrates<br>(mg/L)",
           "Susp. solids - volatile<br>(mg/L)",
           "Susp. solids - fixed<br>(mg/L)",
           "Solids - total<br>(mg/L)",
           "Solids - total volatile<br>(mg/L)",
           "Solids - total fixed<br>(mg/L)"),
  min = c(0, 0.32, 8, 0, 0, rep(NA, 8)),
  max = c(40, 0.41, 20, 15, 25, rep(NA, 8)),
  stringsAsFactors = FALSE)

###################################################
## CORE WATER QUALITY PARAMETERS OF INTEREST (POI)
###################################################

core_poi <- c("turbidity", "TP", "TN", "res_susp_total", "chla")

wq_core_dy <- lapply(core_poi, function(v) {
  std <- filter(poi_standards, variable == v)
  tmp <- select(wq, date, basin, rep, !!v) %>% ungroup() %>%
    spread(basin, !!v) %>% select(date, E:W) %>%
    tk_xts(tzone = "Etc/GMT-5", select = c(W, E), date_var = date) %>%
    xts::make.time.unique(eps = 1800) # Arbitrarily space replicates 30 mins apart to visualize
  init_window <- as.character(c(Sys.time() - years(5), Sys.time()))
  tmp_dy <- dygraph(tmp, group = "water_quality") %>%
    dyOptions(colors = c("#b2182b", "#2166ac"), axisLineWidth = 2, connectSeparatedPoints = FALSE,
              drawPoints = TRUE, pointSize = 3, pointShape = "square", strokeWidth = 0) %>%
    dySeries("W", label = "West") %>%
    dySeries("E", label = "East") %>%
    dyAxis("y", label = std$axis) %>%
    dyShading(from = std$min, to = std$max, axis = "y", color = "#addd8e") %>%
    dyLegend(show = "follow", width = 200) %>%
    dyRangeSelector(height = 20, strokeColor = "", dateWindow = init_window) %>%
    dyCSS("css/matta_wq.css")
})

core_poi_dy <- manipulateWidget::combineWidgets(list = wq_core_dy, ncol = 1)
saveWidget(core_poi_dy, "Mattamuskeet_water_quality_dygraph.html", title = "Mattamuskeet Water Quality")
file.rename("Mattamuskeet_water_quality_dygraph.html", "./docs/Mattamuskeet_water_quality_dygraph.html")
file.remove("./docs/index.html")
file.copy("./docs/Mattamuskeet_water_quality_dygraph.html", "./docs/index.html")

###################################################
## NITROGEN SPECIES OF INTEREST
###################################################

N_poi <- c("NH3", "NOx", "TKN", "TN")

N_poi_dy <- lapply(N_poi, function(v) {
  std <- filter(poi_standards, variable == v)
  tmp <- select(wq, date, basin, rep, !!v) %>% ungroup() %>%
    spread(basin, !!v) %>% select(date, E:W) %>%
    tk_xts(tzone = "Etc/GMT-5", select = c(W, E), date_var = date) %>%
    xts::make.time.unique(eps = 1800) # Arbitrarily space replicates 30 mins apart to visualize
  init_window <- as.character(c(as.POSIXct("2012-05-01 00:00:00"), Sys.time()))
  tmp_dy <- dygraph(tmp, group = "water_quality") %>%
    dyOptions(colors = c("#b2182b", "#2166ac"), axisLineWidth = 2, connectSeparatedPoints = FALSE,
              drawPoints = TRUE, pointSize = 3, pointShape = "square", strokeWidth = 0) %>%
    dySeries("W", label = "West") %>%
    dySeries("E", label = "East") %>%
    dyAxis("y", label = std$axis) %>%
    dyShading(from = std$min, to = std$max, axis = "y", color = "#addd8e") %>%
    dyLegend(show = "follow", width = 200) %>%
    dyRangeSelector(height = 20, strokeColor = "", dateWindow = init_window) %>%
    dyCSS("css/matta_wq.css")
})

N_poi_dy <- manipulateWidget::combineWidgets(list = N_poi_dy, ncol = 1)
saveWidget(N_poi_dy, "Mattamuskeet_N_species_dygraph.html", title = "Mattamuskeet Water Quality - Nitrogen")
file.rename("Mattamuskeet_N_species_dygraph.html", "./docs/Mattamuskeet_N_species_dygraph.html")

###################################################
## SEDIMENT SPECIES OF INTEREST
###################################################

sed_poi <-  c("res_susp_total", "res_susp_fixed", "res_susp_vol",
              "res_total", "res_total_fixed", "res_total_vol")

sed_poi_dy <- lapply(sed_poi, function(v) {
  std <- filter(poi_standards, variable == v)
  tmp <- select(wq, date, basin, rep, !!v) %>% ungroup() %>%
    spread(basin, !!v) %>% select(date, E:W) %>%
    tk_xts(tzone = "Etc/GMT-5", select = c(W, E), date_var = date) %>%
    xts::make.time.unique(eps = 1800) # Arbitrarily space replicates 30 mins apart to visualize
  init_window <- as.character(c(as.POSIXct("2012-05-01 00:00:00"), Sys.time()))
  tmp_dy <- dygraph(tmp, group = "water_quality") %>%
    dyOptions(colors = c("#b2182b", "#2166ac"), axisLineWidth = 2, connectSeparatedPoints = FALSE,
              drawPoints = TRUE, pointSize = 3, pointShape = "square", strokeWidth = 0) %>%
    dySeries("W", label = "West") %>%
    dySeries("E", label = "East") %>%
    dyAxis("y", label = std$axis) %>%
    dyShading(from = std$min, to = std$max, axis = "y", color = "#addd8e") %>%
    dyLegend(show = "follow", width = 200) %>%
    dyRangeSelector(height = 20, strokeColor = "", dateWindow = init_window) %>%
    dyCSS("css/matta_wq.css")
})

sed_poi_dy <- manipulateWidget::combineWidgets(list = sed_poi_dy, ncol = 1)
saveWidget(sed_poi_dy, "Mattamuskeet_sediment_species_dygraph.html", title = "Mattamuskeet Water Quality - Sediment")
file.rename("Mattamuskeet_sediment_species_dygraph.html", "./docs/Mattamuskeet_sediment_species_dygraph.html")
