
# Load required packages
if (!("pacman" %in% installed.packages())) install.packages("pacman", quiet = TRUE)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("nrsmisc", quietly = TRUE)) devtools::install_github("adamdsmith/nrsmisc")
pacman::p_load(ggplot2, viridis, readr, readxl, dplyr, tidyr, lubridate, timetk, xts, dygraphs,
               htmlwidgets, manipulateWidget)

# Water quality metrics from grab samples at USGS gages on Lake Mattamuskeet NWR.

# Historic samples (1981 - 2017)
dwr <-  readxl::read_xlsx("Data/DWR_1981-2017_Surface main and qaqc.xlsx", sheet = "DWRfinal") %>%
  # Put all samples at noon...
  mutate(date = ymd_hm(paste(Date, "12:00"), tz = "Etc/GMT+5"),
         TP = TP * 1000) %>% # convert to micrograms/L
  select(date, basin = `Lake side`,
         chla = starts_with("Chl"), # micrograms/L
         TN, # mg/L
         susp_res = `Suspended Residue`, #mg/L
         TP, # mg/L
         turbidity = Turbidity) %>% # NTUs
  gather("variable", "value", -date, -basin)

# New ~ annual output format from ISB lab
lab <- readxl::read_xlsx("Data/2018 Lake Mattamuskeet Data from ISB Lab.xlsx") %>%
  select(date = CDAT, basin = LCOD, variable = ACOD, value = ACOM) %>%
  mutate(basin = ifelse(basin == "0208458892", "W", "E"),
         date = ymd_hm(paste(date, "12:00"), tz = "Etc/GMT+5"),
         variable = case_when(
           variable == "TURBIDITY"        ~ "turbidity",
           variable == "PHOSTOTP_LIQ"     ~ "TP",
           variable == "TKNN_LIQ"         ~ "TN",
           variable == "NO2&NO3_LIQ"      ~ "TN",
           variable == "RESSUS_WET"       ~ "susp_res",
           variable == "CHLOROPHYLLA_LIQ" ~ "chla",
           TRUE                           ~ NA_character_)) %>%
  filter(!is.na(variable)) %>%
  # Coerce value column to numeric (some records had values like 'X3', etc.)
  mutate(value = as.numeric(value),
         # Convert TP to micrograms/L
         value = ifelse(variable == "TP", value * 1000, value)) %>%
  # Add multiple variables to get total N
  group_by(date, basin, variable) %>%
  summarise(value = sum(value, na.rm = TRUE))

wq <- bind_rows(dwr, lab) %>%
  na.omit() %>%
  arrange(variable, basin, date)

standards <- data.frame(variable = c("chla", "TN", "TP", "susp_res", "turbidity"),
                        axis = c("Chlorophyll<i><sub>a</sub></i><br>(&mu;g/L)",
                                  "Total nitrogen<br> (mg/L)",
                                  "Total phosphorus<br>(&mu;g/L)",
                                  "Total susp. solids<br>(mg/L)",
                                  "Turbidity<br>(NTUs)"),
                        min = c(0, 0.32, 8, 0, 0),
                        max = c(40, 0.41, 20, 15, 25),
                        stringsAsFactors = FALSE)

vars <- unique(wq$variable)
wq_ts <- lapply(vars, function(v) {
  message(v)
  std <- filter(standards, variable == v)
  tmp <- filter(wq, variable == v) %>% tk_xts(tzone = "Etc/GMT+5") %>%
    xts::make.time.unique(eps = 1800)
  init_window <- as.character(c(Sys.time() - years(5), Sys.time()))
  tmp_dy <- dygraph(tmp, group = "water_quality") %>%
    dyOptions(colors = "black", axisLineWidth = 2, connectSeparatedPoints = FALSE, strokeWidth = 0) %>%
    # dySeries("value", label = "SHITFIRE!", stemPlot = TRUE) %>%
    dySeries("value", label = "Measurement", drawPoints = TRUE, pointSize = 4, pointShape = "square") %>%
    dyAxis("y", label = std$axis) %>%
    dyShading(from = std$min, to = std$max, axis = "y", color = "#addd8e") %>%
    dyLegend(show = "follow", width = 200) %>%
    dyRangeSelector(height = 20, strokeColor = "", dateWindow = init_window) %>%
    dyCSS("css/matta_wq.css")
})

all <- manipulateWidget::combineWidgets(list = wq_ts, ncol = 1)
saveWidget(all, "Mattamuskeet_water_quality_dygraph.html", title = "Mattamuskeet Water Quality")
file.rename("Mattamuskeet_water_quality_dygraph.html", "./docs/Mattamuskeet_water_quality_dygraph.html")
file.copy("./docs/Mattamuskeet_water_quality_dygraph.html", "./docs/index.html")
