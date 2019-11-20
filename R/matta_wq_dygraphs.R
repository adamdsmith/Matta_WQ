# Load required packages
if (!("pacman" %in% installed.packages())) install.packages("pacman", quiet = TRUE)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
if (!requireNamespace("nrsmisc", quietly = TRUE)) devtools::install_github("adamdsmith/nrsmisc")
pacman::p_load(ggplot2, viridis, readr, readxl, dplyr, tidyr, lubridate, timetk, xts, dygraphs,
               htmlwidgets, manipulateWidget)

source("R/utils.R")
source("R/construct_loess.R")
source("R/wq_dygraph.R")

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
  # Calculate total dissolved solids
  mutate(res_diss_total = res_total - res_susp_total) %>%
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
         TP = PHOSTOTP_LIQ, turbidity = TURBIDITY, rep) %>%
  # Calculate total dissolved solids
  mutate(res_diss_total = res_total - res_susp_total)

# Attempt to flag potentially problematic dates
# An "empty" data frame result is good
check_labs(new_lab)

# Calculate N:P ratios by mass and molarity
wq <- bind_rows(old, new_lab) %>%
  mutate(NP_mass = TN / (TP / 1000),
         NP_molar = mgL_M(TN, 14.0067) / mgL_M(TP/1000, 30.973762)) %>%
  arrange(date, basin)


# Set initial window of all dygraphs
init_window <- c(max(wq$date) - years(5), max(wq$date) + weeks(1))

###################################################
## CORE WATER QUALITY PARAMETERS OF INTEREST (POI)
###################################################

# Declare parameters of interest
core_poi <- c("chla", "turbidity", "TP", "TN", "res_susp_total")

# Interactive dygraph
core_poi_dy <- lapply(core_poi, function(v) {
  std <- filter(poi_standards, variable == v)
  tmp <- select(wq, date, basin, rep, !!v) %>% ungroup()

  # Loess calculation on recent data
  loess_ts <- construct_loess(data = tmp, variable = v,
                              how_far_back = 5, smooth_span = 1.5)

  # Resume building time series of full data
  tmp <- tmp %>%
    spread(basin, !!v) %>% select(date, E:W) %>%
    bind_rows(loess_ts) %>%
    tk_xts(tzone = "Etc/GMT-5", select = c(W, W_loess, E, E_loess), date_var = date) %>%
    xts::make.time.unique(eps = 1800) # Arbitrarily space replicates 30 mins apart to visualize
  tmp_dy <- wq_dygraph(tmp, std)
  })
core_poi_dy <- manipulateWidget::combineWidgets(list = core_poi_dy, ncol = 1)
saveWidget(core_poi_dy, "Mattamuskeet_water_quality_dygraph.html", title = "Mattamuskeet Water Quality")
file.rename("Mattamuskeet_water_quality_dygraph.html", "./docs/Mattamuskeet_water_quality_dygraph.html")

# Boxplots
bp_dat <- format_boxplot_data(wq, core_poi, poi_standards)
pdf(file = "./docs/Mattamuskeet_water_quality_boxplots.pdf", height = 11, width = 8.5, paper = "US")
(core_poi_annual_bp <- wq_annual_bp(bp_dat, title = "Mattamuskeet NWR: Core water quality parameters (annually since 2012)"))
(core_poi_monthly_bp <- wq_monthly_bp(bp_dat, title = "Mattamuskeet NWR: Core water quality parameters (monthly since 2012)"))
dev.off()

###################################################
## NITROGEN SPECIES OF INTEREST
###################################################

# Declare parameters of interest
N_poi <- c("TN", "NH3", "NOx", "TKN", "NP_molar")

# Interactive dygraph
N_poi_dy <- lapply(N_poi, function(v) {
  std <- filter(poi_standards, variable == v)
  tmp <- select(wq, date, basin, rep, !!v) %>% ungroup()

  # Loess calculation on recent data
  loess_ts <- construct_loess(data = tmp, variable = v,
                              how_far_back = 5, smooth_span = 1.5)

  # Resume building time series of full data
  tmp <- tmp %>%
    spread(basin, !!v) %>% select(date, E:W) %>%
    bind_rows(loess_ts) %>%
    tk_xts(tzone = "Etc/GMT-5", select = c(W, W_loess, E, E_loess), date_var = date) %>%
    xts::make.time.unique(eps = 1800) # Arbitrarily space replicates 30 mins apart to visualize
  tmp_dy <- wq_dygraph(tmp, std)
})

N_poi_dy <- manipulateWidget::combineWidgets(list = N_poi_dy, ncol = 1)
saveWidget(N_poi_dy, "Mattamuskeet_N_species_dygraph.html", title = "Mattamuskeet Water Quality - Nitrogen")
file.rename("Mattamuskeet_N_species_dygraph.html", "./docs/Mattamuskeet_N_species_dygraph.html")

# Boxplots
bp_dat <- format_boxplot_data(wq, N_poi, poi_standards)
pdf(file = "./docs/Mattamuskeet_N_species_boxplots.pdf", height = 11, width = 8.5, paper = "US")
(N_poi_annual_bp <- wq_annual_bp(bp_dat, title = "Mattamuskeet NWR: Nitrogen water quality parameters (annually since 2012)"))
(N_poi_monthly_bp <- wq_monthly_bp(bp_dat, title = "Mattamuskeet NWR: Nitrogen water quality parameters (monthly since 2012)"))
dev.off()


###################################################
## SEDIMENT SPECIES OF INTEREST
###################################################

# Declare parameters of interest
sed_poi <-  c("res_total", "res_diss_total", "res_susp_total", "res_susp_fixed", "res_susp_vol")

# Fix y-axis to facilitate comparison across species
y_range <- range(c(0, wq$res_total), na.rm = TRUE)

# Interactive dygraph
sed_poi_dy <- lapply(sed_poi, function(v) {
  std <- filter(poi_standards, variable == v)
  tmp <- select(wq, date, basin, rep, !!v) %>% ungroup()

  # Loess calculation on recent data
  loess_ts <- construct_loess(data = tmp, variable = v,
                              how_far_back = 5, smooth_span = 1.5)

  # Resume building time series of full data
  tmp <- tmp %>%
    spread(basin, !!v) %>% select(date, E:W) %>%
    bind_rows(loess_ts) %>%
    tk_xts(tzone = "Etc/GMT-5", select = c(W, W_loess, E, E_loess), date_var = date) %>%
    xts::make.time.unique(eps = 1800) # Arbitrarily space replicates 30 mins apart to visualize
  tmp_dy <- wq_dygraph(tmp, std, fix_y_range = y_range)
})

sed_poi_dy <- manipulateWidget::combineWidgets(list = sed_poi_dy, ncol = 1)
saveWidget(sed_poi_dy, "Mattamuskeet_sediment_species_dygraph.html", title = "Mattamuskeet Water Quality - Sediment")
file.rename("Mattamuskeet_sediment_species_dygraph.html", "./docs/Mattamuskeet_sediment_species_dygraph.html")

# Boxplots
bp_dat <- format_boxplot_data(wq, sed_poi, poi_standards)
pdf(file = "./docs/Mattamuskeet_sediment_species_boxplots.pdf", height = 11, width = 8.5, paper = "US")
(sed_poi_annual_bp <- wq_annual_bp(bp_dat, title = "Mattamuskeet NWR: Sediment water quality parameters (annually since 2012)"))
(sed_poi_monthly_bp <- wq_monthly_bp(bp_dat, title = "Mattamuskeet NWR: Sediment water quality parameters (monthly since 2012)"))
dev.off()
