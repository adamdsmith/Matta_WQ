# Load required packages
pacman::p_load(readxl, dplyr, tidyr, lubridate, ggplot2)
source("R/utils.R")
source("R/wq_boxplots.R")

# Load and consolidate historic data with new ISB lab output
# Historic samples (1981 - 2017)
old <-  readxl::read_xlsx("Data/DWR_1981-2017_Surface main and qaqc.xlsx", sheet = "DWRfinal") %>%
  # Put all samples at midday for convenience...
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

# Calculate N:P ratios by mass and molarity
wq <- bind_rows(old, new_lab) %>%
  mutate(NP_mass = TN / (TP / 1000),
         NP_molar = mgL_M(TN, 14.0067) / mgL_M(TP/1000, 30.973762)) %>%
  arrange(date, basin)

# Set up nested loop to make all the boxplots
poi_groups <- c("core", "nitrogen", "sediment")
sum_types <- c("annual", "monthly") # which kind(s) of summaries
raw <- c(TRUE, FALSE)               # show raw data?

# Demo for core water quality parameters, adapt for Nitrogen and Sediment
for (p in poi_groups) { # start parameter grouping loop
  # Get parameters of interest for this group
  poi <- case_when(
    p == "core"     ~ c("chla", "turbidity", "TP", "TN", "res_susp_total"),
    p == "nitrogen" ~ c("TN", "NH3", "NOx", "TKN", "NP_molar"),
    p == "sediment" ~ c("res_total", "res_diss_total", "res_susp_total", "res_susp_fixed", "res_susp_vol")
  )
  # Format WQ data for this parameter group
  bp_dat <- format_boxplot_data(data = wq, variables = poi)

  # Set up pdf output of all plots by parameter group
  bp_pdf_fn <- paste(c("Mattamuskeet", p, "boxplots.pdf"), collapse = "_")
  bp_pdf_path <- file.path("docs", bp_pdf_fn)
  pdf(file = bp_pdf_path, width = 8.5, height = 11, paper = "US")

  for (st in sum_types) { # start summary type loop
    for (r in raw) { # start raw data plot loop
      # Create plot with current summary and raw data options, lake basins in facets
      print(bp <- wq_boxplots(bp_dat, summary = st, raw = r, grouped = FALSE))
      # Create filename describing options
      bp_png_fn <- paste0(paste(c("Mattamuskeet", p, st, "boxplot", ifelse(r, "raw", "no_raw")), collapse = "_"),
                      ".png")
      bp_png_path <- file.path("docs", bp_png_fn)
      ggsave(bp_png_path, plot = bp, height = 9, width = 6.5)
    } # close raw data display loop
  } # close summary type loop
  dev.off() # Finish pdf creation
} # close parameter group loop
