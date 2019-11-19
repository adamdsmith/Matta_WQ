# Convert from mass to molar units
mgL_M <- function(conc, mol_wt) {
  gL <- conc / 1000 # grams/L
  M <- gL / mol_wt
  return(M)
}

# Attempt to handle measurements that contain "less than" symbols
adj_lt <- function(x, na.rm = FALSE) {
  if (is.numeric(x)) return(x)
  has_lt <- grepl("<", x)
  mod_x <- as.numeric(gsub("<", "", x))
  mod_x <- mod_x * (1 - (0.5 * has_lt))
  mod_x
}

# Check for duplicate measurements on same basin in same day,
# suggestive of a data entry error
check_labs <- function(lab_dat) {
  n_reps <- lab_dat %>% count(name = "n_reps")
  check_me <- filter(n_reps, n_reps > 1)
  if (nrow(check_me) > 0) {
    warning("At least one date has multiple replicates for a given basin.\n",
            "Basins typically have a single grab sample on a given date.\n",
            "See the table above for dates inconsistent with that expectation", call. = FALSE)
  }
  return(check_me)
}

# Create standards and/or label data.frame
# Note the "fudge factor" on the N:P ratios to make them appear in the dygraphs
poi_standards <- data.frame(
  variable = c("chla", "TN", "TP", "res_susp_total", "turbidity", "NP_mass",
               "NP_molar", "TKN", "NH3", "NOx", "res_susp_vol", "res_susp_fixed",
               "res_total", "res_total_vol", "res_total_fixed"),
  dy_axis = c("Chlorophyll<i><sub>a</sub></i><br>(&mu;g/L)",
              "Total nitrogen<br>(mg/L)",
              "Total phosphorus<br>(&mu;g/L)",
              "Susp. solids - total<br>(mg/L)",
              "Turbidity<br>(NTUs)",
              "N:P ratio<br>(mass)",
              "N:P ratio<br>(molar)",
              "Total Kjeldahl nitrogen<br>(mg/L)",
              "Ammonia<br>(mg/L)",
              "Nitrites/Nitrates<br>(mg/L)",
              "Susp. solids - volatile<br>(mg/L)",
              "Susp. solids - fixed<br>(mg/L)",
              "Solids - total<br>(mg/L)",
              "Solids - total volatile<br>(mg/L)",
              "Solids - total fixed<br>(mg/L)"),
  gg_label = c("atop(Chlorophyll*{}[a],(mu*g/L))",
               "atop(Total~nitrogen,(mg/L))",
               "atop(Total~phosphorus,(mu*g/L))",
               "atop(Susp.~solids~-~total,(mg/L))",
               "atop(Turbidity,(NTUs))",
               "atop(N:P~ratio,(mass))",
               "atop(N:P~ratio,(molar))",
               "atop(Total~Kjeldahl~nitrogen,(mg/L))",
               "atop(Ammonia,(mg/L))",
               "atop(Nitrites/Nitrates,(mg/L))",
               "atop(Susp.~solids~-~volatile,(mg/L))",
               "atop(Susp.~solids~-~fixed,(mg/L))",
               "atop(Solids~-~total,(mg/L))",
               "atop(SolidS~-~total~volatile,(mg/L))",
               "atop(Solids~-~total~Fixed,(mg/L))"),
  min = c(0, 0.32, 8, 0, 0, 7.15, 15.95, rep(NA, 8)),
  max = c(40, 0.41, 20, 15, 25, 7.25, 16.05, rep(NA, 8)),
  stringsAsFactors = FALSE)
