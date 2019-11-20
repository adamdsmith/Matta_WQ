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
               "res_total", "res_total_vol", "res_total_fixed", "res_diss_total"),
  dy_axis = c("Chlorophyll<i><sub>a</sub></i><br>(&mu;g/L)",
              "Total nitrogen<br>(mg/L)",
              "Total phosphorus<br>(&mu;g/L)",
              "Total suspended solids<br>(mg/L)",
              "Turbidity<br>(NTUs)",
              "N:P ratio<br>(mass)",
              "N:P ratio<br>(molar)",
              "Total Kjeldahl nitrogen<br>(mg/L)",
              "Ammonia<br>(mg/L)",
              "Nitrites/Nitrates<br>(mg/L)",
              "Susp. solids (volatile)<br>(mg/L)",
              "Susp. solids (fixed)<br>(mg/L)",
              "Total solids<br>(mg/L)",
              "Total solids (volatile)<br>(mg/L)",
              "Total solids (fixed)<br>(mg/L)",
              "Total dissolved solids<br>(mg/L)"),
  gg_label = c("atop(Chlorophyll*{}[a],(mu*g/L))",
               "atop(Total~nitrogen,(mg/L))",
               "atop(Total~phosphorus,(mu*g/L))",
               "atop(Total~suspended~solids,(mg/L))",
               "atop(Turbidity,(NTUs))",
               "atop(N:P~ratio,(mass))",
               "atop(N:P~ratio,(molar))",
               "atop(Total~Kjeldahl~nitrogen,(mg/L))",
               "atop(Ammonia,(mg/L))",
               "atop(Nitrites/Nitrates,(mg/L))",
               "atop(Susp.~solids~(volatile),(mg/L))",
               "atop(Susp.~solids~(fixed),(mg/L))",
               "atop(Total~solids,(mg/L))",
               "atop(Total~solids~(volatile),(mg/L))",
               "atop(Total~solids~(fixed),(mg/L))",
               "atop(Total~dissolved~solids,(mg/L))"),
  min = c(0, 0.32, 8, 0, 0, 7.15, 15.95, rep(NA, 9)),
  max = c(40, 0.41, 20, 15, 25, 7.25, 16.05, rep(NA, 9)),
  stringsAsFactors = FALSE)

cyclic_colors <- function(n = 128) {
  cols <- c("#d03ea9", "#ca3dab", "#c43dad", "#be3caf", "#b73cb0", "#b13cb2",
            "#aa3cb2", "#a43db3", "#9d3db3", "#973db3", "#903db2", "#8a3eb2",
            "#833eb0", "#7d3faf", "#773fad", "#7140ab", "#6e40aa", "#6c43af",
            "#6a46b4", "#6849b9", "#664cbe", "#6450c2", "#6153c6", "#5f57ca",
            "#5c5bce", "#595fd1", "#5663d5", "#5267d7", "#4f6bda", "#4b70dc",
            "#4874de", "#4479df", "#417de0", "#3d82e1", "#3a87e1", "#378ce1",
            "#3390e1", "#3095e0", "#2d9adf", "#2a9fdd", "#27a4dc", "#25a8d9",
            "#22add7", "#20b2d4", "#1eb6d1", "#1dbbcd", "#1bbfca", "#1ac4c6",
            "#1ac8c1", "#19ccbd", "#19d0b8", "#1ad4b4", "#1ad7af", "#1bdbaa",
            "#1ddea4", "#1fe19f", "#21e49a", "#24e795", "#27e98f", "#2aeb8a",
            "#2eed85", "#32ef80", "#37f17c", "#3bf277", "#41f373", "#46f46e",
            "#4cf56a", "#52f667", "#59f664", "#5ff761", "#66f75e", "#6df65c",
            "#74f65a", "#7cf658", "#83f557", "#8bf457", "#93f457", "#9af357",
            "#a2f258", "#aaf159", "#b0ef59", "#b3eb53", "#b6e84e", "#bae449",
            "#bee044", "#c2db40", "#c6d73c", "#cad239", "#cece36", "#d3c934",
            "#d7c432", "#dbbf30", "#e0ba2f", "#e4b52e", "#e8b02e", "#ecaa2e",
            "#f0a52f", "#f4a030", "#f89b32", "#fb9633", "#fe9136", "#ff8c38",
            "#ff873b", "#ff823e", "#ff7d42", "#ff7946", "#ff744a", "#ff704e",
            "#ff6c52", "#ff6857", "#ff645b", "#ff6060", "#ff5d65", "#ff596a",
            "#ff566f", "#ff5374", "#ff5079", "#ff4e7e", "#fe4b83", "#fb4987",
            "#f7478c", "#f34590", "#ef4494", "#ea4298", "#e5419c", "#e040a0",
            "#db3fa3", "#d53ea6")
  colorRampPalette(cols)(n)
}


