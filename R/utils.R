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
