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
