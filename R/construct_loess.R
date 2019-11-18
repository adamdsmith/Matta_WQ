construct_loess <- function(data, variable, how_far_back = 5, smooth_span = 1.5) {
  date_cutoff <- max(data$date) - as.difftime(how_far_back * 365, units = "days")
  tmp <- filter(data, !is.na(!!sym(variable)),
                date >= date_cutoff) %>%
    group_by(date, basin) %>%
    summarize(!!variable := mean(!!sym(variable)))
  t_range <- as.integer(difftime(max(tmp$date), min(tmp$date), units = "days"))
  t_span <- (smooth_span * 365) / t_range
  basins <- c("E", "W")
  loess_dat <- lapply(basins, function(b) {
    form <- as.formula(paste(variable, "~ t"))
    b_dat <- filter(tmp, basin == b) %>%
      mutate(t = as.numeric(date))
    l_b <- loess(form, data = b_dat, span = t_span)

    toi <- as.numeric(seq.POSIXt(date_cutoff - as.difftime(30, units = "mins"),
                                 by = "month", length.out = 60))
    out <- data.frame(t = toi,
                      date = as_datetime(toi, tz = "Etc/GMT-5"),
                      pred = predict(l_b, newdata = data.frame(t = toi)))
    out
  })
  names(loess_dat) <- paste0(basins, "_loess")
  loess_ts <- bind_rows(loess_dat, .id = "basin") %>%
    spread(basin, pred) %>%
    select(-t)
  loess_ts
}
