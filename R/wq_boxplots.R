wq_boxplots <- function(data, title = NULL,
                        summary = c("annual", "monthly"),
                        raw = TRUE, grouped = TRUE, fix_y_range = NULL) {

  p_type <- match.arg(summary)

  data <- mutate(data,
                 x = if(identical(p_type, "annual")) year else month,
                 col = if(identical(p_type, "annual")) month else year,
                 group = if(!grouped) x else interaction(x, basin))

  p <- ggplot(data, aes(x, value)) +
    # THis adds standards
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = min, ymax = max), fill = "gray80") +
    theme_bw()

  if (raw) {
    p <- p + geom_point(aes(color = col, group = group),
                        position = position_jitterdodge(),
                        alpha = 0.75, size = 2) +
      guides(color = guide_colorbar(label.position = "top",
                                    title.vjust = 0.3)) +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            legend.key.width = unit(2, "cm"),
            strip.text = element_text(face = "bold"))
    if (identical(p_type, "annual"))
      p <- p + scale_color_gradientn(NULL, colors = cyclic_colors(12),
                                     breaks = 1:12, labels = month.abb)
    else
      p <- p + scale_color_viridis_c(NULL)
  }

  p +
    geom_boxplot(aes(group = group), fill = NA) +
    facet_grid(rows = vars(gg_label),
               cols = if(!grouped) vars(basin) else NULL,
               scales = "free_y", labeller = label_parsed) +
    scale_x_continuous(NULL, breaks = seq(min(data$x), max(data$x)),
                       labels = if(identical(p_type, "annual")) waiver() else month.abb[seq(min(data$x), max(data$x))]) +
    scale_y_continuous(NULL, limits = if(is.null(fix_y_range)) c(0, NA) else fix_y_range) +
    ggtitle(title)
}


format_boxplot_data <- function(data, variables, standards = poi_standards) {
  out <- select(data, date, basin, rep, !!!variables) %>%
    mutate(year = year(date),
           month = month(date)) %>%
    filter(year > 2010) %>%
    pivot_longer(cols = variables, names_to = "variable") %>%
    filter(!is.na(value)) %>%
    group_by(date, basin, year, month, variable) %>%
    summarize(value = mean(value)) %>% ungroup()
  out <- left_join(out, standards, by = "variable") %>%
    # Here we put the gg_labels in the order we want them
    right_join(tibble(variable = variables), by = "variable") %>%
    mutate(basin = factor(basin, levels = c("W", "E"), labels = c("West", "East")),
           gg_label = factor(gg_label, unique(gg_label))) %>%
    arrange(date, basin, variable)
  out
}
