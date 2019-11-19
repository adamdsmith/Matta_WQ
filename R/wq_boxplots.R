wq_annual_bp <- function(data, title) {
  ggplot(data, aes(year, value)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = min, ymax = max), fill = "gray80") +
    geom_jitter(aes(color = month, group = year), alpha = 0.75, size = 2) +
    geom_boxplot(aes(group = year), fill = NA) +
    facet_grid(gg_label ~ basin, scales = "free_y", labeller = label_parsed) +
    scale_color_gradientn(NULL, colours = cyclic_colors(12),
                          breaks = 1:12, labels = month.abb) +
    scale_x_continuous(NULL, breaks = sort(unique(bp_dat$year))) +
    scale_y_continuous(NULL, limits = c(0, NA)) +
    ggtitle(title) +
    theme_bw() +
    guides(color = guide_colorbar(label.position = "top",
                                  title.vjust = 0.3)) +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.key.width = unit(2, "cm"),
          strip.text = element_text(face = "bold"))
}

wq_monthly_bp <- function(data, title) {
    ggplot(data, aes(month, value)) +
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = min, ymax = max), fill = "gray80") +
    geom_jitter(aes(color = year, group = month), alpha = 0.75, size = 2) +
    geom_boxplot(aes(group = month), fill = NA) +
    facet_grid(gg_label ~ basin, scales = "free_y", labeller = label_parsed) +
    scale_color_viridis_c(NULL) +
    scale_x_continuous(NULL, breaks = 1:12, labels = month.abb) +
    scale_y_continuous(NULL, limits = c(0, NA)) +
    ggtitle(title) +
    theme_bw() +
    guides(color = guide_colorbar(label.position = "top",
                                  title.vjust = 0.3)) +
    theme(legend.position = "top",
          legend.direction = "horizontal",
          legend.key.width = unit(2, "cm"),
          strip.text = element_text(face = "bold"))

}

format_boxplot_data <- function(data, variables, standards) {
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
