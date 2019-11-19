wq_dygraph <- function(data, std) {
  dy <- dygraph(data, group = "water_quality") %>%
    dyOptions(colors = c("#b2182b", "#b2182b", "#2166ac", "#2166ac"),
              axisLineWidth = 2, connectSeparatedPoints = TRUE) %>%
    dySeries("W", label = "West", drawPoints = TRUE, pointSize = 3, pointShape = "square",
             strokeWidth = 0) %>%
    dySeries("W_loess", drawPoints = FALSE) %>%
    dySeries("E", label = "East", drawPoints = TRUE, pointSize = 3, pointShape = "square",
             strokeWidth = 0) %>%
    dySeries("E_loess", drawPoints = FALSE) %>%
    dyAxis("y", label = std$dy_axis) %>%
    dyShading(from = std$min, to = std$max, axis = "y", color = "#addd8e") %>%
    dyLegend(show = "follow", width = 200) %>%
    dyRangeSelector(height = 20, strokeColor = "", dateWindow = init_window) %>%
    dyCSS("css/matta_wq.css")
  dy
}

