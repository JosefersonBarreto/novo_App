set.seed(1)

size <- 24 * 20

time <- seq(
  from = as.POSIXct("2021-01-01 00:00", tz = "UTC"),
  length.out = size,
  by = "hour"
)

white.noise <- rnorm(n = size)

test_df <- data.frame(time, value = cumsum(white.noise))

remarks <- data.frame(
  time = sample(time, 10),
  comment = paste("test", sample(1:100, 10))
)



library(echarts4r)
library(magrittr)
library(purrr)


test_df %>%
  e_charts(x = time) %>%
  e_line(serie = value) %>%
  e_legend_select(1) %>%
  e_datazoom(type = "slider") %>%
  e_theme("dark") %>%
  e_tooltip(trigger = "axis") %>%
  reduce2(
    .x = remarks$time, .y = remarks$comment,
    .f = function(x, y, z) e_mark_line(x, data = list(xAxis = y), title = z),
    .init = .
  )