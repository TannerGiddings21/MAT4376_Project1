library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::lag)
library(tidyverse)
library(lubridate)

earliestdate = ymd("2016-08-01")

pred = readr::read_csv("EC_Timeline.csv") |>
  select(votes_timeline, dates)

windowsize = 5
means = pred |>
  mutate(
    means_timeline = votes_timeline |>
      data.table::frollmean(
        n = windowsize,
        algo = "exact",
        hasNA = FALSE
      ),
    .keep = "unused"
  )

result = tibble(
  votes_timeline = c(232, 232),
  dates = c(
    ymd("2016-11-08"),
    ymd("2016-11-09")
  )
)

pred = pred |>
  slice_tail(n = nrow(pred) - windowsize) |>
  filter(dates >= earliestdate)
means = means |>
  slice_tail(n = nrow(pred))
fiftypctpo = 270 # 538/2 + 1
pred |>
  ggplot() + geom_segment(
    x = earliestdate - days(2),
    xend = ymd("2016-11-07") + days(2),
    y = fiftypctpo,
    yend = fiftypctpo,
    linetype = "11"
  ) + geom_ribbon(
    data = means,
    mapping = aes(
      x = dates,
      ymin = fiftypctpo,
      ymax = vapply(X = means_timeline, FUN = max, FUN.VALUE = 0, fiftypctpo)
    ),
    alpha = 0.7,
    fill = "blue"
  ) + geom_ribbon(
    data = means,
    mapping = aes(
      x = dates,
      ymax = fiftypctpo,
      ymin = vapply(X = means_timeline, FUN = min, FUN.VALUE = 0, fiftypctpo)
    ),
    alpha = 0.7,
    fill = "red"
  ) + labs(
    title = "Predicted Results of Electoral College",
    subtitle = "assuming faithful electors"
  ) + xlab("Polling Date") + ylab("Electoral votes for Hillary Clinton") + geom_segment(
    data = result,
    mapping = aes(
      x = ymd("2016-11-08"),
      xend = ymd("2016-11-08"),
      y = 232,
      yend = 270
    ),
    colour = "red",
    alpha = 0.7,
    linewidth = 2
  ) + scale_y_continuous(position = "right", breaks = c(230, 250, 270, 290, 310, 330, 350)) + scale_x_date(date_breaks = "2 weeks") + theme_classic() + theme(axis.text.x = element_text(angle = 45, vjust = -0.001))
