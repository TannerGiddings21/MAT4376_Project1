library(conflicted)
conflicts_prefer(dplyr::filter, dplyr::lag)
library(tidyverse)
library(lubridate)
library(extrafont)
  font_import(paths = ".", prompt = FALSE)

earliestdate = ymd("2016-08-01")

pred = read_csv("EC_Timeline.csv") |>
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
    ymd("2016-11-07"),
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
  ggplot() + geom_ribbon(
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
    fill = "red",
    alpha = 0.7
  ) + labs(
    title = "Predicted Electoral Votes for Hillary Clinton",
    subtitle = "(with faithful electors)"
    ) + xlab("") + ylab("") + geom_ribbon(
    data = result,
    mapping = aes(
      x = dates,
      ymin = 232,
      ymax = 270
    ),
    fill = "red",
    alpha = 0.7
  ) + scale_y_continuous(
    position = "right",
    breaks = c(230, 270, 310, 350),
    limits = c(230, 350)
  ) + annotate(
    "text",
    x = ymd("2016-08-22"),
    y = 325,
    label = "?"
  )+ scale_x_date(
    limits = c(
      ymd("2016-08-13"),
      ymd("2016-11-09")
    ),
    date_breaks = "2 weeks",
    date_labels = "%b %d"
  ) + annotate(
    "text",
    x = ymd("2016-10-28"),
    y = 350,
    label = "Comey:\nemails"
  ) + annotate(
    "text",
    x = ymd("2016-09-22"),
    y = 243,
    label = "1st\ndebate"
  ) + annotate(
    "text",
    x = ymd("2016-10-07"),
    y = 325,
    label = "Access\nHollywood\ntapes"
  ) + annotate(
    "text",
    x = ymd("2016-11-03"),
    y = 244,
    label = "result",
    colour = "red"
  ) + theme_classic() + theme(
    text = element_text(
      family = "Geneva",
      size = 12
    ),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(c(0.75,0.6,0.75,0.6), "cm")
  )
