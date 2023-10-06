library(conflicted)
library(tidyverse)
  conflicts_prefer(dplyr::filter, dplyr::lag, .quiet = TRUE)
library(R6)
library(lubridate)
library(extrafont)
  font_import(paths = ".", prompt = FALSE)

polls = "polls_us_election_2016.csv" |>
  read_delim(show_col_types = FALSE)

allstates = polls$state |>
  unique() |>
  sort() # alphabetically sorted
polls = polls |>
  mutate( # create time interval (easier to work with), calculate middate
    interval = startdate %--% enddate,
    middate = ymd(startdate + (enddate - startdate)/2), # mean
    .keep = "unused",
    .after = "state"
  ) |>
  mutate(
    grade = grade |>
      factor( # grades as (ordered) factors
        levels = c(
          NA,
          "F",
          "D-",
          "D",
          "D+",
          "C-",
          "C",
          "C+",
          "B-",
          "B",
          "B+",
          "A-",
          "A",
          "A+"
        ),
        exclude = NA,
        ordered = TRUE # e.g., A+ is better than A
      ),
    .keep = "unused",
    .before = "samplesize"
  )

# manually entered final results for  each state/c. district
finalresults = tibble(
  state = allstates,
  clinton = c(34.36, 36.55, 45.13, 33.65, 61.73, 48.16, 54.57, 53.09, 90.86, 47.82, 45.64, 62.22, 27.49, 55.83, 37.77, 41.74, 36.05, 32.68, 38.45, 47.83, 53.96, 40.98, 60.33, 60.01, 47.27, 46.44, 40.11, 38.14, 35.75, 33.70, 35.46, 44.92, 19.73, 47.92, 46.83, 55.45, 48.26, 59.01, 46.17, 27.23, 43.56, 28.93, 50.07, 47.46, 54.41, 40.67, 31.74, 34.72, 43.24, 48.18, 27.46, 56.68, 49.73, 52.54, 26.43, 46.45, 21.88),
  trump = c(62.08, 51.28, 48.67, 60.57, 31.62, 43.25, 40.93, 41.71, 4.09, 49.02, 50.77, 30.04, 59.26, 38.76, 56.94, 51.15, 56.65, 62.52, 58.09, 44.87, 39.15, 51.26, 33.91, 32.81, 47.50, 44.92, 57.94, 56.77, 56.17, 58.75, 56.18, 47.16, 73.92, 45.50, 46.46, 41.35, 40.04, 36.52, 49.83, 62.96, 51.69, 65.32, 39.09, 48.18, 38.90, 54.94, 61.53, 60.72, 52.23, 46.09, 45.54, 30.27, 44.41, 36.83, 68.50, 47.22, 68.17),
  johnson = c(2.09, 5.88, 4.13, 2.65, 3.37, 5.18, 2.96, 3.33, 1.58, 2.20, 3.05, 3.72, 4.10, 3.79, 4.90, 3.78, 4.68, 2.79, 1.87, 5.09, 4.71, 5.52, 2.86, 4.15, 3.59, 3.84, 1.19, 3.47, 5.64, 4.61, 4.97, 4.54, 4.32, 3.32, 4.14, 1.87, 9.34, 2.29, 2.74, 6.22, 3.17, 5.75, 4.71, 2.38, 3.18, 2.34, 5.63, 2.81, 3.16, 3.28, 3.50, 3.20, 2.97, 4.85, 3.22, 3.58, 5.19),
  mcmullin = c(NA, NA, 0.68, 1.17, 0.28, 1.04, 0.13, 0.16, NA, NA, 0.32, NA, 6.73, 0.21, NA, 0.79, 0.55, 1.18, 0.42, 0.25, 0.20, 0.31, 0.35, NA, 0.17, 1.80, NA, 0.25, 0.46, NA, NA, NA, NA, NA, 0.14, NA, 0.73, 0.13, NA, NA, 0.23, NA, NA, 0.10, 0.17, 1.00, NA, 0.48, 0.47, 0.54, 21.54, 0.20, 1.36, NA, 0.15, 0.40, NA)
)

# helps filter for candidate name in results
# names are in lowercase to match how they are identified in the polls tibble
candidatenames = c("clinton", "trump", "johnson", "mcmullin")

candidate = R6Class(
  classname = "candidate",
  public = list(
    name = "character",
    opponents = "character",
    colour = "character",

    initialize = function(name, colour){
      self$name = name
      self$colour = colour
      # identifies the list of other candidates
      self$opponents = candidatenames[candidatenames != self$name]
    } # end of initialize
  ) # end of public list
) # end of candidate R6Class

clinton = candidate$new(name = candidatenames[1], colour = "blue")
trump = candidate$new(name = candidatenames[2], colour = "red")
# Johnson and Mcmullin's colours were changed from their official colours for aesthetics reasons
johnson = candidate$new(name = candidatenames[3], colour = "orange")
mcmullin = candidate$new(name = candidatenames[4], colour = "purple")

candidates = list(clinton, trump, johnson, mcmullin)
rm(candidatenames) # was only needed for candidate construction

# the earliest date for which polls should be graphed
# graph will start on a the first middate >= earliestdate for which a poll was conducted in the given state/c. district
earliestdate = ymd("2016-07-15")
votingday = ymd("2016-11-08")
# used just for technical graphing purposes
dayafter = votingday + days(1)

# states that weren't two-horse races between Clinton and Trump
# Mcmullin was only a contender in Utah
oddstates = c("Utah", "New Mexico", "Idaho")

# states that were well polled. We use a higher grade standard of poll in these states
oftpolled = c("U.S.", "Arizona", "Iowa", "Kansas", "Ohio", "Wisconsin")

# thestate: capitalized string. The state/congressional district for which the polls are to be plotted
# adjstring: capitalized string. Indicates whether the polls to be plotted are the raw numbers, FiveThirtyEight's adjusted polls, or polls adjusted in some other manner
# mingrade: lowercase string. To be matched with a label for one of the pollster grades (see the labels of the factor levels);
  # every poll of grade <NA> or a grade lower than minlevel is discarded for plotting purposes
# minmiddate: lubridate. The earliest middate value for which a poll's results will be displayed in the plot
# returns a filled ggplot
stateplot = function(thestate, adjstring, mingrade, minmiddate, givenpolls) {

  plot = ggplot()
  toplot = list(clinton, trump)

  if (thestate %in% oddstates) {
    if (thestate == "Utah") {
      toplot = candidates # plot all four candidates
      mingrade = "C+"
    }
    else toplot = append(toplot, johnson) # plot Johnson in addition to Clinton and Trump
  }
  if (thestate %in% c("Alaska", "District of Columbia", "Hawaii", "Idaho", "New Mexico", "North Dakota", "Rhode Island", "Vermont", "Wyoming")) mingrade = "B"

  statepolls = givenpolls |>
    filter(state == thestate, middate >= minmiddate, grade >= mingrade)

  stateplotstr = thestate |>
    paste(adjstring, sep = " - ") |>
    paste("Polls", "(%)", sep = " ")

  if (adjstring == "Raw") adjstring = "rawpoll"
  else if (adjstring == "Adjusted") adjstring = "adjpoll"
  # might not have the time to implement these
  else if (adjstring == "Original Adjusted") adjstring = "origadjpoll"

  candpolls = statepolls

  for (cand in toplot) {

    candpolls = statepolls |>
      select(
        state:population,
        values_toplot = matches( # meant to match exactly one column
            adjstring |>
              paste(cand$name, sep = "_") # e.g. adjpoll_johnson
          )
      )

    plot = plot + geom_point(
      data = candpolls,
      mapping = aes(x = middate, y = values_toplot),
      colour = cand$colour,
      alpha = 0.8,
      na.rm = TRUE
    )
    plot = plot + geom_smooth(
      data = candpolls,
      mapping = aes(x = middate, y = values_toplot),
      method = "loess",
      formula = y ~ x,
      fullrange = FALSE,
      colour = cand$colour,
      fill = cand$colour,
      alpha = 0.4,
      level = 1 - 0.025/length(toplot) # joint confidence 95% per Bonferroni
    )

  } # end of for

  plot = plot + geom_segment( # indicates the vote share actually received
    data = candpolls,
    mapping = aes(
      x = votingday - days(1),
      y = finalresults |>
        filter(state == thestate) |>
        select(matches(clinton$name)) |>
        as.numeric(),
      xend = dayafter,
      yend = finalresults |>
        filter(state == thestate) |>
        select(matches(clinton$name)) |>
        as.numeric()
    ),
    colour = clinton$colour,
    linewidth = 2.5,
    alpha = 0.4
  )

  plot = plot + geom_segment( # indicates the vote share actually received
    data = candpolls,
    mapping = aes(
      x = votingday - days(1),
      y = finalresults |>
        filter(state == thestate) |>
        select(matches(trump$name)) |>
        as.numeric(),
      xend = dayafter,
      yend = finalresults |>
        filter(state == thestate) |>
        select(matches(trump$name)) |>
        as.numeric()
    ),
    colour = trump$colour,
    linewidth = 2.5,
    alpha = 0.4
  )

  if (has_element(toplot, johnson)) {
    plot = plot + geom_segment( # indicates the vote share actually received
      data = candpolls,
      mapping = aes(
        x = votingday - days(1),
        y = finalresults |>
          filter(state == thestate) |>
          select(matches(johnson$name)) |>
          as.numeric(),
        xend = dayafter,
        yend = finalresults |>
          filter(state == thestate) |>
          select(matches(johnson$name)) |>
          as.numeric()
      ),
      colour = johnson$colour,
      linewidth = 2.5,
      alpha = 0.4
    )
  }

  if (has_element(toplot, mcmullin)) {
    plot = plot + geom_segment( # indicates the vote share actually received
      data = candpolls,
      mapping = aes(
        x = votingday - days(1),
        y = finalresults |>
          filter(state == thestate) |>
          select(matches(mcmullin$name)) |>
          as.numeric(),
        xend = dayafter,
        yend = finalresults |>
          filter(state == thestate) |>
          select(matches(mcmullin$name)) |>
          as.numeric()
      ),
      colour = mcmullin$colour,
      linewidth = 3.0,
      alpha = 0.4
    )
  }

  plot = plot + labs(
    title = stateplotstr,
    subtitle = "joint 97.5%-confidence intervals",
    x = "Polling Date",
    y = "",
    caption = "Polls rated" |>
      paste(mingrade, "or better", sep = " ")
  )
  plot = plot + scale_y_continuous(
    position = "right"
  )
  plot = plot + theme_classic()
  plot = plot + theme(
    plot.title = element_text(hjust = 0.5),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = unit(
      c(0.75,0.75,0.75,0.5), "cm"
    ),
    plot.subtitle = element_text(hjust = 0.5)
  )

  return(plot)

} # end of function stateplot

mainecds = c("Maine", "Maine CD-1", "Maine CD-2")
nebraskacdsonly = c("Nebraska CD-1", "Nebraska CD-2", "Nebraska CD-3")

plotstates = allstates[!(allstates %in% c(mainecds, nebraskacdsonly, oftpolled))]

for (state in plotstates) {
  state |>
    stateplot("Adjusted", "B+", earliestdate, polls) |>
    print()
}

for (state in oftpolled) {
  state |>
    stateplot("Raw", "B", earliestdate, polls) |>
    print()
  state |>
    stateplot("Adjusted", "B", earliestdate, polls) |>
    print()
  state |>
    stateplot("Raw", "A-", earliestdate, polls) |>
    print()
  state |>
    stateplot("Adjusted", "A-", earliestdate, polls) |>
    print()
}

# Maine has two congressional districts; as such, from the data of one congressional district and of the state as a whole, we can expect to reconstruct the data for the other congressional district (as long as the poll dates are relatively close together).

# Seeing as the polls for the Nebraskan congressional districts are so sparse, it's difficult to supplement the data with data from pan-Nebraskan polls -- the CD polls are so few in number that they can't serve to support or refute any supplemental data. Although Clinton and Trump polled comparably in the 2nd Nebraskan congressional district, it makes the most sense (for the sake of electoral simulation) to consider Nebraska's electoral votes as a whole, rather than allocating electoral votes by district.

# If we were to eliminate some of the polls from lower-graded pollsters, in some states we would run the risk of only keeping polls from one or two sources. Given that each pollster has their own bias, there is reason to avoid overly relying on a single source for polls.

# We supplement Maine congressional district data with Maine polls from higher-rated pollsters.

minmainegrade = "B"

gpolls = polls |>
  filter(has_element(mainecds, state)) |>
  mutate(middate = round_date(middate, unit = "week"))
mainepolls = gpolls |>
  filter(state == "Maine") |>
  select(middate, rawpoll_clinton, rawpoll_trump)
d1polls = gpolls |>
  filter(state == "Maine CD-1")
d1polls2 = gpolls |>
  filter(state == "Maine CD-1") |>
  mutate(rawpoll_clinton_dist1 = rawpoll_clinton, rawpoll_trump_dist1 = rawpoll_trump) |>
  select(middate, rawpoll_clinton_dist1, rawpoll_trump_dist1)

x = merge(mainepolls, d1polls2) |>
  as_tibble()
regc = lm(rawpoll_clinton_dist1 ~ rawpoll_clinton, data = x)
regc
anova(regc) # pretty lousy. May be due to repeated X-values with different Y-values;
# low number of datapoints
regt = lm(rawpoll_trump_dist1 ~ rawpoll_trump, data = x)
regt
anova(regt)

newdata = mainepolls |>
  mutate(
    rawpoll_clinton = regc$coefficients["(Intercept)"] + regc$coefficients["rawpoll_clinton"]*rawpoll_clinton,
    state = "Maine CD-1", # lying for the sake of working with stateplot
    grade = minmainegrade # ditto
  ) |>
  mutate(
    rawpoll_trump = regt$coefficients["(Intercept)"] + regt$coefficients["rawpoll_trump"]*rawpoll_trump
  ) |>
  bind_rows(d1polls)

plot = stateplot("Maine CD-1", "Raw", "C+", earliestdate, newdata)

print(plot)
