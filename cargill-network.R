# load libraries
library(dplyr) # data manipulation
library(igraph) # network manipulation
library(png) # read PNG images

# create directory for figures
dir.create("fig", showWarnings = FALSE)

# generate data

## April 20: 484 total cases
## https://www.cbc.ca/news/canada/calgary/cargill-meat-plant-closed-outbreak-covid-19-1.5538824
## primary cases: "484 cases are now linked to the plant, 360 of whom are Cargill workers."
## secondary cases: "The remaining 124 cases are people who came into contact with those workers."
## More details (April 17 CBC story): https://www.cbc.ca/news/canada/calgary/cargill-alberta-covid-19-deena-hinshaw-1.5537377

## set number of primary and secondary cases
n_prim <- 360
n_second <- 124

## set random seed
set.seed(as.integer(as.Date("2020-04-20")))

## generate primary cases linked to the plant
cases <- data.frame(
  ### the plant itself
  case_origin = 0,
  ### primary cases
  case_number = 1:n_prim,
  case_type = "Primary case",
  stringsAsFactors = FALSE
)

## distribute secondary cases among primary cases (since seconday cases < primary cases, many will be 0)
secondary <- data.frame(
  case_origin = cases$case_number,
  ### distribute secondary cases
  n_secondary_cases = rmultinom(
    n = 1,
    size = n_second,
    ### probabilities are not uniform across primary cases
    prob = runif(
      n = length(cases$case_number),
      min = 0,
      max = 0.75
    )
  )
)

## turn each secondary case into a line and join to cases data frame
secondary <- secondary %>%
  filter(n_secondary_cases > 0) %>%
  {
    data.frame(
      case_origin = rep(
        x = .$case_origin,
        times = .$n_secondary_cases
      ),
      case_number = seq(n_prim + 1, n_prim + n_second),
      case_type = "Secondary case",
      stringsAsFactors = FALSE
    )
  }

## join cases data frames
cases <- cases %>%
  bind_rows(secondary) %>%
  mutate(case_type = as.factor(case_type))

# convert data to graph
cases_net <-
  graph.data.frame(d = cases[, c("case_origin", "case_number")], directed = FALSE)
V(cases_net)$color <-
  c("gray", ifelse(cases$case_type == "Primary case", "red", "blue"))
V(cases_net)$size <-
  c(10, rep(3, times = nrow(cases)))

# create and save plot
png("fig/network_cargill.png",
    width = 600,
    height = 600)
par(mar = c(0.2, 0.2, 0.2, 0.2))
plot(cases_net, layout = layout_nicely, vertex.label = NA)
legend(
  "topleft",
  legend = levels(cases$case_type),
  col = "black",
  pch = 21,
  pt.bg = c("red", "blue"),
  pt.cex = 1.8,
  cex = 1.8,
  bty = "n"
)
dev.off()