# install.packages("data.table")
library(data.table)

setwd("E:/Repos/nCOV-research/")
# set datadir
datadir <- "./week2/"

# iterate over our csv files and append them to dataset
for (i in list.files(
  path = datadir,
  pattern = ".csv$"
)) {
  assign(
    i,
    data.table::fread(
      input = paste0(datadir, i),
      stringsAsFactors = T
    )
  )
}
invisible(gc())

# add type column
train.csv[, type := "train"]
test.csv[, type := "test"]
colnames(test.csv)[1] <- "Id"

# combine data
dataset <- data.table::rbindlist(
  l = list(train.csv, test.csv),
  fill = T
)

# delete unneeded columns, transform date
dataset[, date := as.Date(as.character(Date))
      ][
        , state := get("Province_State")
      ][
        , country := get("Country_Region")
        ][
      , c("Province_State", "Country_Region", "Date") := NULL
      ]

# create clean_country_names function
clean_country_names <- function(c) {
  if (c == "Bosnia and Herzegovina") {
    c <- "Bosnia"
  } else if (c == "Congo (Brazzaville)") {
    c <- "Congo"
  } else if (c == "Congo (Kinshasa)") {
    c <- "Congo (Democratic Republic of the)"
  } else if (c == "Czechia") {
    c <- "Czech Republic"
  } else if (c == "Gambia, The") {
    c <- "Gambia"
  } else if (c == "The Gambia") {
    c <- "Gambia"
  } else if (c == "Korea, South") {
    c <- "Korea (Democratic People's Republic of)"
  } else if (c == "North Macedonia") {
    c <- "Macedonia (the former Yugoslav Republic of)"
  } else if (c == "Republic of the Congo") {
    c <- "Congo (Democratic Republic of the)"
  } else if (c == "Taiwan*") {
    c <- "Taiwan"
  } else if (c == "Venezuela") {
    c <- "Venezuela (Bolivarian Republic of)"
  } else if (c == "Bolivia") {
    c <- "Bolivia (Plurinational State of)"
  } else if (c == "Brunei") {
    c <- "Brunei Darussalam"
  } else if (c == "Cote d'Ivoire") {
    c <- "Côte d'Ivoire"
  } else if (c == "Eswatini") {
    c <- "Swaziland"
  } else if (c == "Iran") {
    c <- "Iran (Islamic Republic of)"
  } else if (c == "Kosovo") {
    c <- "Republic of Kosovo"
  } else if (c == "Moldova") {
    c <- "Moldova (Republic of)"
  } else if (c == "Reunion") {
    c <- "Réunion"
  } else if (c == "Russia") {
    c <- "Russian Federation"
  } else if (c == "Tanzania") {
    c <- "Tanzania, United Republic of"
  } else if (c == "The Bahamas") {
    c <- "Bahamas"
  } else if (c == "US") {
    c <- "United States of America"
  } else if (c == "United Kingdom") {
    c <- "United Kingdom of Great Britain and Northern Ireland"
  } else if (c == "Vietnam") {
    c <- "Viet Nam"
  }
  return(c)
}

# apply clean_country_names function to work with REST API
# https://restcountries.eu/
dataset[, country := as.character(country)]
dataset[, country := sapply(country, function(x) { clean_country_names(x) })]
dataset[, country := factor(country)]

# sort data
dataset <- dataset[order(date)]
stopifnot(!is.unsorted(dataset$date))

# transform data as integer
dataset[, ("d") := as.integer(factor(date))]

# get countryinformation from api
country_data <- data.table::data.table()

for (c in sort(unique(as.character(dataset$country)))) {
  print(c)
  
  if (c %in% c("Diamond Princess", "Cruise Ship")) {
    next
  }
  
  # get payload from REST API: https://restcountries.eu/
  url <- paste0("https://restcountries.eu/rest/v2/name/", c)
  
  req <- httr::GET(url)
  
  if (req$status_code == 200) {
    
    payload <- data.table::as.data.table(
      jsonlite::fromJSON(
        txt = httr::content(req, "text"),
        simplifyVector = T
      )
    )
    
    # payload <- data.table::as.data.table(jsonlite::fromJSON(
    #   txt = url,
    #   simplifyVector = T
    # ))
    
    # get border table
    borders <- data.table::as.data.table(
      sapply(
        unlist(payload[name == c, .(borders)][, get("borders")]),
        FUN = function(x) { return(as.integer(1)) },
        simplify = F,
        USE.NAMES = TRUE
      )
    )
    
    # append country information
    country_data <- data.table::rbindlist(
      l = list(
        country_data,
        cbind(
          payload[name == c, .(name, region, subregion, population, area)],
          borders
        )
      ),
      fill = TRUE
    )
  } else {
    message(paste0("Error occured fetching ", c))
  }
}

# check if all countries but the cruise ship have been found
setdiff(unique(country_data$name), dataset[, sort(unique(as.character(get("country"))))])

# create population density feature
country_data[, pop_density := population / area][
  , region := factor(region)
  ][
  , subregion := factor(subregion)
  ]

# finalize border feature (NA = 0)
vec <- setdiff(colnames(country_data), c("name", "region", "subregion", "population", "area", "pop_density"))
country_data[, (vec) := lapply(.SD, function(x) { ifelse(is.na(x), as.integer(0), x) }), .SDcols = vec]

# merge country data to dataset
vec <- setdiff(colnames(country_data), c("population", "area"))
dataset <- data.table::merge.data.table(
  x = dataset,
  y = country_data[, (vec), with = F],
  by.x = "country",
  by.y = "name",
  all.x = T,
  suffixes = c("", "")
)

# add weekday feature
dataset[
  , weekday := factor(weekdays(date))
  ][
  , weekend := as.integer(
    ifelse(weekday %in%
             c("Saturday", "Sunday"), 1, 0)
  )]

# # create delta features
# dataset[type == "train", `:=` (
#   delta_confirmed = as.integer(ConfirmedCases - dplyr::lag(ConfirmedCases)),
#   delta_fatalities = as.integer(Fatalities - dplyr::lag(Fatalities))
#   ), by = c("country", "state")]

# log1p-transform target variables
dataset[type == "train", `:=` (
  ConfirmedCases_log = log1p(ConfirmedCases),
  Fatalities_log = log1p(Fatalities)
)]

# create days from first case feature
dataset[, days_from_first_case := NA_integer_]
dataset[ConfirmedCases > 0 | is.na(ConfirmedCases), 
        days_from_first_case := as.integer(factor(date)), by = c("country", "state")
        ]
dataset[ConfirmedCases == 0, days_from_first_case := as.integer(0)]

# create weights for wuhan and south korea (as training gold standard)
# dataset[type == "train" & country == "Korea (Democratic People's Republic of)", weight := 3.0]
# dataset[type == "train" & state == "Hubei", weight := 4.0]
# dataset[type == "train" & is.na(weight), weight := 1.0]

# Missings
sapply(dataset, function(x) sum(is.na(x)))

rm(train.csv, test.csv); invisible(gc())

# Prepare the learner

# devtools::install_github("kapsner/mlr3learners.lightgbm", upgrade = "never")
library(mlr3)
library(mlr3learners.lightgbm)

nrounds <- 5000
early_stop <- 50

# create train/test splits
by(dataset$date, dataset$type, summary)

# add public leaderboard dates
date_validation_start <- "2020-03-19"
date_validation_end <- dataset[type == "train", as.character(max(date))]

split <- list()
# train indices for public and private leaderboard
split$train_index_public <- which(dataset$type == "train" &
                                    dataset$date < as.Date(date_validation_start)
)
split$train_index_private <- which(dataset$type == "train")
# use training data here for public index to be able to evaluate the metric
split$validation_index_public <- which(dataset$type == "train" &
                                         dataset$date >= as.Date(date_validation_start) & 
                                         dataset$date <= as.Date(date_validation_end)
)
# test indices for public and private leaderboard
split$test_index_public <- which(dataset$type == "test" &
                                   dataset$date <= as.Date(date_validation_end)
)
split$test_index_private <- which(dataset$type == "test" &
                                    dataset$date > as.Date(date_validation_end)
)

params <- list(
  "objective" = "regression"
  , "learning_rate" = 0.1
  , "seed" = 17L
  , "metric" = "rmse"
  , "bagging_fraction" = 0.7
  , "bagging_freq" = 5
  , "feature_fraction" = 0.7
  #, "device_type" = "gpu"
  #, "max_bin" = 63L
  #, "num_threads" = 1L
)

# Stage 1: predict confirmed cases for public and private leaderboard

# get needed columns
vec <- setdiff(colnames(dataset), c(
  "Id", "Fatalities", "date", "type", "ConfirmedCases", "Fatalities_log")
)

# create task for confirmed cases
task <- mlr3::TaskRegr$new(
  id = "confirmed_prediction",
  backend = dataset[, (vec), with = F],
  target = "ConfirmedCases_log"
)
#task$col_roles$feature <- setdiff(task$col_roles$feature, "weight")
#task$col_roles$weight <- "weight"

# train learner 2 times for public and private leaderboard
for (l in c("public", "private")) {
  
  # instantiate the learner
  learner <- mlr3::lrn("regr.lightgbm")
  
  # define learning arguments
  learner$early_stopping_rounds <- early_stop
  learner$nrounds <- nrounds
  # define learner parameters
  learner$param_set$values <- params
  
  learner$train(task, row_ids = split[[paste0("train_index_", l)]])
  invisible(gc())
  
  if (l == "public") {
    
    # Evaluate on validation set
    predictions <- learner$predict(
      task,
      row_ids = split$validation_index_public
    )
    rmsle1 <- predictions$score(mlr3::msr("regr.rmsle"))
    
    # Make Prediction
    predictions <- learner$predict(
      task,
      row_ids = split[[paste0("test_index_", l)]]
    )
    
  } else if (l == "private") {
    
    predictions <- learner$predict(task, row_ids = split[[paste0("test_index_", l)]])
  }
  
  # save predictions:
  dataset[split[[paste0("test_index_", l)]],
          ConfirmedCases := round(exp(predictions$response), 4)]
}

imp <- learner$lgb_learner$importance2()
imp$plot

paste0("Public rmsle ConfirmedCases: ", round(rmsle1, 4))

## Plotting stage 1 results

aggr_data <- dataset[, .(
  sum(ConfirmedCases)
), keyby = c("d", "date", "type")][order(d)]
colnames(aggr_data)[4] <- "ConfirmedCases"

dd <- data.table::melt.data.table(
  data = aggr_data,
  id.vars = c("d", "date", "type")
)

p <- ggplot2::ggplot(
  data = dd,
  ggplot2::aes_string(
    x = "date",
    y = "value",
    colour = "type"
  )
) + 
  ggplot2::geom_line() + 
  ggplot2::facet_wrap(~ variable) + 
  ggplot2::labs(
    title = "Prediction stage 1: ConfirmedCases worldwide"
  )
p

# Stage 2: predict fatalities for public and private leaderboard

# get needed columns
vec <- setdiff(colnames(dataset), c(
  "Id", "date", "type", "ConfirmedCases_log", "Fatalities")
)

# create task for fatalities
task2 <- mlr3::TaskRegr$new(
  id = "fatalities_prediction",
  backend = dataset[, (vec), with = F],
  target = "Fatalities_log"
)
#task$col_roles$feature <- setdiff(task$col_roles$feature, "weight")
#task$col_roles$weight <- "weight"

# train learner 2 times for public and private leaderboard
for (l in c("public", "private")) {
  
  # instantiate the learner
  learner <- mlr3::lrn("regr.lightgbm")
  
  # define learning arguments
  learner$early_stopping_rounds <- early_stop
  learner$nrounds <- nrounds
  # define learner parameters
  learner$param_set$values <- params
  
  learner$train(task2, row_ids = split[[paste0("train_index_", l)]])
  invisible(gc())
  
  if (l == "public") {
    
    # Evaluate on validation set
    predictions <- learner$predict(
      task2,
      row_ids = split$validation_index_public
    )
    rmsle2 <- predictions$score(mlr3::msr("regr.rmsle"))
    
    # Make Prediction
    predictions <- learner$predict(
      task2,
      row_ids = split[[paste0("test_index_", l)]]
    )
    
  } else if (l == "private") {
    
    predictions <- learner$predict(task2, row_ids = split[[paste0("test_index_", l)]])
  }
  
  # save predictions:
  dataset[split[[paste0("test_index_", l)]],
          Fatalities := round(exp(predictions$response), 4)]
}

imp <- learner$lgb_learner$importance2()
imp$plot

paste0("Public rmsle Fatalities: ", rmsle2)

paste0("Final metric: ", (rmsle1 + rmsle2) / 2)

## Plotting stage 2 results

aggr_data <- dataset[, .(
  sum(Fatalities)
), keyby = c("d", "date", "type")][order(d)]
colnames(aggr_data)[4] <- "Fatalities"

dd <- data.table::melt.data.table(
  data = aggr_data,
  id.vars = c("d", "date", "type")
)

p <- ggplot2::ggplot(
  data = dd,
  ggplot2::aes_string(
    x = "date",
    y = "value",
    colour = "type"
  )
) + 
  ggplot2::geom_line() + 
  ggplot2::facet_wrap(~ variable) + 
  ggplot2::labs(
    title = "Prediction stage 2: Fatalities worldwide"
  )
p

# Plot results

aggr_data <- dataset[, .(
  sum(ConfirmedCases),
  sum(Fatalities)
), keyby = c("d", "date", "type")][order(d)]
colnames(aggr_data)[4:5] <- c("ConfirmedCases", "Fatalities")

dd <- data.table::melt.data.table(
  data = aggr_data,
  id.vars = c("d", "date", "type")
)

p <- ggplot2::ggplot(
  data = dd,
  ggplot2::aes_string(
    x = "date",
    y = "value",
    colour = "type"
  )
) + 
  ggplot2::geom_line() + 
  ggplot2::facet_wrap(~ variable) + 
  ggplot2::labs(
    title = "Predicted Cases worldwide"
  )
p

# Create submission data

submit <- dataset[
  c(split$test_index_public,
    split$test_index_private), .(
      Id,
      ConfirmedCases,
      Fatalities
    )]
colnames(submit)[1] <- "ForecastId"

#outdir <- "export/"
outdir <- ""
data.table::fwrite(
  submit,
   paste0(outdir, "submission.csv"))
