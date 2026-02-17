# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Stand-alone script for updating weather station data
# formatting and outputting for next step analyses for the app
#
# Script to format datasets containing annual updates and output formatted
# datasets to 'processed' folder.
# Prep: 2025 - using `20241125 MeteorologicalNetworks-FERN-VF-shared.xlsx`'s
# StationList sheet as metadata to match file basename to station code on the
# metadata. Renamed to match records from line 39 on and added nbcclim_label
# column to the metadata to supply station name to display on the app.
# Copied `20241125 MeteorologicalNetworks-FERN-VF-shared.xlsx` into data folder
# Set new_data_dir variable to directory path containing station updates.

library(tidyverse)
library(testthat)
library(lubridate)
library(glue)
library(janitor)

`%nin%` <- Negate(`%in%`)
col_list <- c(
  "Date",
  "Day",
  "Rain",
  "Pressure",
  "Temp",
  "RH",
  "DewPt",
  "Wind Speed",
  "Gust Speed",
  "Wind Direction",
  "Solar Radiation"
  )

if (!dir.exists("data/processed")) {
  dir.create("data/processed")
}
new_data_dir <- "data/2026_update/"

## station list to be updated
## reading in updated files with new wind records
updates <- dir(new_data_dir, pattern = "csv", full.names = TRUE)
if (length(updates) == 0) {
  stop(glue("No CSV updates found under {new_data_dir}"))
}

## station lat long info, rename stations according to update csvs
## left to the ~ is Vanesssa's metadata lookup's station name,
## right is raw data's station name
sites <- readxl::read_excel(
  "data/20241125 MeteorologicalNetworks-FERN-VF-shared.xlsx",
  sheet = "StationList") |>
  mutate(station_name = case_when(
    station_name == "Atlin School" ~ "Atlin school",
    station_name == "BarrenWx" ~ "Barren",
    station_name == "BlackhawkWx" ~ "Blackhawk",
    station_name == "BoulderWx" ~ "BoulderCr",
    station_name == "BowronPit" ~ "Bowron Pit",
    station_name == "BulkleyWx" ~ "Bulkley PGTIS 1",
    station_name == "Canoe Mountain Stn" ~ "Canoe",
    station_name == "ChapmanWx" ~ "Chapman",
    station_name == "ChiefLakeWx" ~ "ChiefLk",
    station_name == "CoalmineWx" ~ "Coalmine",
    station_name == "CPFWx" ~ "CPF PGTIS 3",
    station_name == "CrookedLk" ~ "Crooked Lake",
    station_name == "CrystalWx" ~ "CrystalLk",
    station_name == "DunsterWx" ~ "Dunster",
    station_name == "EndakoWx" ~ "Endako",
    station_name == "GeorgeWx" ~ "George",
    station_name == "GunnelWx" ~ "Gunnel",
    station_name == "HourglassWx" ~ "Hourglass",
    station_name == "Hudson Bay Mtn2" ~ "HudsonBayMtn2",
    station_name == "IBB2Wx" ~ "IBB2 Ganokwa Canyon",
    station_name == "IBB3Wx" ~ "IBB3 Pine Creek",
    station_name == "MacJxnWx" ~ "MacJxn",
    station_name == "MiddleforkWx" ~ "Middlefork",
    station_name == "BednestiWx" ~ "Tamarac",
    station_name == "PinkWx" ~ "PinkMtnWx",
    station_name == "SaxtonWx" ~ "SaxtonLakeWx",
    station_name == "SeebachWx" ~ "Seebach",
    station_name == "SumWxCC" ~ "Sunbeam",
    station_name == "ThompsonWx" ~ "Thompson",
    station_name == "Willow-BowronWx" ~ "WillowBowron PGTIS 2",
    TRUE ~ station_name
  ))

test_that("All updated csv basenames exist in metadata", {
  update_names <- updates |>
    basename() |>
    stringr::str_remove("\\.csv$") |>
    sort()

  meta_names <- sites$station_name |> sort()

  expect_true(setequal(update_names, meta_names))

  })

glue("Total station this update ",
           length(updates))

## column variables that may or may not be existent
optional_cols <- c(
  "Water Content 15cm",
  "Water Content 5cm",
  "Water Content 30cm",
  "Soil Temp",
  "Wetness",
  "Snow depth"
  )

optional_col_lookup <- c(
  SD_avg = "Snow depth",
  WC_avg_5cm = "Water Content 5cm",
  WC_avg_15cm = "Water Content 15cm",
  WC_avg_30cm = "Water Content 30cm",
  W_avg = "Wetness",
  ST_avg  = "Soil Temp"
)

# function to pick a column name from a vector of column names
# based on a pattern and a negate pattern
# used is a vector of column names that have already been used
# negate_pattern is a pattern to exclude from the column names
pick_col <- function(nms, pattern, used = character(), negate_pattern = NULL) {
  idx <- stringr::str_detect(nms, pattern)
  if (!is.null(negate_pattern)) {
    idx <- idx & !stringr::str_detect(nms, negate_pattern)
  }
  candidates <- setdiff(nms[idx], used)
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[1]
}

pick_col_any <- function(nms, patterns, used = character()) {
  for (pattern in patterns) {
    picked <- pick_col(nms, pattern, used = used)
    if (!is.na(picked)) {
      return(picked)
    }
  }
  NA_character_
}

## adding new records to originals, then output to directory
for (file_i in seq_along(updates)) {
  glue("Processing file {updates[file_i]}")
  df <- read.csv(updates[file_i], check.names = FALSE, encoding = "UTF-8") |>
    clean_names()

  raw_names <- names(df)

  date_name <- pick_col(raw_names, "^(date|datetime|time_stamp|timestamp)")
  day_name <- pick_col(raw_names, "^day$", used = date_name)

  # some dfs don't have a datetime column
  if (!is.na(date_name)) {
    names(df)[names(df) == date_name] <- "Date"
    if (!is.na(day_name)) {
      names(df)[names(df) == day_name] <- "Day"
    } else {
      df$Day <- substr(df$Date, 1, 10)
    }
    df$Day <- substr(df$Date, 1, 10)
  } else {
    # No datetime column present, add it with Day column for analysis consistency
    if (!is.na(day_name)) {
      names(df)[names(df) == day_name] <- "Day"
    } else {
      names(df)[1] <- "Day"
    }
    df$Date <- df$Day
  }

  df <- df |>
    select(Date, Day, everything())

  wc_5 <- pick_col_any(
    names(df),
    c("^water.*content.*(?:^|_)5(?:_|$)", "^wc.*(?:^|_)5(?:_|$)")
  )
  wc_30 <- pick_col_any(
    names(df),
    c("^water.*content.*(?:^|_)30(?:_|$)", "^wc.*(?:^|_)30(?:_|$)"),
    used = wc_5
  )
  wc_15 <- pick_col_any(
    names(df),
    c(
      "^water.*content.*(?:^|_)15(?:_|$)",
      "^wc.*(?:^|_)15(?:_|$)",
      "^wc_cal(_m3_m3)?$",
      "^water_content_m3_m3$"
    ),
    used = c(wc_5, wc_30)
  )

  standardized_cols <- c(
    Rain = pick_col(names(df), "^rain"),
    Pressure = pick_col(names(df), "^pressure"),
    Temp = pick_col(names(df), "^(temp|air_temp|temperature)", negate_pattern = "soil"),
    RH = pick_col(names(df), "^(rh|relative_humidity)"),
    DewPt = pick_col(names(df), "^(dewpt|dew_point|dew)"),
    `Wind Speed` = pick_col(names(df), "^(wind.*speed|ws$)", negate_pattern = "gust"),
    `Gust Speed` = pick_col(names(df), "^(gust.*speed|wind_gust|gs$)"),
    `Wind Direction` = pick_col(names(df), "^(wind.*direction|wd$)"),
    `Solar Radiation` = pick_col(names(df), "^(solar.*radiation|sr$)"),
    `Water Content 5cm` = wc_5,
    `Water Content 15cm` = wc_15,
    `Water Content 30cm` = wc_30,
    `Soil Temp` = pick_col(names(df), "^soil.*temp"),
    Wetness = pick_col(names(df), "^wetness"),
    `Snow depth` = pick_col(names(df), "^snow.*depth")
  )

  standardized_cols <- standardized_cols[!is.na(standardized_cols)]
  for (new_name in names(standardized_cols)) {
    old_name <- standardized_cols[[new_name]]
    names(df)[names(df) == old_name] <- new_name
  }

  fname <- str_match(basename(updates[file_i]), "(.*)\\..*$")[,2]

  glue("--------------------------------- processing {updates[file_i]}")
  glue("Data range: {range(df$Date)}")
  print("Data shape (ncol, nrow) and unique datetimes: ")
  print(paste(ncol(df), nrow(df), length(unique(df$Date))))


  ## keep expected weather columns as well as optional soil, moisture and snow data
  df <- df |>
    select(any_of(c(col_list, optional_cols)))

  df$key <- seq_len(nrow(df))
  print(names(df))

  glue("Finished cleaning column names for {fname}\n",
       "Checking for expected columns and formatting data")

  # special case for Atlin
  if (toupper(fname) == "ATLIN SCHOOL") {
    # Atlin station doesn't have pressure data (all NANs)
    df <- df |>
      select(Date, Day, Rain, Temp, RH, DewPt, "Wind Speed",
             "Gust Speed", "Wind Direction", "Solar Radiation", "key")
  } else {
    # assuming first 11 columns are expected to be had by all stations, test this
    # is true from pre-defined column list.
    glue("Formatted dataframe contains all of defined variables in the columns")
    test_that(
      "Formatted dataframe contains all of defined variables in the columns", {
        expect_equal(names(df)[1:11], col_list)
      })

    glue("Formatted datafame only contains expected optional columns")
    test_that(
      "Formatted datafame only contains expected optional columns", {
        expect_true(all(names(df) %in% c(col_list, optional_cols, "key")))
      }
    )

    ## correct 'NAN' character to be numbers
    ## could not use tidyverse dynamic variables to index thru cols
    df$Rain[toupper(df$Rain) == "NAN"] <- NA
    df$Pressure[toupper(df$Pressure) == "NAN"] <- NA
    df$Temp[toupper(df$Temp) == "NAN"] <- NA
    df$RH[toupper(df$RH) == "NAN"] <- NA
    df$DewPt[toupper(df$DewPt) == "NAN"] <- NA
    df$`Wind Speed`[toupper(df$`Wind Speed`) == "NAN"] <- NA
    df$`Gust Speed`[toupper(df$`Gust Speed`) == "NAN"] <- NA
    df$`Wind Direction`[toupper(df$`Wind Direction`) == "NAN"] <- NA
    df$`Solar Radiation`[toupper(df$`Solar Radiation`) == "NAN"] <- NA
    df <- df |>
      mutate(across(any_of(optional_cols),
                    ~ case_when(. == "NAN" ~ NA,
                                TRUE ~ .))
      )
  }

  ## do a count and filter out daily records that are less than 12 observations
  ## for any variable, if we have more than 12 records
  ## then calculate stats (min, max and mean)

  ## get a QA df to join with full df for qualified datetime records
  df_qa <- df |>
    gather(key = 'var', value = 'val', -Date, -Day, -key) |>
    group_by(Day, var) |>
    mutate(s = sum(!is.na(val))) |>
    filter(s > 12)

  df_qa <- tibble(Date = unique(df_qa$Date))
  df_qa <- df_qa[complete.cases(df_qa), ]
  df_qa <- df_qa |> filter(nchar(Date) > 0)
  df_wx <- left_join(df_qa, df, by = "Date")


  ## hourly wind columns
  ## cleaning input updated dataframe
  df_wind <- df_wx |>
    select(Date, Day, `Wind Speed`, `Wind Direction`) |>
    rename("WS" = `Wind Speed`,
           "WD" =  `Wind Direction`)
  df_wind$Site <- as.character(
    sites[sites$station_name == fname, "nbcclim_label"]
    )

  ## if summing all NAs, return NA instead of 0
  suma <- function(x) if (all(is.na(x))) x[NA_integer_] else sum(x, na.rm = TRUE)

  if (toupper(fname) == "ATLIN SCHOOL") {
    df_wx <- df_wx |>
      group_by(Day) |>
      summarise(
        Rain_sum = round(suma(as.numeric(Rain)), 2),
        # Pressure_avg = round(mean(as.numeric(Pressure), na.rm = TRUE), 2),
        Temp_max = round(max(as.numeric(Temp), na.rm = TRUE), 2),
        Temp_min = round(min(as.numeric(Temp), na.rm = TRUE), 2),
        Temp_avg = round(mean(as.numeric(Temp), na.rm = TRUE), 2),
        RH_avg = round(mean(as.numeric(RH), na.rm = TRUE), 2),
        DP_avg = round(mean(as.numeric(DewPt), na.rm = TRUE), 2),
        WS_avg = round(mean(as.numeric(`Wind Speed`), na.rm = TRUE), 2),
        GS_max = round(max(as.numeric(`Gust Speed`), na.rm = TRUE), 2),
        WD_avg = round(mean(as.numeric(`Wind Direction`), na.rm = TRUE), 2),
        SR_avg = round(mean(as.numeric(`Solar Radiation`), na.rm = TRUE), 2),
        across(any_of(optional_cols), mean, na.rm = TRUE)
      ) |>
      mutate(across(any_of(optional_cols), round, 2)) |>
      rename(any_of(optional_col_lookup))

  } else {
    df_wx <- df_wx |>
      group_by(Day) |>
      summarise(
        Rain_sum = round(suma(as.numeric(Rain)), 2),
        Pressure_avg = round(mean(as.numeric(Pressure), na.rm = TRUE), 2),
        Temp_max = round(max(as.numeric(Temp), na.rm = TRUE), 2),
        Temp_min = round(min(as.numeric(Temp), na.rm = TRUE), 2),
        Temp_avg = round(mean(as.numeric(Temp), na.rm = TRUE), 2),
        RH_avg = round(mean(as.numeric(RH), na.rm = TRUE), 2),
        DP_avg = round(mean(as.numeric(DewPt), na.rm = TRUE), 2),
        WS_avg = round(mean(as.numeric(`Wind Speed`), na.rm = TRUE), 2),
        GS_max = round(max(as.numeric(`Gust Speed`), na.rm = TRUE), 2),
        WD_avg = round(mean(as.numeric(`Wind Direction`), na.rm = TRUE), 2),
        SR_avg = round(mean(as.numeric(`Solar Radiation`), na.rm = TRUE), 2),
        across(any_of(optional_cols), mean, na.rm = TRUE)
      ) |>
      mutate(across(any_of(optional_cols), round, 2)) |>
      rename(any_of(optional_col_lookup))

  }


  df_wx$Site <- as.character(sites[sites$station_name == fname, "nbcclim_label"])
  df_wx$Longitude <- as.character(sites[sites$station_name == fname, "lon"])
  df_wx$Latitude <- as.character(sites[sites$station_name == fname, "lat"])
  df_wx$Elevation <- as.character(sites[sites$station_name == fname, "elev"])

  # uncomment these lines for QA
  ## check if the colnames are correctly renamed. Compare among two dataframes
  ## for number of NAs and dimensions
  # print("---------------------------------------------------- summary of wx df")
  # print(summary(df_wx))
  # print(head(df_wx))
  #
  # ## look for number of NAs, if they are consistent with inputs'.
  # ## Look for start and end date and see if everything updated.
  # ## Also detect for mutating rows (shouldn't be any for updates)
  # print("-------------------------------------------------- summary of wind df")
  # print(summary(df_wind))
  # print(head(df_wind))

  # format fname to be correct
  fname <- strsplit(fname, "_21_22")[[1]][1]

  # do a second pattern split if an extra underscore is contained
  fname <- strsplit(fname, "21_22")[[1]][1]


  ## check to see if all is numeric
  df_wx <- df_wx |>
    mutate(Longitude = as.numeric(str_trim(Longitude, side = "both")),
           Latitude = as.numeric(str_trim(Latitude, side = "both")),
           Elevation = as.numeric(str_trim(Elevation, side = "both")))


  ## stop and examine outputs
  # browser()
  write_csv(df_wx, paste0("data/processed/", fname, "_wx.csv"))
  write_csv(df_wind, paste0("data/processed/", fname, "_wind.csv"))
  glue("----------------------------- Finished processing {fname}")

  # browser()

}


