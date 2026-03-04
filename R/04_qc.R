#!/usr/bin/env Rscript

# Copyright 2018 Province of British Columbia
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

# QC script to compare data currently served by Shiny (R/shiny/data)
# versus newly updated data (data/) for wxstn_df and wind_df.
# Output: Excel workbook with by-site row counts and date range checks.

library(tidyverse)
library(glue)
library(openxlsx)

shiny_data_dir <- "R/shiny/data"
updated_data_dir <- "data"
qc_output_dir <- "data/qc"

if (!dir.exists(qc_output_dir)) {
  dir.create(qc_output_dir, recursive = TRUE)
}

detect_date_col <- function(df, preferred = c("Date", "Day")) {
  existing <- preferred[preferred %in% names(df)]
  if (length(existing) > 0) {
    return(existing[1])
  }

  lower_names <- tolower(names(df))
  candidates <- names(df)[
    stringr::str_detect(lower_names, "date|day|datetime|timestamp|time")
  ]
  if (length(candidates) == 0) {
    return(NA_character_)
  }
  candidates[1]
}

coerce_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }
  as.Date(as.character(x))
}

site_range_summary <- function(df, site_col = "Site", date_col = NA_character_) {
  if (!(site_col %in% names(df))) {
    stop(glue("Expected site column '{site_col}' not found."))
  }

  out <- df |>
    group_by(Site = .data[[site_col]]) |>
    summarise(rows = n(), .groups = "drop")

  if (!is.na(date_col) && date_col %in% names(df)) {
    out <- df |>
      mutate(.qc_date = coerce_date(.data[[date_col]])) |>
      group_by(Site = .data[[site_col]]) |>
      summarise(
        rows = n(),
        non_na_dates = sum(!is.na(.qc_date)),
        min_date = if (all(is.na(.qc_date))) as.Date(NA) else min(.qc_date, na.rm = TRUE),
        max_date = if (all(is.na(.qc_date))) as.Date(NA) else max(.qc_date, na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    out <- out |>
      mutate(
        non_na_dates = NA_integer_,
        min_date = as.Date(NA),
        max_date = as.Date(NA)
      )
  }

  out
}

compare_dataset <- function(dataset_name, preferred_date_col = c("Date", "Day")) {
  shiny_path <- file.path(shiny_data_dir, glue("{dataset_name}.csv"))
  updated_path <- file.path(updated_data_dir, glue("{dataset_name}.csv"))

  if (!file.exists(shiny_path)) {
    stop(glue("Missing Shiny file: {shiny_path}"))
  }
  if (!file.exists(updated_path)) {
    stop(glue("Missing updated file: {updated_path}"))
  }

  shiny_df <- readr::read_csv(shiny_path, show_col_types = FALSE)
  updated_df <- readr::read_csv(updated_path, show_col_types = FALSE)

  shiny_date_col <- detect_date_col(shiny_df, preferred = preferred_date_col)
  updated_date_col <- detect_date_col(updated_df, preferred = preferred_date_col)

  shiny_summary <- site_range_summary(shiny_df, site_col = "Site", date_col = shiny_date_col) |>
    rename(
      shiny_rows = rows,
      shiny_non_na_dates = non_na_dates,
      shiny_min_date = min_date,
      shiny_max_date = max_date
    )

  updated_summary <- site_range_summary(updated_df, site_col = "Site", date_col = updated_date_col) |>
    rename(
      updated_rows = rows,
      updated_non_na_dates = non_na_dates,
      updated_min_date = min_date,
      updated_max_date = max_date
    )

  comparison <- full_join(shiny_summary, updated_summary, by = "Site") |>
    mutate(
      dataset = dataset_name,
      shiny_site_present = !is.na(shiny_rows),
      updated_site_present = !is.na(updated_rows),
      row_diff = coalesce(updated_rows, 0L) - coalesce(shiny_rows, 0L),
      min_date_diff_days = as.numeric(updated_min_date - shiny_min_date),
      max_date_diff_days = as.numeric(updated_max_date - shiny_max_date),
      range_status = case_when(
        !shiny_site_present ~ "only_in_updated",
        !updated_site_present ~ "only_in_shiny",
        is.na(shiny_min_date) & is.na(updated_min_date) ~ "no_date_column_or_empty",
        shiny_min_date == updated_min_date & shiny_max_date == updated_max_date ~ "date_range_match",
        TRUE ~ "date_range_changed"
      )
    ) |>
    arrange(Site)

  metadata <- tibble(
    dataset = dataset_name,
    shiny_file = shiny_path,
    updated_file = updated_path,
    shiny_date_column = shiny_date_col,
    updated_date_column = updated_date_col,
    shiny_rows_total = nrow(shiny_df),
    updated_rows_total = nrow(updated_df),
    shiny_sites = n_distinct(shiny_df$Site),
    updated_sites = n_distinct(updated_df$Site),
    run_timestamp = as.character(Sys.time())
  )

  list(comparison = comparison, metadata = metadata)
}

wx_qc <- compare_dataset("wxstn_df", preferred_date_col = c("Date"))
wind_qc <- compare_dataset("wind_df", preferred_date_col = c("Day", "Date"))

overall_summary <- bind_rows(wx_qc$comparison, wind_qc$comparison) |>
  group_by(dataset, range_status) |>
  summarise(site_count = n(), .groups = "drop") |>
  arrange(dataset, range_status)

metadata <- bind_rows(wx_qc$metadata, wind_qc$metadata)

qc_file <- file.path(
  qc_output_dir,
  glue("qc_shiny_vs_updated_{format(Sys.Date(), '%Y%m%d')}.xlsx")
)

wb <- createWorkbook()
addWorksheet(wb, "summary")
addWorksheet(wb, "wxstn_df")
addWorksheet(wb, "wind_df")
addWorksheet(wb, "metadata")

writeData(wb, "summary", overall_summary)
writeData(wb, "wxstn_df", wx_qc$comparison)
writeData(wb, "wind_df", wind_qc$comparison)
writeData(wb, "metadata", metadata)

saveWorkbook(wb, qc_file, overwrite = TRUE)
message(glue("QC workbook written: {qc_file}"))
