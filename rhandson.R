inps <- c(
  "work_order", "date", "building", "room", "issue",
  "trade_responsible", "trade_attended",
  "staff_called", "staff_attended_1",
  "staff_attended_1_type", "staff_attended_2",
  "staff_attended_2_type", "contractor_1", "contractor_2"
)

read_rda <- function(path){
  envir <- environment()
  data_name <- load(path, envir = envir)
  
  get(data_name)
}

library(tidyverse)
library(tibble)
library(rhandsontable)
library(stringr)
library(glue)
emps <- read_rda("/Users/adam/repo/pr-data-pins/data/employee.rda")
emps <- emps |> 
  mutate(name = glue("{given_names} {surname}")) |>
  as.data.frame()
long_sentence <- rep(sentences[1], 6) |> glue_collapse()

trade <- read_rda("/Users/adam/repo/pr-data-pins/data/wo_trade.rda") |> 
  distinct(trade, .keep_all = T)
bldg <- read_rda("/Users/adam/repo/pr-data-pins/data/wo_bldg.rda")

bldgs <- paste0(bldg$building_number, " - ", bldg$building_name)
bldg_factor <- factor(sample(bldgs, 2), levels = bldgs)
trades <- paste0(trade$trade_code, " - ", trade$trade)
trade_factor <- factor(sample(trades, 2), levels = str_sort(trades))

staff_sample <- sample(emps$name, 2)
staff_factor <- factor(staff_sample, levels = str_sort(unique(emps$name)))
room_factor <- c(NA_integer_, as.integer(32))
wo_factor <- c(as.integer(51233), NA_integer_)

DF = tibble(
  hour = c(10, 4),
  minute = c(0, 43),
  "am/pm" = factor(c("am", "pm"), levels = c("am", "pm")),
  date = c(seq(from = Sys.Date(), by = "days", length.out = 2)),
  "Work Order #" = c(as.integer(513233), NA_integer_), 
  building = bldg_factor,
  room = room_factor, 
  issue = c(long_sentence, sample(sentences, 1)),
  reporter = NA_character_,
  trade_responsible = factor(NA_character_, levels = str_sort(trade$trade)), 
  trade_attended_1 = trade_factor,
  staff_called_1 = staff_factor,
  staff_attended_1 = staff_factor,
  trade_attended_2 = trade_factor,
  staff_called_2 = staff_factor,
  staff_attended_2 = staff_factor,
  contractor_1 = c(rep("P&S Electric", 1), NA_character_),
  contractor_2 = NA_character_,
) |> 
  mutate(
    across(
      ! c("Work Order #", room, date, issue), \(x) as_factor(x)
    )
  ) |> 
  rename_with(\(x) x |> str_replace_all("_", " ") |> str_to_title())

# try updating big to a value not in the dropdown
rhandsontable(
  DF, 
  rowHeaders = NULL, 
  width = 3000,
  height = 700,
) %>%
  hot_cols(
    names(DF),
    allowInvalid = TRUE, 
    strict = FALSE,
    colWidths = c(50, 60, 70, 100, 75, 213, 50, 308, rep(130, 10)),
    # fixedColumnsLeft = 2,
    renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
             if ([4, 7, 8, 9, 16, 17].includes(col)) {
              td.style.background = 'lightgrey';
             } 
           }",
    halign = "htCenter"
  ) %>%
  hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
  hot_validate_numeric(cols = 5, min = 100000, max = 999999) %>%
  hot_validate_numeric(cols = 1, min = 0, max = 12) %>%
  hot_validate_numeric(cols = 2, min = 0, max = 60)  %>%
  hot_col(8, halign = "htLeft")
