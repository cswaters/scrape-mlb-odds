## Load packages ----
library(tidyverse)
library(readxl)

## Load parsing functions ----
source('funcs.R')

## Get file names and seasons ----
data_files <- get_files()
seasons <- get_seas_from_file()

## Run parsing pipeline ----
df <- odds_pipeline(data_files, seasons)

## Clean up some bad OU data and change Dodgers teamname from `LOS` to `LAD` ----
## Export the the dataframe to a csv file
mlb_polish_dataset(df) %>% 
  write_csv('mlb_odds_2010_2019.csv')
