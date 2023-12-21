# libraries ---------------------------------------------------------------
library(tidyverse) 
library(readxl)    
library(openxlsx)  
library(lubridate) 
library(atslib)
library(here)      
library(qs)

source(here("R", "funs_ggplot2.R"))
source(here("R", "funs_excel.R"))

theme_set(theme_kbn())

# Ermolaev's database -----------------------------------------------------
df_q_ermol <- 
  openxlsx::read.xlsx(
    here("data", "hydro","кубань_для цыпленкова.xlsx"),
    sheet = 1) %>% 
  as_tibble() %>% 
  rename(id = 1, year = 2, q_mean = 3) 

df_ssd_ermol <- 
  openxlsx::read.xlsx(
    here("data", "hydro","кубань_для цыпленкова.xlsx"),
    sheet = 2) %>% 
  as_tibble() %>% 
  rename(id = 1, year = 2, ssd_mean = 3) 

df_ermol <- 
  full_join(
    df_q_ermol, 
    df_ssd_ermol,
    by = c("id", "year")
  ) %>% 
  group_by(id) %>% 
  complete(
    year = seq(min(df_q_ermol$year), 2020, by = 1)
  )

df_ermol %>% 
  explore_miss(q_mean)

# Alexey's database -------------------------------------------------------
# i.e. 2000-2019 
# Load id database
gage_id <- 
  openxlsx::read.xlsx(
    here("data", "hydro", "Список постов.xlsx"),
    sheet = 2
  ) %>% 
  as_tibble() %>% 
  dplyr::select(id = `Код.поста`,
                aid = `алексей_ид`,
                river = `Река`,
                gage = `Пост`,
                area = `F`,
                comment = `Примечание`) %>% 
  mutate(id = as.double(id))

# 2000-2007
path_2000_2007  <-  
  here("data", "hydro", "итоговый (2000-2007) 19.06.xlsx")

alex_2007 <-
  seq_along(readxl::excel_sheets(path_2000_2007)) %>%
  as.list() %>%
  map_dfr(~sheet_to_table(path_2000_2007, .x))

# 2008-2020
path_2008_2020 <- 
  here("data", "hydro", "итоговый (2008-2020) 1.03.xlsx")

alex_2019 <-
  seq_along(excel_sheets(path_2008_2020)) %>%
  as.list() %>%
  map_dfr(~sheet_to_table(path_2008_2020, .x))

# Combine Alexey's data together
df_ssd_alex <- 
  bind_rows(alex_2007, alex_2019) %>% 
  # convert year-month-decade to {date}
  mutate(date = decade_to_date(year, month, decade),
         day = yday(date),
         .after = "year") %>% 
  mutate(across(c(aid, year), as.numeric)) %>% 
  # Join with {id} table
  left_join(
    gage_id %>% 
      dplyr::select(id, aid),
    by = "aid"
  ) %>% 
  # add NA if the data was skipped
  group_by(id) %>% 
  complete(
    date = all_dates
  ) %>% 
  ungroup() %>% 
  mutate(year = lubridate::year(date)) %>% 
  dplyr::select(
    id, year, date, ssd
  )

# Estimate yearly-based {SSD} stats
df_ssd_alex_year <-
  df_ssd_alex %>% 
  group_by(id, year) %>% 
  summarise(
    ssd_mean = .mean_na(ssd),
    ssd_max = .max_na(ssd),
    ssd_sd = .sd_na(ssd),
    ssd_mad = .mad_na(ssd),
    .groups = "drop"
  ) %>% 
  arrange(id, year)

df_ssd_alex_year %>% 
  explore_miss(ssd_mad)

# SSD 1976-1980 -----------------------------------------------------------
path1976 <- 
  here("data", "hydro", "kuban_1976-1980_ssd-mean.xlsx")

sheets1976 <- 
  readxl::excel_sheets(path1976)

data_1976 <- 
  seq(1, length(sheets1976)) %>%
  map_dfr(~read_1976(path1976, .sheet = .x))

# Mean SSD DB -------------------------------------------------------------
ssd_year <- 
  df_ssd_ermol %>% 
  filter(year < 1976) %>% 
  bind_rows(data_1976) %>% 
  bind_rows(df_ssd_alex_year) %>% 
  group_by(id) %>% 
  complete(year = seq(1927, 2020, by = 1)) %>% 
  ungroup()

ssd_year %>% 
  explore_miss(ssd_mean)

# Save --------------------------------------------------------------------
# По состоянию на 1 марта 2023 года:
# нет данных из ОГХ, так как бд Ермолаева не полная!

qs::qsave(
  ssd_year,
  here("workflow", "01_sediment-database", "data", "SSD-yr-all_21dec23.qs")
)

qs::qsave(
  df_ssd_alex,
  here("workflow", "01_sediment-database", "data", "SSD-decade-all_21dec23.qs")
)
