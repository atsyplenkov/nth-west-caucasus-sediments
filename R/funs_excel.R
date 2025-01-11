# 01. Extract-table to long df --------------------------------------------
xtract_table <- function() {
  # Read wide table from clipboard
  tt <- read.table(file = "clipboard", sep = "\t", header = F) %>%
    magrittr::set_colnames(c("year", as.character(c(1:12))))

  # Transform to long table and copy to clipboard
  tt %>%
    # na_if("-") %>%
    # na_if("") %>%
    as_tibble() %>%
    mutate_all(~as.character(.)) %>%
    mutate_at(vars(2:13), ~str_remove(., "-")) %>%
    mutate_at(
      vars(2:13),
      ~parse_number(., locale = locale(decimal_mark = ","))
    ) %>%
    mutate_at(vars(1), ~parse_number(.)) %>%
    gather(month, ss, -year) %>%
    mutate(month = as.integer(month)) %>%
    arrange(year, month) %>%
    complete(year = seq(min(year), max(year), by = 1)) %>%
    group_by(year) %>%
    nest() %>%
    mutate(n = map_dbl(data, ~nrow(.x))) %>%
    mutate(
      data = map2(
        .x = data,
        .y = n,
        ~(if (.y == 1) tibble(month = c(1:12), ss = rep(NA_real_, 12)) else .x)
      )
    ) %>%
    unnest(data) %>%
    ungroup() %>%
    dplyr::select(-n) %>%
    clipr::write_clip()
}

clip2long <- function() {
  read.table(file = "clipboard", sep = "\t", header = F) %>%
    magrittr::set_colnames(c(1:12)) %>%
    rowid_to_column(var = "decade") %>%
    gather(month, ssd, -decade) %>%
    mutate(ssd = parse_double(ssd, locale = locale(decimal_mark = ","))) %>%
    transmute(
      year = 2020,
      month,
      decade,
      ssd
    ) %>%
    write.table(
      file = "clipboard",
      sep = "\t",
      col.names = F,
      row.names = F,
      na = ""
    )
}

# 02. Batch excel sheet reading -------------------------------------------
sheet_to_table <- function(.path, i) {
  aid <- readxl::excel_sheets(.path)[i] %>%
    str_remove("(\\=)") %>%
    as.numeric()

  # readxl::read_xlsx(.path,
  #                   sheet = i,
  #                   skip = 2,
  #                   col_types = rep("text", 4),
  #                   trim_ws = T,
  #                   na = "-",
  #                   .name_repair = "minimal",
  #                   progress = F) %>%
  df <- openxlsx::read.xlsx(
    xlsxFile = .path,
    sheet = i,
    detectDates = F,
    startRow = 4,
    colNames = F
  ) %>%
    magrittr::set_colnames(c("year", "month", "decade", "ssd")) %>%
    mutate(
      aid = aid,
      ssd = as.character(ssd),
      ssd = na_if(ssd, "-"),
      .before = 1
    ) %>%
    as_tibble()

  tt <- tryCatch(
    as.numeric(df$ssd),
    error = function(e) e,
    warning = function(w) w
  )

  if (is(tt, "warning")) cat(paste0("Parse error at sheet ", aid, "\n"))

  df$ssd <- as.numeric(df$ssd)
  return(df)
}

# 03. Decade to date ------------------------------------------------------
decade_to_date <- function(year, month, decade) {
  year <- as.numeric(year)
  month <- as.numeric(month)
  decade <- as.numeric(decade)

  d <- lubridate::days_in_month(month) %>%
    seq_len()

  decade_start_date <- split(d, ceiling(seq_along(d) / 10)) %>%
    purrr::map_dbl(~median(.x)) %>%
    floor()

  lubridate::make_date(year, month, decade_start_date[decade])
}

# 04. Possible date entries -----------------------------------------------
all_dates <- tibble(
  year = rep(seq(from = 2000, 2020, by = 1), 3 * 12)
) %>%
  arrange(year) %>%
  mutate(month = rep(c(1:12), 3 * 21)) %>%
  arrange(year, month) %>%
  group_by(year, month) %>%
  mutate(decade = c(1:3)) %>%
  ungroup() %>%
  mutate(date = decade_to_date(year, month, decade)) %>%
  pull(date)

# 05. Helper functions to calc correct stats ------------------------------
# Max
.max_na <- function(.v) {
  if (base::all(base::is.na(.v))) {
    return(NA_real_)
  } else {
    base::max(.v, na.rm = T)
  }
}

# Mean
.mean_na <- function(.v) {
  if (base::all(base::is.na(.v))) {
    return(NA_real_)
  } else {
    base::mean(.v, na.rm = T)
  }
}

# Median
.median_na <- function(.v) {
  if (base::all(base::is.na(.v))) {
    return(NA_real_)
  } else {
    stats::median(.v, na.rm = T)
  }
}

# Standard deviation
.sd_na <- function(.v) {
  if (base::all(base::is.na(.v))) {
    return(NA_real_)
  } else {
    stats::sd(.v, na.rm = T)
  }
}

# Mean Absolute Deviation
.mad_na <- function(.v) {
  if (base::all(base::is.na(.v))) {
    return(NA_real_)
  } else {
    stats::mad(.v, center = mean(.v, na.rm = T), na.rm = T)
  }
}

# 06. Visualize missing data ----------------------------------------------
explore_miss <- function(.data, .var, .year = year, .id = id) {
  min_max_yr <- .data %>%
    drop_na({{ .var }}) %>%
    pull({{ .year }}) %>%
    quantile(c(0, 1))

  years_of_measurements <- .data %>%
    group_by({{ .id }}) %>%
    summarise(
      var_mean_n = sum(!is.na({{ .var }}))
    ) %>%
    mutate(id = as_factor({{ .id }}))

  .data %>%
    drop_na({{ .var }}) %>%
    mutate(id = as_factor({{ .id }})) %>%
    ggplot(aes(x = {{ .year }}, y = {{ .id }})) +
    geom_errorbarh(aes(xmax = {{ .year }}, xmin = {{ .year }}), linewidth = 4) +
    geom_text(
      data = years_of_measurements %>%
        dplyr::filter(var_mean_n != 0),
      aes(x = 2022, y = as.character(id), label = paste0("n = ", var_mean_n)),
      color = "gray50"
    ) +
    scale_y_discrete(
      limits = years_of_measurements %>%
        dplyr::filter(var_mean_n != 0) %>%
        arrange(var_mean_n) %>%
        pull(id)
    ) +
    scale_x_continuous(
      limits = c(min_max_yr[1], 2022),
      breaks = seq(min_max_yr[1], 2022, by = 5)
    ) +
    labs(
      x = "",
      y = "Gage id"
    )
}

# 07. Read 1976-1980 data -------------------------------------------------
read_1976 <- function(.path, .sheet) {
  # Excel info
  all_sheets <- readxl::excel_sheets(.path)

  station_id <- all_sheets[.sheet]

  # Load excel
  data <- readxl::read_excel(.path, sheet = .sheet, .name_repair = "minimal")

  data_mean <- data %>%
    dplyr::select(year:ssd) %>%
    mutate(id = as.numeric(station_id), .before = year) %>%
    group_by(id, year) %>%
    summarise(
      ssd_mean = .mean_na(ssd),
      ssd_sd = .sd_na(ssd),
      ssd_mad = .mad_na(ssd),
      .groups = "drop"
    )

  data_max <- data %>%
    dplyr::select(year = year_max, ssd_max)

  data_mean %>%
    left_join(data_max, by = "year")
}

# 08. Pretty years range --------------------------------------------------
years_range <- function(df) {
  df %>%
    mutate(temp = year) %>%
    complete(temp = seq(min(year), max(year), by = 1)) %>%
    mutate(gr = rleid(is.na(year))) %>%
    dplyr::filter(!is.na(year)) %>%
    group_by(gr) %>%
    summarise(
      n = n(),
      range = ifelse(
        n == 1,
        glue::glue("{year}"),
        glue::glue("{first(year)}-{last(year)}")
      ),
      .groups = "drop"
    ) %>%
    pull(range) %>%
    paste(collapse = ", ")
}
