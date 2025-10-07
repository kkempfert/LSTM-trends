# Function to create a dataframe of date information for the study period. 
create_dates = function(start_yr = 1900, end_yr = 2024) {# start_date = "1900-01-01", end_date = "2024-12-31") {
  years = start_yr:end_yr
  
  dates_df = lapply(years, create_dates_year) 
  dates_df = do.call("rbind", dates_df)
  
  return(dates_df)
}

# Helper function for create_dates function.
create_dates_year = function(year) {
  # Form dates as if every year is a leap year, then remove day 60 (Feb. 29th) if the year is not a leap year
  day_of_year = 1:366
  
  len_month = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  month = unlist(lapply(1:12, function(i) rep(i, len_month[i])))
  day_of_month = unlist(lapply(1:12, function(i) 1:len_month[i]))
  
  len_season = c(len_month[12] + sum(len_month[1:2]),
                 sum(len_month[3:5]), 
                 sum(len_month[6:8]), 
                 sum(len_month[9:11]))
  day_of_JF = (len_month[12] + 1):(len_month[12] + sum(len_month[1:2]))
  day_of_D = 1:len_month[12]
  day_of_other = unlist(lapply(2:4, function(i) 1:len_season[i]))
  day_of_season = c(day_of_JF, day_of_other, day_of_D)
  
  season = sapply(month, get_season)
  
  raw_year = rep(year, 366)
  m_year = raw_year 
  m_year[month == 12] = year + 1 # meteorological year
  
  month2 = sapply(month, convert_digits)
  day_of_month2 = sapply(day_of_month, convert_digits)
  
  date = sapply(1:366, function(i) paste(raw_year[i], month2[i], day_of_month2[i], sep = "-"))
  
  dates_df = data.frame(date,
                        raw_year, raw_month = month, m_year, 
                        day_of_year, day_of_month, day_of_season,
                        season)
  
  if (!is_leap_year(year)) {
    dates_df = dates_df[-60, ] # remove entry for Feb. 29th
  }
  
  return(dates_df)
}

# Helper function for create_dates_year.
convert_digits = function(num) {
  if (num < 10) {
    return(paste0("0", num))
  } else {
    return(paste0(num))
  }
}

# Function to detect if a year is a leap year (returns an indicator)
is_leap_year = function(year) {
  if (year == 1900) {
    F
  } else if (year == 2000) {
    T
  } else if (year %% 4 == 0) {
    T
  } else {
    F
  }
}

# Function that returns the season that a month belongs to.
get_season = function(month) {
  season = month
  season[month %in% c(12, 1, 2)] = "DJF"
  season[month %in% 3:5] = "MAM"
  season[month %in% 6:8] = "JJA"
  season[month %in% 9:11] = "SON"
  return(season)
}

# Function that preprocesses a weather station's data based on the raw file. 
make_station_df = function(df, start_date = NULL, end_date = NULL, return_full = F, units_raw = "in") {
  if (mean(names(df) %in% c("station", "name")) != 0) {
    df = df[, -which(names(df) %in% c("station", "name"))]
  }
  inds_sub = which(!names(df) %in% c("date", "prcp"))
  if (!return_full) {
    df = df[, -inds_sub]
  }
  if (!is.null(start_date)) {
    start_ind = which(df$date == start_date)
    start_ind2 = which(dates$date == start_date)
  } else {
    start_ind = 1
    start_ind2 = 1
  }
  if (!is.null(end_date)) {
    end_ind = which(df$date == end_date)
    end_ind2 = which(dates$date == end_date)
  } else {
    end_ind = nrow(df)
    end_ind2 = nrow(dates)
  }
  
  df = df[start_ind:end_ind, ]
  dates = dates[start_ind2:end_ind2, ]
  
  # Sometimes there are entire entries missing from the GHCN data
  days_missing = sapply(dates$date, function(x) !(x %in% df$date))
  len_missing = length(which(days_missing))
  if (len_missing > 0) {
    # Create new variables with missing values
    create_var = function(len, var, inds_missing) {
      var2 = rep(NA, len)
      var2[!inds_missing] = var
      return(var2)
    }
    if (return_full) {
      vars = apply(df[, -1], 2, function(x) create_var(nrow(dates), x, days_missing)) %>% cbind.data.frame()
    } else {
      vars = data.frame(create_var(nrow(dates), df[, 2], days_missing))
    }
    
    names(vars) = names(df)[-1]
    
    # Merge variables with dates
    df = cbind(date = dates$date, vars, dates[, -1])
  } else {
    df = merge(df, dates, by = "date") 
  }
  
  # Convert the units to cm
  if (units_raw == "in") {
    df$prcp = df$prcp *2.54
  } else if (units_raw == "mm") {
    df$prcp = df$prcp * .1
  }
  
  # Replace NAs with 0s and create an indicator variable for whether the value is missing or not
  inds_NA = is.na(df$prcp)
  df$prcp[inds_NA] = rep(0, length(which(inds_NA)))
  dummy_NA = as.numeric(inds_NA)
  
  df$missing = dummy_NA
  
  if (return_full) {
    df_full = df
    df = df[, -inds_sub]
    return(list(df_sub = df, df_full = df_full))
  } else {
    return(df)
  }
}  

# Function that creates a palette of colors depending on the number of colors required. 
get_cat_palette = function(num_colors, prcp) {
  if (num_colors == 3 & prcp == T) {
    palette = list(blue = "#0f27ba", red = "#FF6A7b", green = "#39ae86")
  } else if (num_colors == 3 & prcp == F) {
    palette = list(purple = "#a073e3", red = "#FF6A7b", green = "#39ae86")
  } else if (num_colors == 5 & prcp == T) {
    palette = list(blue = "#0f27ba", purple = "#a073e3", red = "#FF6A7b", green = "#39ae86",
                   tan = "#F9A14D")
  } else if (num_colors == 10 & prcp == F) {
    palette = list(red = "#c4213d", orange = "#f57733", yellow = "#f7a943",
                   lightgreen = "#47cc69", darkgreen = "#2a8265", cyan = "#00ebeb",
                   periwinkle = "#2987e6", blue = "#0f25b8", purple = "#a073e3", pink = "#db53d5")
  }
  return(palette)
}

# Function that makes a plot of 5 meteorological variables. 
plot_m_vars = function(m_long) {
  palette = get_cat_palette(5, T)
  
  pl = ggplot(m_long) + aes(x = Day, y = Amt, colour = Weather) + geom_line() +
    facet_wrap(~ Weather, nrow = 5, ncol = 1, scales = "free_y", 
               strip.position = "left", 
               labeller = as_labeller(c(Rain = "Precipitation (cm)",
                                        Snow = "Snow (cm)", 
                                        MinTemp = "Min. Temperature (F)", 
                                        MaxTemp = "Max. Temperature (F)",
                                        WindSpeed = "Wind Speed (mi/hr)"))) + #"Fastest 1-min. Wind Speed"
    theme_classic() + ylab(NULL) +
    theme(strip.background = element_blank(), strip.placement = "outside",
          panel.border = element_rect(colour = "gray48", fill=NA), 
          panel.spacing = unit(0, "lines"), 
          axis.text.x = element_text(angle = 45, vjust = .5),
          title = element_text(size = .05),
          axis.title.x = element_text(size = 10),
          legend.position = "none") +
    scale_x_continuous(breaks=inds_yrs, labels=pl_yrs) +
    scale_colour_manual(values = c("Rain" = palette$blue, 
                                   "Snow" = palette$purple,
                                   "MinTemp" = palette$tan, "MaxTemp" = palette$red, 
                                   "WindSpeed" = palette$green))
  return(pl)
}
