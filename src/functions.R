# Define functions --------------------------------------------------------

# Calculates the mean of the 5 smallest values in a vector
# rounded to one decimal place
# Requires a numeric vector
# Returns a numeric value
min_five_mean <- function(x){
  return(round(mean(head(sort(x), 5)), 1))
}

# Calculates the mean of the 5 largest values in a vector
# rounded to one decimal place
# Requires a numeric vector
# Returns a numeric value
max_five_mean <- function(x){
  return(round(mean(tail(sort(x), 5)), 1))
}

# Creates a one-way frequency table
# Requires a dataframe, a column name as a string
# and a path to save the outputs.
# Returns nothing, saves a csv file as a side effect
write_frequency_table <- function(data, col, path){
  data %>% 
    tabyl(!!col) %>% 
    adorn_pct_formatting %>% 
    write_csv(., str_c(path, "_Freq_", col, ".csv"))
}

# Creates a two-way frequency table 
# Requires a dataframe, two column names as characters 
# and a path to save the outputs.
# Returns nothing, saves atwo csv files as a side effect
write_2x2frequency_tables <- function(data, col1, col2, path){
  
  data %>% 
    tabyl(!!rlang::sym(col1), !!rlang::sym(col2)) %>% 
    adorn_title() %>% 
    write_csv(., str_c(path, "_2x2Freq_", col1, "_", col2, "_n.csv"))
  
  data %>% 
    tabyl(!!rlang::sym(col1), !!rlang::sym(col2)) %>% 
    adorn_percentages() %>% 
    adorn_pct_formatting() %>% 
    adorn_title() %>% 
    write_csv(., str_c(path, "_2x2Freq_", col1, "_", col2, "_percent.csv"))
}

# Creates a summary of the number of admissions by admission category (emergency, 
# elective non-cancer and elective cancer) by counting daily admissions on a national
# level and extracting the busiest and least busy day, and by calculating the average 
# day and the average weekend day.
# Requires a dataframe with columns EPISTART, EPISTART_WEEKEND_FLAG, ADMITYPE, 
# a column name as a character to stratify the results and a path to save the outputs.
# Returns nothing, saves a csv file as a side effect
create_admissions_summary <- function(data, var, path){
  
  # overall summary
  daily_adm <- FAEs %>% 
    group_by(EPISTART, ADMITYPE, !!rlang::sym(var)) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    complete(EPISTART, ADMITYPE, !!rlang::sym(var), fill = list(n = 0))
  
  summary <- daily_adm %>% 
    group_by(ADMITYPE, !!rlang::sym(var)) %>% 
    summarise(average_day = round(mean(n), 1),
              busiest_day = max(n),
              leastbusy_day = min(n),
              n_days = n())
  
  # weekday summary
  daily_adm_weekday <- FAEs %>% 
    filter(EPISTART_WEEKEND_FLAG == FALSE) %>% 
    group_by(EPISTART, ADMITYPE, !!rlang::sym(var)) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    complete(EPISTART, ADMITYPE, !!rlang::sym(var), fill = list(n = 0))
  
  summary_weekday <- daily_adm_weekday %>% 
    group_by(ADMITYPE,!!rlang::sym(var)) %>% 
    summarise(average_week_day = round(mean(n), 1),
              n_days_week = n())
  
  # weekend summary
  daily_adm_weekend <- FAEs %>% 
    filter(EPISTART_WEEKEND_FLAG == TRUE) %>% 
    group_by(EPISTART, ADMITYPE, !!rlang::sym(var)) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    complete(EPISTART, ADMITYPE, !!rlang::sym(var), fill = list(n = 0))
  
  summary_weekend <- daily_adm_weekend %>% 
    group_by(ADMITYPE,!!rlang::sym(var)) %>% 
    summarise(average_weekend_day = round(mean(n), 1),
              n_days_weekend = n())
  
  summary_combined <- summary %>% 
    left_join(summary_weekday, by = c("ADMITYPE", var)) %>% 
    left_join(summary_weekend, by = c("ADMITYPE", var))
  
  write_csv(summary_combined, str_c(path, var, ".csv"))
  
}