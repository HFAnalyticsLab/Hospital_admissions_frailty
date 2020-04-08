####
# Secondary priority analysis:
# Number of admissions, distributions and trends for 2015 to 2019.
####

library(tidyverse)
library(DBI)
library(tidylog)
library(janitor)
library(lubridate)

source("src/file_paths.R")
source("src/functions.R")
source("src/vars.R")

# Connect to HES database -------------------------------------------------

db <- DBI::dbConnect(RSQLite::SQLite(), database_path_2015_to_2019_VM)

dbListTables(db)
dbListFields(db, "APC")

# Number of records  ------------------------------------------------------

# Only admission episodes
# Only acute trusts
# Only valid admissions
# From Nov 2018 to Oct 2019
# Only elective and emergency admissions

filter_query <- "WHERE SUBSTR(PROCODE3, 1, 1) = 'R' AND EPIORDER = 1 AND EPI_VALID = 1 
                 AND ADMIMETH IN (21, 22, 23, 24, 25, 26, 27, 28, 66, 67, 69, 11, 12, 13)
                 AND ((SUBSTR(EPISTART, 1, 4) = '2019' AND SUBSTR(EPISTART, 6, 2) IN ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10')) 
                     OR SUBSTR(EPISTART, 1, 4) IN ('2018', '2017', '2016', '2015')
                     OR (SUBSTR(EPISTART, 1, 4) = '2014' AND SUBSTR(EPISTART, 6, 2) IN ('11', '12')))"

# Total number of admissions
n_adm <- dbGetQuery(db, paste0("SELECT COUNT(*) FROM APC ", filter_query))

n_adm_unfinished <- dbGetQuery(db, "SELECT COUNT(*) FROM APC 
                                    WHERE SUBSTR(PROCODE3, 1, 1) = 'R' AND EPIORDER = 1 
                                    AND ADMIMETH IN (21, 22, 23, 24, 25, 26, 27, 28, 66, 67, 69, 11, 12, 13)
                                    AND ((SUBSTR(EPISTART, 1, 4) = '2019' AND SUBSTR(EPISTART, 6, 2) IN ('01', '02', '03', '04', '05', '06', '07', '08', '09', '10')) 
                                     OR  (SUBSTR(EPISTART, 1, 4) = '2018' AND SUBSTR(EPISTART, 6, 2) IN ('11', '12'))) 
                                    AND ENCRYPTED_HESID_MISSING = 0
                                    AND ADMIDATE_MISSING = 0 AND PROCODE3_MISSING = 0 
                                    AND EPISTART IS NOT NULL AND EPIEND IS NOT NULL 
                                    AND EPIKEY IS NOT NULL AND EPISTAT <> 3")

n_adm_by_year <- dbGetQuery(db, paste0("SELECT COUNT(*) AS RECORDS, SUBSTR(EPISTART, 1, 4) AS YEAR,
                                        COUNT(DISTINCT SUBSTR(EPISTART, 6, 2)) AS MONTHS
                                        FROM APC ", filter_query, " GROUP BY YEAR"))

write_csv(n_adm_by_year, str_c(results_path_2015_to_2019, 'Admissions_by_year.csv'))



# Function ----------------------------------------------------------------


# Yearly summaries, creates summaries from analysis_priority_1
summarise_year <- function(db, year, path){
  
  APC_db <- tbl(db, 'APC')
  
  FAEs <- APC_db %>% 
    filter(substr(EPISTART, 1, 4) == year &
             EPIORDER == 1 & substr(PROCODE3, 1, 1) =='R' & 
             EPI_VALID == 1 &
             ADMIMETH %in% c(21:28, 66, 67, 69, 11:13)) %>% 
    select(ENCRYPTED_HESID, ADMIDATE_FILLED, ADMIMETH, ADMISORC, DISDATE, 
           DISMETH, EPISTART, EPIEND, EPIDUR_CALC, EPIORDER, EPISTAT, 
           EPI_VALID, PROCODE3, DIAG_01, SEX, ETHNOS, CCG19ons, CCG19, 
           STARTAGE_CALC, STARTAGE, FILENAME, MYDOB,
           CHARLSON_WSCORE, ELIXHAUSER_WSCORE_AHRQ, ELIXHAUSER_WSCORE_VW, IMPFRAILTY_SCORE,
           IMPFRAILTY_NORM_SCORE) %>% 
    collect()
  
  
  FAEs <- FAEs %>% 
    mutate(STARTAGE_BANDS_NARROW = cut(STARTAGE_CALC, breaks = age_breaks_narrow, 
                                       labels = age_labels_narrow, include.lowest = TRUE, right = FALSE),
           STARTAGE_BANDS_NARROW = fct_explicit_na(STARTAGE_BANDS_NARROW, na_level = "Missing"),
           STARTAGE_BANDS_WIDE = cut(STARTAGE_CALC, breaks = age_breaks_wide, 
                                     labels = age_labels_wide, include.lowest = TRUE, right = FALSE),
           STARTAGE_BANDS_WIDE = fct_explicit_na(STARTAGE_BANDS_WIDE, na_level = "Missing"),
           SEX_FCT = factor(SEX, levels = c("1", "2", "9", "0")),
           SEX_FCT = fct_recode(SEX_FCT, Male = "1", Female = "2", Unspecified = "9", Unknown = "0"),
           SEX_FCT = fct_explicit_na(SEX_FCT, na_level = "Missing"),
           STARTAGE_SDC = ifelse(STARTAGE < 100, STARTAGE, "100+"),
           STARTAGE_SDC = factor(STARTAGE_SDC, levels = c(paste(1:99), '100+')),
           STARTAGE_SDC = fct_explicit_na(STARTAGE_SDC, na_level = "Missing"))
  
  ## Mean and standard deviations
  FAEs %>% 
    select(IMPFRAILTY_SCORE, IMPFRAILTY_NORM_SCORE, CHARLSON_WSCORE, ELIXHAUSER_WSCORE_AHRQ,
           ELIXHAUSER_WSCORE_VW) %>% 
    pivot_longer(everything(), names_to = "score", values_to = "value") %>% 
    group_by(score) %>% 
    summarise(min5mean = min_five_mean(value), 
              mean = round(mean(value), 1),
              sd = round(sd(value), 1),
              max5mean = max_five_mean(value),
              n = n()) %>% 
    write_csv(str_c(path, 'Summaries_byYear_', year, '_mean_sd.csv'))
  
  # By age band
  FAEs %>% 
    select(STARTAGE_BANDS_WIDE, IMPFRAILTY_SCORE, IMPFRAILTY_NORM_SCORE, CHARLSON_WSCORE, 
           ELIXHAUSER_WSCORE_AHRQ, ELIXHAUSER_WSCORE_VW) %>% 
    pivot_longer(-STARTAGE_BANDS_WIDE, names_to = "score", values_to = "value") %>% 
    group_by(STARTAGE_BANDS_WIDE, score) %>% 
    summarise(min5mean = min_five_mean(value), 
              mean = round(mean(value), 1),
              sd = round(sd(value), 1),
              max5mean = max_five_mean(value),
              n = n()) %>% 
    write_csv(., str_c(path, 'Summaries_byYear_', year, '_STARTAGE_BANDS_WIDE_mean_sd.csv'))
  
  
  
  ## One-way frequency tables
  vars_to_summarise_1D <- c("IMPFRAILTY_SCORE", "CHARLSON_WSCORE", 
                            "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW",  
                            "STARTAGE_BANDS_WIDE", "SEX_FCT", "STARTAGE_SDC")
  
  walk(vars_to_summarise_1D, 
       write_frequency_table, data = FAEs, path = str_c(path, "Summaries_byYear_", year))
  
  # Histogram of startage
  age_histogram <- FAEs %>% 
    tabyl(STARTAGE_SDC) %>% 
    ggplot(aes(x = STARTAGE_SDC, y = n)) +
    geom_bar(stat = "identity") +
    ylab("Number of admissions") +
    ggtitle("Admissions by age (Nov 2018 - Oct 2019)") +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) 
  ggsave(str_c(path, 'Summaries_byYear_', year, '_STARTAGE_SDC_histogram.png'), age_histogram,
         width = 16, height = 4)
  
  ## Two-way frequency tables
  
  vars_to_summarise_2D <- c("IMPFRAILTY_SCORE", "CHARLSON_WSCORE", 
                            "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW")
  
  # Scores vs. wide age bands
  walk(vars_to_summarise_2D, write_2x2frequency_tables, data = FAEs, 
       path = str_c(path, 'Summaries_byYear_', year), col1 = "STARTAGE_BANDS_WIDE")
  
  # Scores vs. frailty
  walk(c("CHARLSON_WSCORE", "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW", "ETHNOS", 
         "CCG19"), 
       write_2x2frequency_tables, data = FAEs, 
       path = str_c(path, 'Summaries_byYear_', year), col1 = "IMPFRAILTY_SCORE")
  
  # Age vs. sex
  write_2x2frequency_tables(data = FAEs, path = str_c(path, 'Summaries_byYear_', year), 
                            col1 = "STARTAGE_BANDS_WIDE", col2 = "SEX_FCT")
  
}


# Create summaries by year ------------------------------------------------

c("2015", "2016", "2017", "2018", "2019") %>% 
  walk(summarise_year, db = db, path = results_path_2015_to_2019)


# Combine yearly summaries ------------------------------------------------

read_summary <- function(path, skip = 0){
  data_year <- as.integer(str_split(path, "_")[[1]][3])
  data <- read_csv(path, skip = skip) %>% 
    mutate(year = data_year) %>% 
    select(everything(), year)
}

list.files(path = results_path_2015_to_2019, pattern = "Summaries_byYear_201._mean_sd.csv",
           full.names = TRUE)  %>% 
  map_dfr(read_summary) %>% 
  write_csv(str_c(results_path_2015_to_2019, "Summaries_byYear_COMBINED_mean_sd.csv"))


list.files(path = results_path_2015_to_2019, pattern = "Summaries_byYear_201._STARTAGE_BANDS_WIDE_mean_sd.csv",
           full.names = TRUE)  %>% 
  map_dfr(read_summary) %>% 
  mutate(STARTAGE_BANDS_WIDE = str_c(" ", STARTAGE_BANDS_WIDE)) %>%  #to prevent excel from converting to dates
  write_csv(str_c(results_path_2015_to_2019, "Summaries_byYear_COMBINED_STARTAGE_BANDS_WIDE_mean_sd.csv"))

combine_summary <- function(path, var, skip = 0){
  
  data <- list.files(path = results_path_2015_to_2019, 
             pattern = str_c("Summaries_byYear_201.", var, ".csv"),
             full.names = TRUE)  %>% 
    map_dfr(read_summary, skip = skip) %>% 
    select(year, everything())
  

  if("STARTAGE_BANDS_WIDE" %in% names(data)) {
    data <- data %>% 
      mutate(STARTAGE_BANDS_WIDE = str_c(" ", STARTAGE_BANDS_WIDE))  #to prevent excel from converting to dates
  }

  write_csv(data, str_c(results_path_2015_to_2019, "Summaries_byYear_COMBINED", var, ".csv"))
  
}

str_c("_Freq_", c("IMPFRAILTY_SCORE", "CHARLSON_WSCORE", "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW",  
  "STARTAGE_BANDS_WIDE", "SEX_FCT", "STARTAGE_SDC")) %>% 
  walk(combine_summary, path = results_path_2015_to_2019)


str_c("_2x2Freq_STARTAGE_BANDS_WIDE_", 
      c("IMPFRAILTY_SCORE", "CHARLSON_WSCORE", "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW"),
      "_n") %>% 
  walk(combine_summary, path = results_path_2015_to_2019, skip = 1)



# Disconnect db -----------------------------------------------------------

dbDisconnect(db)
