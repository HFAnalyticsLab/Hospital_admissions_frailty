####
# Primary priority analysis:
# Number of admissions, distributions and trends for 2019.
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

db <- DBI::dbConnect(RSQLite::SQLite(), database_path_2019_VM)

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
                  OR  (SUBSTR(EPISTART, 1, 4) = '2018' AND SUBSTR(EPISTART, 6, 2) IN ('11', '12')))"

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

# should be managable in memory

# Extract admissions from database ---------------------------------------------

APC_db <- tbl(db, 'APC')

FAEs <- APC_db %>% 
  filter(EPIORDER == 1 & substr(PROCODE3, 1, 1) =='R' & EPI_VALID == 1 &
           ADMIMETH %in% c(21:28, 66, 67, 69, 11:13) &
           ((substr(EPISTART, 1, 4) == '2019' & substr(EPISTART, 6, 7) %in% c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10')) |
            (substr(EPISTART, 1, 4) == '2018' & substr(EPISTART, 6, 7) %in% c('11', '12')))) %>% 
  select(ENCRYPTED_HESID, ADMIDATE_FILLED, ADMIMETH, ADMISORC, DISDATE, 
         DISMETH, EPISTART, EPIEND, EPIDUR_CALC, EPIORDER, EPISTAT, 
         EPI_VALID, PROCODE3, DIAG_01, SEX, ETHNOS, CCG19ons, CCG19, 
         STARTAGE_CALC, STARTAGE, FILENAME, MYDOB,
         CHARLSON_WSCORE, ELIXHAUSER_WSCORE_AHRQ, ELIXHAUSER_WSCORE_VW, IMPFRAILTY_SCORE,
         IMPFRAILTY_NORM_SCORE) %>% 
  collect()


FAEs <- FAEs %>% 
  mutate(ELECTIVE = ifelse(ADMIMETH %in% c(11, 12, 13), TRUE, FALSE),
         DIAG_01_CANCER = str_detect(replace_na(DIAG_01, ""), str_c(cancer_codes, collapse = "|")),
         ELECTIVE_NONCANCER = ifelse(ELECTIVE == TRUE & DIAG_01_CANCER == FALSE, TRUE, FALSE), 
         ELECTIVE_CANCER = ifelse(ELECTIVE == TRUE & DIAG_01_CANCER == TRUE, TRUE, FALSE),
         EMERGENCY = ifelse(ADMIMETH %in% c(21:28, 66, 67, 69), TRUE, FALSE),
         ADMITYPE = case_when(EMERGENCY == 1 ~ 'Emergency',
                              ELECTIVE_NONCANCER == 1 ~'Elective non-cancer',
                              ELECTIVE_CANCER == 1 ~ 'Elective cancer'),
         STARTAGE_BANDS_NARROW = cut(STARTAGE_CALC, breaks = age_breaks_narrow, 
                                     labels = age_labels_narrow, include.lowest = TRUE, right = FALSE),
         STARTAGE_BANDS_NARROW = fct_explicit_na(STARTAGE_BANDS_NARROW, na_level = "Missing"),
         STARTAGE_BANDS_WIDE = cut(STARTAGE_CALC, breaks = age_breaks_wide, 
                                   labels = age_labels_wide, include.lowest = TRUE, right = FALSE),
         STARTAGE_BANDS_WIDE = fct_explicit_na(STARTAGE_BANDS_WIDE, na_level = "Missing"),
         STARTAGE_OVER65 = case_when(STARTAGE_CALC > 65 ~ ">65",
                                     STARTAGE_CALC <= 65 ~ "<=65", 
                                     is.na(STARTAGE_CALC) ~ "Missing"),
         STARTAGE_Under18 = case_when(STARTAGE_CALC >= 18 ~ "18+",
                                     STARTAGE_CALC < 18 ~ "<18", 
                                     is.na(STARTAGE_CALC) ~ "Missing"),
         SEX_FCT = as.factor(SEX),
         SEX_FCT = fct_recode(SEX_FCT, Male = "1", Female = "2", Unspecified = "9", Unknown = "0"),
         SEX_FCT = fct_explicit_na(SEX_FCT, na_level = "Missing"),
         EPISTART_WEEKDAY = wday(EPISTART, label = TRUE),
         EPISTART_WEEKEND_FLAG = ifelse(EPISTART_WEEKDAY %in% c("Sat", "Sun"), TRUE, FALSE),
         IMPFRAILTY_SCORE_TERTILE = case_when(IMPFRAILTY_SCORE == 0 ~ "0",
                                              IMPFRAILTY_SCORE %in% c(1,2,3) ~ " 1-3",
                                              IMPFRAILTY_SCORE %in% c(4,5,6) ~ " 4-6",
                                              IMPFRAILTY_SCORE %in% c(7,8,9) ~ " 7-9"),
         STARTAGE_CHARLSON = case_when(STARTAGE_OVER65 == ">65" & CHARLSON_WSCORE >= 2 ~ ">65 and >=2 conditions",
                                     STARTAGE_OVER65 == ">65" & CHARLSON_WSCORE < 2 ~ ">65 and < 2 conditions",
                                     STARTAGE_OVER65 == "<=65" & CHARLSON_WSCORE >= 2 ~ "<=65 and >=2 conditions",
                                     STARTAGE_OVER65 == "<=65" & CHARLSON_WSCORE < 2 ~ "<=65 and <2 conditions",
                                     STARTAGE_OVER65== "Missing" ~ "STARTAGE Missing"),
         STARTAGE_FRAIL = case_when(STARTAGE_OVER65 == ">65" & IMPFRAILTY_SCORE >= 5 ~ ">65 and eFI >=5",
                                     STARTAGE_OVER65 == ">65" & IMPFRAILTY_SCORE < 5 ~ ">65 and eFI < 5",
                                     STARTAGE_OVER65 == "<=65" & IMPFRAILTY_SCORE >= 5 ~ "<=65 and eFI >=5",
                                     STARTAGE_OVER65 == "<=65" & IMPFRAILTY_SCORE < 5 ~ "<=65 and eFI <5",
                                     STARTAGE_OVER65== "Missing" ~ "STARTAGE Missing"),
         STARTAGE_SDC = ifelse(STARTAGE < 100, STARTAGE, "100+"),
         STARTAGE_SDC = factor(STARTAGE_SDC, levels = c(paste(1:99), '100+')),
         STARTAGE_SDC = fct_explicit_na(STARTAGE_SDC, na_level = "Missing")) %>% 
  mutate_at(vars(ADMIDATE_FILLED, DISDATE, EPISTART, EPIEND), ~as.Date(., "%Y-%m-%d"))

saveRDS(FAEs, str_c(processed_data_path, 'FAEs_2019.Rds'))

# Admissions by trust  ----------------------------------------------------

FAEs_by_trust <- FAEs %>% 
  group_by(PROCODE3, EPISTART) %>% 
  summarise(admissions = n()) %>% 
  ungroup %>% 
  complete(PROCODE3, EPISTART, fill = list(admissions = 0))

FAEs_by_trust_summary <- FAEs_by_trust %>% 
  group_by(PROCODE3) %>% 
  summarise(admission_daily_mean = round(mean(admissions), 1),
            admissions_daily_max = max(admissions),
            n_days = n()) %>% 
  mutate(pct_increase = round(100*admissions_daily_max/admission_daily_mean - 100, 1))

write_csv(FAEs_by_trust_summary, str_c(results_path, 'Trust_summaries_2019.csv'))

mean_max_pct_increase_summary <- FAEs_by_trust_summary %>% 
  summarise(min_pct_increase = round(min(pct_increase),1),
            mean_pct_increase = round(mean(pct_increase),1),
            sd_pct_increase = round(sd(pct_increase),1),
            max_pct_increase = round(max(pct_increase),1),
            n_trusts = n())

write_csv(mean_max_pct_increase_summary, str_c(results_path, 'Trust_summaries_2019_pct_increase.csv'))


# Summary stats on all admissions -----------------------------------------

## Mean and standard deviations
adm_summary <- FAEs %>% 
  select(IMPFRAILTY_SCORE, IMPFRAILTY_NORM_SCORE, CHARLSON_WSCORE, ELIXHAUSER_WSCORE_AHRQ,
         ELIXHAUSER_WSCORE_VW) %>% 
  pivot_longer(everything(), names_to = "score", values_to = "value") %>% 
  group_by(score) %>% 
  summarise(min5mean = min_five_mean(value), 
            mean = round(mean(value), 1),
            sd = round(sd(value), 1),
            max5mean = max_five_mean(value),
            n = n())
  
write_csv(adm_summary, str_c(results_path, 'Summaries_2019_mean_sd.csv'))

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
  write_csv(., str_c(results_path, 'Summaries_2019_STARTAGE_BANDS_WIDE_mean_sd.csv'))



## One-way frequency tables
vars_to_summarise_1D <- c("IMPFRAILTY_SCORE", "IMPFRAILTY_NORM_SCORE", "CHARLSON_WSCORE", 
                       "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW",  
                       "STARTAGE_BANDS_WIDE", "SEX_FCT", "ADMITYPE", "STARTAGE_SDC")

walk(vars_to_summarise_1D, 
     write_frequency_table, data = FAEs, path = str_c(results_path, "Summaries_2019"))

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
ggsave(str_c(results_path, 'Summaries_2019_STARTAGE_SDC_histogram.png'), age_histogram,
       width = 16, height = 4)

## Two-way frequency tables

vars_to_summarise_2D <- c("IMPFRAILTY_SCORE", "IMPFRAILTY_NORM_SCORE", "CHARLSON_WSCORE", 
                          "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW")

# Scores vs. wide age bands
walk(vars_to_summarise_2D, write_2x2frequency_tables, data = FAEs, 
     path = str_c(results_path, "Summaries_2019"), col1 = "STARTAGE_BANDS_WIDE")

# Scores vs. frailty
walk(c("CHARLSON_WSCORE", "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW"), 
     write_2x2frequency_tables, data = FAEs, 
     path = str_c(results_path, "Summaries_2019"), col1 = "IMPFRAILTY_SCORE")

# Age vs. sex
write_2x2frequency_tables(data = FAEs, path = str_c(results_path, "Summaries_2019"), 
                          col1 = "STARTAGE_BANDS_WIDE", col2 = "SEX_FCT")

# Age (under/over 65) vs. Charlson score
# exclude patients under 18

write_2x2frequency_tables(data = FAEs[FAEs$STARTAGE_Under18 == '18+',], path = str_c(results_path, "Summaries_2019_over18s"), 
                          col1 = "STARTAGE_OVER65", col2 = "CHARLSON_WSCORE")



# Admission counts by ADMITYPE and additional vars ----------------------------------------

# All patients
walk(c("STARTAGE_BANDS_NARROW", "STARTAGE_BANDS_WIDE"), 
     create_admissions_summary, data = FAEs, path = str_c(results_path, "Admissions_2019_") )

# Excluding patients under 18 and patients with missing age
walk(c("IMPFRAILTY_SCORE", "IMPFRAILTY_NORM_SCORE", "IMPFRAILTY_SCORE_TERTILE",
       "CHARLSON_WSCORE", "ELIXHAUSER_WSCORE_AHRQ", "ELIXHAUSER_WSCORE_VW",
       "STARTAGE_CHARLSON", "STARTAGE_FRAIL"), 
     create_admissions_summary, data = FAEs[FAEs$STARTAGE_Under18 == '18+',], 
     path = str_c(results_path, "Admissions_2019_"))

# Patients over 65 only

create_admissions_summary(data = FAEs[FAEs$STARTAGE_OVER65 == '>65',], var = "CHARLSON_WSCORE",
                          path = str_c(results_path, "Over65s_admissions_2019_"))

# Disconnect db -----------------------------------------------------------

dbDisconnect(db)
