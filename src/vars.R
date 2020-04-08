# Define variables --------------------------------------------------------

age_breaks_narrow <- c(0, seq(5, 75, by = 5), Inf)
age_labels_narrow <- c(' 0-4',
                       str_c(" ", seq(5, 70, by = 5), '-', seq(9, 74, by = 5)), 
                       ' 75+')


age_breaks_wide <- c(0, 5, 15, 25, 45, 60, 80, Inf)
age_labels_wide <- c(' 0-4', ' 5-14', ' 15-24', ' 25-44', ' 45-60', ' 61-80', ' 80+')

cancer_codes <- c("C.*", "D[0|1|2|3|4].*")



