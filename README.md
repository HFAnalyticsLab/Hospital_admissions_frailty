# Trends in hospital admissions by frailty and comorbidity burden

#### Project Status: In progress

## Project Description

A descriptive analysis of trends in hospital admissions to NHS acute trusts across frailty index, comorbidity scores and age.

This analysis is performed to support Imperial College London in estimating hospital capacity for admissions for COVID pneumonia. A key question is how much extra capacity could be freed up by implementing the most recent [NICE guidelines on ICU admissions of frail patients](https://www.nice.org.uk/guidance/ng159/chapter/2-Admission-to-critical-care). 

To this end, we are creating historical baseline estimates of the average daily number of hospital admissions, as well as the expected variation between the busiest and the least busy days. These will be stratified by frailty index, comorbidity index and patient age. 

## Data source

We are using Hospital Episodes Statistics (HES) Admitted Patient Care data, covering the time period between 2015 and 2019. The data application for this project has been approved by the NHS Digital [Data Access Request Service (DARS)](https://digital.nhs.uk/services/data-access-request-service-dars).

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Secure Data Environment, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information was used that could directly identify a patient or other individual. 

## How does it work?

As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level HES Admitted Patient Care extracts. However, once [statistical disclosure checks](https://ukdataservice.ac.uk/media/622521/thf_datareport_aw_web.pdf) have been completed, aggregate outputs of this analysis can be made available.

### Requirements

These scripts were written under R version 3.6.2 (2019-12-12) -- "Dark and Stormy Night".
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)(1.2.1)
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html)(0.2.0)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**DBI**](https://cran.r-project.org/web/packages/DBI/index.html)(1.0.0)


Before HES data was used for this analysis, it was cleaned and processed using our in-house [HES pipeline](https://github.com/HFAnalyticsLab/HES_pipeline). The pipeline flags comorbidities for
each hospital episode and calculates the Charlson and Elixhauser comorbidity scores using the 
R package [comorbidity](https://cran.r-project.org/web/packages/comorbidity/vignettes/comorbidityscores.html). 

The HES pipeline also creates a custom electronic frailty index created by Imperial College London (IMPFRAILTY). Additional documentation for this score can be found in doc/frailty_score.md and the R 
code for the score can be found in the HES pipeline repository. This score is based on diagnosis codes from Soong *et al.* but calculated similarly to the electronic Frailty Index (eFI) developed by Clegg *et al.*.


### Getting started

The 'R' folder contains:
* analysis_priority_1.R - descriptive analysis of hospital admissions for the most recent available data (Nov 2018 - Oct 2019)
* [analysis of data from 2015-2019 to be added]


## Useful references

* Soong J, Poots AJ, Scott S, Donald K, Bell D. Developing and validating a risk prediction model for acute care based on frailty syndromes. BMJ Open. 2015;5(10):1-12. doi:10.1136/bmjopen-2015-008457
* Gilbert T, Neuburger J, Kraindler J, et al. Development and validation of a Hospital Frailty Risk Score focusing on older people in acute care settings using electronic hospital records: an observational study. Lancet. 2018;391(10132):1775-1782. doi:10.1016/S0140-6736(18)30668-8
* Clegg A, Bates C, Young J, et al. Development and validation of an electronic frailty index using routine primary care electronic health record data. Age Ageing. 2016;45(3):353-360. doi:10.1093/ageing/afw039

## Authors
* **Fiona Grimm** - [@fiona_grimm](https://twitter.com/fiona_grimm) - [fiona-grimm](https://github.com/fiona-grimm)
* **Pablo Perez**, Imperial College London
* **Dheeya Rizmie**, Imperial College London

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/Hospital_admissions_frailty/blob/master/LICENSE).
