# data3essay

##### Packages #####
library(tidyverse)
library(data.table)
library(readr)
library(readxl)

##### github #####
# https://github.com/mas254/d3e

##### Loading data #####
w1d <- read_tsv("data/UKDA-6614-tab/tab/us_w1/a_indresp.tab")
w2d <- read_tsv("data/UKDA-6614-tab/tab/us_w2/b_indresp.tab")

##### Variable names #####
# pidp - participant number
# a_sex - sex
# a_racel - ethnicity
# a_ukborn - born in UK
# a_plbornc - country of birth
# a_dvage - age
# a_qfhigh - highest qualification
# a_fenow - in further education
# a_fednt - reason no further education
# a_jbstat - job status
# a_jspayu - average income
# a_jspytx - income figure given before or after tax
# a_paedqf - father's qualifications
# a_maedqf - mother's qualifications

##### Selecting variable #####
# Wave 1
w1 <- w1d %>%
  select(pidp, a_sex, a_racel, a_ukborn, a_plbornc, a_dvage, a_qfhigh, a_fenow, a_fednt, a_jbstat, a_jspayu, a_jspytx, a_paedqf, a_maedqf)
rm(w1d)

# Wave 2
w2d <- w2 %>%
  select(pidp, b_sex, b_racel, b_ukborn, b_plbornc, b_dvage, b_qfhigh, b_fenow, b_fednt, b_jbstat, b_jspayu, b_jspytx, b_paedqf, b_maedqf)
rm(w2d)
