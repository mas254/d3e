# All Variables - Data Cleaning

# Selecting Data
files <- dir("data/UKDA-6614-tab/tab",
             pattern="indresp",
             recursive = TRUE,
             full.names=TRUE)

files <- files[str_detect(files, "us")]

varsI <- c('pidp', 'qfhigh_dv', 'fimnnet_dv', 'sclfsat1', 'dvage', 'sex', 'racel')

# pidp - ID number; qfhigh - highest qualification; fimnnet - monthly net personal income; sclfsat1 - wellbeing; dvage - age; sex - sex; racel - race

for (i in 1:7) {
  varsToSelectI <- paste(letters[i], varsI, sep = "_")
  varsToSelectI <- c("pidp", varsToSelectI)
  dataI <- fread(files[i], select = varsToSelectI)
  if (i == 1) {
    dI <- dataI  
  }
  else {
    dI <- full_join(dI, dataI, by = "pidp")
  }
  rm(dataI)
}

write_tsv(dI, "myData/dI.tab")

dmeltI <- dI %>%
  melt(id = "pidp") %>%
  separate(variable, into = c("wave", "variable"), sep = "_") %>%
  dcast(pidp + wave ~ variable)

dnaI <- dmeltI %>%
  mutate(qfhigh = ifelse(qfhigh > 0, qfhigh, NA)) %>%
  mutate(qfhigh = ifelse(qfhigh < 96, qfhigh, NA)) %>%
  mutate(sclfsat1 = ifelse(sclfsat1 > 0, sclfsat1, NA)) %>%
  mutate(dvage = ifelse(dvage > 0, dvage, NA)) %>%
  mutate(sex = ifelse(sex > 0, sex, NA)) %>%
  mutate(racel = ifelse(racel > 0, racel, NA)) %>%
  mutate(racel = ifelse(racel < 96, racel, NA))

dcontI <- na.omit(dnaI) %>%
  mutate(qfhigh = recode(qfhigh, "1" = "1-Higher degree", "2" = "2-Degree/equiv", "3" = "3-Higher ed",
                         "4" = "3-Higher ed", "5" = "3-Higher ed", "6" = "3-Higher ed",
                         "7" = "4-A level/equiv", "8" = "4-A level/equiv", "9" = "4-A level/equiv",
                         "10" = "5-AS level/equiv", "11" = "5-AS level/equiv", "12" = "5-AS level/equiv",
                         "13" = "6-GCSE/equiv", "14" = "7-CSE", "15" = "6-GCSE/equiv", "16" = "8-Other school cert")) %>%
  mutate(qfhigh = factor(qfhigh))

rm(dI, dmeltI, dnaI)
