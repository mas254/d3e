# Clean Project

##### Packages ######
library(tidyverse)
library(data.table)
library(readr)
library(readxl)
require(reshape2)
library(plyr)
library("fmsb")
library(arm)

##### All Variables - Data Cleaning #####

# Firstly, we create a character vector with the full paths to the individual indresp files from all the waves.
files <- dir("data/UKDA-6614-tab/tab",
             pattern="indresp",
             recursive = TRUE,
             full.names=TRUE)

# This includes files from the BHPS that we don't need so we must drop them. We can use the function
# str_detect() to identify files from the Understanding Society only and then keep only
# their names in the vector "files".
files <- files[str_detect(files, "us")]

# Selecting variables
varsI <- c('pidp', 'qfhigh_dv', 'fimnnet_dv', 'fimngrs_dv', 'sclfsat1', 'dvage', 'sex', 'racel', 'racelo_code')

# pidp - ID number; qfhigh - highest qualification; fimnnet - monthly net personal income; sclfsat1 - wellbeing; dvage - age; sex - sex; racel - race

# Create the data file with all 7 waves with the selected variables
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

# Saving this data in myData
write_tsv(dI, "myData/dI.tab")

# Cleaning the data so wave number is a variable and we can see the answers for each variable for each person with ease.
dmeltI <- dI %>%
  melt(id = "pidp") %>%
  separate(variable, into = c("wave", "variable"), sep = "_") %>%
  dcast(pidp + wave ~ variable)

# Changing negative and other 'unknown' values to NA
dnaI <- dmeltI %>%
  mutate(qfhigh = ifelse(qfhigh > 0, qfhigh, NA)) %>%
  mutate(qfhigh = ifelse(qfhigh < 96, qfhigh, NA)) %>%
  mutate(sclfsat1 = ifelse(sclfsat1 > 0, sclfsat1, NA)) %>%
  mutate(dvage = ifelse(dvage > 0, dvage, NA)) %>%
  mutate(sex = ifelse(sex > 0, sex, NA)) %>%
  mutate(racel = ifelse(racel > 0, racel, NA)) %>%
  mutate(racel = ifelse(racel < 96, racel, NA)) %>%
  mutate(racelo = ifelse(racelo > 0, racelo, NA)) %>%
  mutate(racelo = ifelse(racelo < 96, racelo, NA))


# Subsetting the data frame into individual waves
W1 <- subset(dnaI, wave == 'a',
             select = c(1:10))

W7 <- subset(dnaI, wave == 'g',
             select = c(1:10))

# Checking for variables with larger than average number of NAs
summary(W1)
summary(W7)

# As fimngrs and fimnnet are similar, we will use fimnnet, and as racel and racelo have almost double the NA values of any other variable for W7, we cut them
dnaI$racel <- dnaI$racelo <- NULL
dnaI <- subset(dnaI, , -c(racel, racelo, fimngrs))

# Changing numerical responses to labels (this is to make our descriptive analysis easier to do, as well as making the results clearer).
# We have also combined equivalent qualifications, so we have fewer variables, again making our descriptive analysis easier to do and
# clearer to interpret.
dcleanI <- na.omit(dnaI) %>%
  mutate(qfhigh = recode(qfhigh, "1" = "1-Higher degree", "2" = "2-Degree/equiv", "3" = "3-Higher ed",
                         "4" = "3-Higher ed", "5" = "3-Higher ed", "6" = "3-Higher ed",
                         "7" = "4-A level/equiv", "8" = "4-A level/equiv", "9" = "4-A level/equiv",
                         "10" = "5-AS level/equiv", "11" = "5-AS level/equiv", "12" = "5-AS level/equiv",
                         "13" = "6-GCSE/equiv", "14" = "7-CSE", "15" = "6-GCSE/equiv", "16" = "8-Other school cert")) %>%
  mutate(qfhigh = factor(qfhigh)) %>%
  mutate(sclfsat1 = recode(sclfsat1, "1" = "1-completely dissatisfied", "2" = "2-mostly dissatisfied",
                           "3" = "3-somewhat dissatisfied", "4" = "4-neither satisfied or dissatisfied",
                           "5" = "5-somewhat satisfied", "6" = "6-mostly satisfied", "7" = "7-completely satisfied")) %>%
  mutate(sclfsat1 = factor(sclfsat1)) %>%
  mutate(sex = recode(sex, "1" = "male", "2" = "female")) %>%
  mutate(sex = factor(sex))

# This is for treating the wellbeing score as a continuous variable
# When we do our inferential analysis, we will need to treat "sclfsat1" as a continuous, not a factor variable,
# thus using the numerical, rather than categorical, values.
dcontI <- na.omit(dnaI) %>%
  mutate(qfhigh = recode(qfhigh, "1" = "1-Higher degree", "2" = "2-Degree/equiv", "3" = "3-Higher ed",
                         "4" = "3-Higher ed", "5" = "3-Higher ed", "6" = "3-Higher ed",
                         "7" = "4-A level/equiv", "8" = "4-A level/equiv", "9" = "4-A level/equiv",
                         "10" = "5-AS level/equiv", "11" = "5-AS level/equiv", "12" = "5-AS level/equiv",
                         "13" = "6-GCSE/equiv", "14" = "7-CSE", "15" = "6-GCSE/equiv", "16" = "8-Other school cert")) %>%
  mutate(qfhigh = factor(qfhigh)) %>%
  mutate(sex = recode(sex, "1" = "male", "2" = "female")) %>%
  mutate(sex = factor(sex))

# Removing unnecessary data files. These are taking up memory and will not be needed for the rest of the analysis.
rm(dI, dmeltI, dnaI)

##### Visualising Relationships #####
# First, to explore our data, we will look at the distribution of wellbeing in each wave to see for any overall trends
ggplot(dcleanI, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

# We are going to take a closer look at the first and last waves of the data
W1 <- subset(dcleanI, wave == 'a',
             select = c(qfhigh, sclfsat1))

W7 <- subset(dcleanI, wave == 'a',
             select = c(qfhigh, sclfsat1))

prop.table(table(dcleanI$sclfsat1[dcleanI$wave == 'a']))
prop.table(table(dcleanI$sclfsat1[dcleanI$wave == 'g']))

