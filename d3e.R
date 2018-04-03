# Home copy of R work on d3e

##### All Variables - Data Cleaning #####

# Packages required
library(data.table)
library(readr)
library(tidyverse)

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

# Reading this back in (do not copy)
dI <- fread('myData/dI.tab')

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
  mutate(sex = factor(sex)) %>% 
  mutate(year = recode(wave, "a" = "2009",
                              "b" = "2010",
                              "c" = "2011",
                              "d" = "2012",
                              "e" = "2013",
                              "f" = "2014",
                              "g" = "2015")) %>%
  mutate(year = as.numeric(year))

# Removing unnecessary data files. These are taking up memory and will not be needed for the rest of the analysis.
rm(dI, dmeltI, dnaI, W1, W7)

##### Visualising Relationships #####
# First, to explore our data, we will look at the distribution of wellbeing in each wave to see for any overall trends
ggplot(dcleanI, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

# An easy way to compare the numbers
prop.table(table(dcleanI$sclfsat1, dcleanI$wave))
table(dcleanI$sclfsat1, dcleanI$wave)

# We are going to take a closer look at the first, middle and last waves of the data
# Subsetting the data frame into waves 1, 4 and 7
Wc <- subset(dcleanI, wave == c('a', 'd', 'g'),
             select = c(1:7))

# Visualising the spread of wellbeing in each wave
ggplot(Wc, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

# Showing this in a table for a closer look
signif(prop.table(table(Wc$sclfsat1, Wc$wave)), 2)
table(Wc$sclfsat1, Wc$wave)

# Do average wellbeing score next (make and use data frame).
av <- data.frame(tapply(dcontI$sclfsat1, dcontI$wave, mean))
Wave <- c('1', '2', '3', '4', '5', '6', '7')
df <- data.frame(av, Wave)
colnames(df)[1] <- 'Wellbeing'

ggplot(df, aes(x = Wave, y = Wellbeing)) +
  geom_point() +
  coord_cartesian(ylim = c(4.25, 5.5))

# Looking at trend of wellbeing across waves
dcontI %>%
  group_by(year) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb)) +
  geom_point() +
  geom_line()

# Looking at trend of wellbeing across waves, distributed by sex
dcontI %>%
  group_by(year, sex) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = sex)) +
  geom_point() +
  geom_line()

# Comparing wave 1 against wave 7 - we are going to look at either end of the data, as this is where the most obvious differences should be.

# Tables of numbers of education levels and wellbeing.
table(W1$sclfsat1, W1$qfhigh)
table(W7$sclfsat1, W7$qfhigh)

# Subsetting the waves.
W1 <- subset(dcleanI, wave == 'a',
             select = c(qfhigh, sclfsat1))

W4 <- subset(dcleanI, wave == 'd',
             select = c(qfhigh, sclfsat1))

W7 <- subset(dcleanI, wave == 'g',
             select = c(qfhigh, sclfsat1))

# Looking at differences across time
ggplot(W1, aes(x = qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill') +
  coord_polar()

ggplot(W4, aes(x = qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill') +
  coord_polar()

ggplot(W7, aes(qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill') +
  coord_polar()

# Showing proportions of wellbeing in education levels for these two waves
signif(prop.table(table(W1$qfhigh)), 2)
signif(prop.table(table(W4$qfhigh)), 2)
signif(prop.table(table(W7$qfhigh)), 2)

# Looking at the differences in the two most extreme education levels, and the middle
High <- subset(dcleanI, qfhigh == '1-Higher degree',
               select = c(wave, sclfsat1))

A <- subset(dcleanI, qfhigh == '4-A level/equiv',
            select = c(wave, sclfsat1))

CSE <- subset(dcleanI, qfhigh == '7-CSE',
              select = c(wave, sclfsat1))

ggplot(High, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')
coord_polar()

ggplot(A, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')
coord_polar()

ggplot(CSE, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')
coord_polar()

# Looking at trend of wellbeing across waves, distributed by education
dcontI %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line()

# Subsetting between male and female
f <- subset(dcontI, sex == 'female')
m <- subset(dcontI, sex == 'male')

# Looking at trend of wellbeing across waves, distributed by education of females
f %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line()

# Looking at trend of wellbeing across waves, distributed by education of males
m %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(4.25, 5.25))

# Putting these over one another
dcontI %>%
  group_by(year, qfhigh, sex) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh, linetype = sex, shape = sex)) +
  geom_point() +
  geom_line()

# Analysing by net income
library(broom)

dcontI %>%
  filter(fimnnet > 0) %>%
  nest(-year) %>%
  mutate(Quantiles = map(data, ~ quantile(.$fimnnet_dv,
                                          c(0.01, 0.05, 0.1, 0.5, 0.9, 0.95, 0.99, 0.999)))) %>% 
  ggplot(aes(x = year, y = fimnnet, colour = names)) +
  geom_point(na.rm = TRUE) + 
  geom_line(na.rm = TRUE) +
  ylab("Net monthly income") + 
  xlab("Year")

##### Analysis #####

library(plm)
m <- plm(sclfsat1 ~ factor(qfhigh), data = dcontI, model = "within", index = c("pidp", "year"),
         effect = "twoways")
summary(m)

##### Cut #####

# Checking trends by sex
fem <- subset(dcleanI, sex == 'female')
male <- subset(dcleanI, sex == 'male')

ggplot(fem, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

ggplot(male, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

# Checking averages

f <- subset(dcontI, sex == 'female')
m <- subset(dcontI, sex == 'male')

fav <- data.frame(tapply(f$sclfsat1, f$wave, mean))
mav <- data.frame(tapply(m$sclfsat1, m$wave, mean))
fmdf <- data.frame(fav, mav, Wave)
colnames(fmdf) <- c('Female.Wellbeing', 'Male.Wellbeing', 'Wave')

# Plotting this
ggplot(fmdf) +
  geom_point(aes(x = Wave, y = Female.Wellbeing, colour = 'Female.Wellbeing')) +
  geom_point(aes(x = Wave, y = Male.Wellbeing, colour = 'Male.Wellbeing')) +
  coord_cartesian(ylim = c(4.25, 5.5))

mean(f$sclfsat1[f$year == 2012])
mean(m$sclfsat1[m$year == 2012])

# Then split by education and income (for individual waves).

tapply(dcontI$sclfsat1, dcontI$sex, mean)
signif(tapply(dcontI$sclfsat1, dcontI$dvage, mean), 3)

library(plm)
m <- plm(sclfsat1 ~ factor(qfhigh), data = dcontI, model = "within", index = c("pidp", "year"),
          effect = "twoways")
summary(m)

