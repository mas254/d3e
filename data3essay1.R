# data3essay

##### Folding #####
# alt + cmd + o Collapses all
# shft + alt + cmd + o Opens all
# alt + cmd + l Collapses individual
# shft + alt + cmd + l Opens individual

##### Packages #####
library(tidyverse)
library(data.table)
library(readr)
library(readxl)
require(reshape2)
library(plyr)
library("fmsb")
install.packages("arm")
library(arm)

##### github #####
# https://github.com/mas254/d3e

##### Ideas #####
# Could also segregate data by age to look to see if there was less differentiation between wellbeing and education in the youth
# If there was, this would suggest that structures are of less importance
# If there was still a large contrast, then it suggests levels of cultural capital are still important for wellbeing

# Could also look at parents education levels.

# Do I exclude people still in education?
##### Variable names #####
# pidp - participant number
# a_qfhigh - highest qualification
# a_qfhigh_dv - highest qualification reported

# a_fenow - in further education
# a_edasp - highest level exam like to get b/f leave school
# a_jbstat - job status
# a_paedqf - father's qualifications
# a_maedqf - mother's qualifications

# a_fedlik - likelihood of entering further education
# a_fednt - reason no further education

##### Variable levels for age #####
# Value label information for a_dvage
# Value = -8.0	Label = inapplicable
# Value = -2.0	Label = refused
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing

##### Variable levels for education #####
# Value label information for a_qfhigh
# Value = 1.0	Label = university higher degree (e.g. msc, phd)
# Value = 2.0	Label = 1st degree level inc found'n deg, grad memb prof inst, pgce
# Value = 3.0	Label = diploma in higher education
# Value = 4.0	Label = teaching qualification (excluding pgce)
# Value = 5.0	Label = nursing or other medical qualification not yet mentioned
# Value = 6.0	Label = a level
# Value = 7.0	Label = welsh baccalaureate
# Value = 8.0	Label = international baccalaureate
# Value = 9.0	Label = as level
# Value = 10.0	Label = higher grade/advanced higher (scotland)
# Value = 11.0	Label = certificate of sixth year studies
# Value = 12.0	Label = gcse/o level
# Value = 13.0	Label = cse
# Value = 14.0	Label = standard/ordinary (o) grade / lower (scotland)
# Value = 15.0	Label = other school (inc. school leaving exam cert or matriculation
# Value = 96.0	Label = none of the above
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

# Value label information for a_fenow
# Value = 1.0	Label = write in age
# Value = 2.0	Label = never went to college or university
# Value = 3.0	Label = at college/university
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

# Value label information for a_fednt
# Value = 1.0	Label = having school qualification is enough
# Value = 2.0	Label = have decided on a specific career
# Value = 3.0	Label = want to work and earn money
# Value = 4.0	Label = cost of education too high
# Value = 5.0	Label = depends on grades
# Value = 6.0	Label = not academic enough
# Value = 7.0	Label = just don't want to go
# Value = 8.0	Label = want to travel
# Value = 9.0	Label = undecided about it
# Value = -1.0	Label = don't know
# Value = 97.0	Label = other reason
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

# Value label information for a_fedlik
# Value = 1.0	Label = very likely
# Value = 2.0	Label = fairly likely
# Value = 3.0	Label = not very likely
# Value = 4.0	Label = or not at all likely?
#   Value = 5.0	Label = depends
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

##### Variable levels for parents' education ######
# Value label information for a_paedqf
# Value = 1.0	Label = he did not go to school at all
# Value = 2.0	Label = he left school with no qualifications or certificates
# Value = 3.0	Label = he left school with some qualifications or certificates
# Value = 4.0	Label = he gained post school quals or certs (e.g. city & guilds)
# Value = 5.0	Label = he gained a university degree or higher degree
# Value = 97.0	Label = other
# Value = 98.0	Label = don't know
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

# Value label information for a_maedqf
# Value = 1.0	Label = she did not go to school at all
# Value = 2.0	Label = she left school with no qualifications or certificates
# Value = 3.0	Label = she left school with some qualifications or certificates
# Value = 4.0	Label = she gained post school quals or certs (e.g. city & guilds)
# Value = 5.0	Label = she gained a university degree or higher degree
# Value = 97.0	Label = other
# Value = 98.0	Label = don't know
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

##### Variable levels for job #####
# Value label information for a_jbstat
# Value = 1.0	Label = self employed
# Value = 2.0	Label = in paid employment (full or part-time)
# Value = 3.0	Label = unemployed
# Value = 4.0	Label = retired
# Value = 5.0	Label = on maternity leave
# Value = 6.0	Label = looking after family or home
# Value = 7.0	Label = full-time student
# Value = 8.0	Label = long-term sick or disabled
# Value = 9.0	Label = on a government training scheme
# Value = 10.0	Label = unpaid worker in family business
# Value = 97.0	Label = doing something else
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

##### Joining waves #####
# Firstly, we create a character vector with the full paths to the individual indresp files from all the waves.

files <- dir("data/UKDA-6614-tab/tab",
             pattern="indresp",
             recursive = TRUE,
             full.names=TRUE)

# This includes files from the BHPS that we don't need so we must drop them. We can use the function
# str_detect() to identify files from the Understanding Society only and then keep only
# their names in the vector "files".

files <- files[str_detect(files, "us")]

# Checking we were successful

files

# Selecting variables
vars <- c('pidp', 'qfhigh_dv', 'sclfsat1')

# Create the data file with all 7 waves with the selected variables
for (i in 1:7) {
  varsToSelect <- paste(letters[i], vars, sep = "_")
  varsToSelect <- c("pidp", varsToSelect)
  data <- fread(files[i], select = varsToSelect)
  if (i == 1) {
    d <- data  
  }
  else {
    d <- full_join(d, data, by = "pidp")
  }
  rm(data)
}

# Saving this data in myData
write_tsv(d, "myData/d.tab")
##### Cleaning data #####
# Cleaning the data so wave number is a variable and we can see the answers for each variable for each person with ease.

dmelt <- d %>%
  melt(id = "pidp") %>%
  separate(variable, into = c("wave", "variable"), sep = "_") %>%
  dcast(pidp + wave ~ variable)

# Changing negative and other 'unknown' values to NA
dna <- dmelt %>%
  mutate(qfhigh = ifelse(qfhigh > 0, qfhigh, NA)) %>%
  mutate(qfhigh = ifelse(qfhigh < 96, qfhigh, NA)) %>%
  mutate(sclfsat1 = ifelse(sclfsat1 > 0, sclfsat1, NA))

# Changing numerical responses to labels (this is to make our descriptive analysis easier to do, as well as making the results clearer).
# We have also combined equivalent qualifications, so we have fewer variables, again making our descriptive analysis easier to do and
# clearer to interpret.
dclean <- na.omit(dna) %>%
  mutate(qfhigh = recode(qfhigh, "1" = "1-Higher degree", "2" = "2-Degree/equiv", "3" = "3-Higher ed",
                         "4" = "3-Higher ed", "5" = "3-Higher ed", "6" = "3-Higher ed",
                         "7" = "4-A level/equiv", "8" = "4-A level/equiv", "9" = "4-A level/equiv",
                         "10" = "5-AS level/equiv", "11" = "5-AS level/equiv", "12" = "5-AS level/equiv",
                         "13" = "6-GCSE/equiv", "14" = "7-CSE", "15" = "6-GCSE/equiv", "16" = "8-Other school cert")) %>%
  mutate(qfhigh = factor(qfhigh)) %>%
  mutate(sclfsat1 = recode(sclfsat1, "1" = "1-completely dissatisfied", "2" = "2-mostly dissatisfied",
                         "3" = "3-somewhat dissatisfied", "4" = "4-neither satisfied or dissatisfied",
                         "5" = "5-somewhat satisfied", "6" = "6-mostly satisfied", "7" = "7-completely satisfied")) %>%
  mutate(sclfsat1 = factor(sclfsat1))


# This is for treating the wellbeing score as a continuous variable
# When we do our inferential analysis, we will need to treat "sclfsat1" as a continuous, not a factor variable,
# thus using the numerical, rather than categorical, values. I THINK, MUST CHECK THIS
dcont <- na.omit(dna) %>%
  mutate(qfhigh = recode(qfhigh, "1" = "1-Higher degree", "2" = "2-Degree/equiv", "3" = "3-Higher ed",
                         "4" = "3-Higher ed", "5" = "3-Higher ed", "6" = "3-Higher ed",
                         "7" = "4-A level/equiv", "8" = "4-A level/equiv", "9" = "4-A level/equiv",
                         "10" = "5-AS level/equiv", "11" = "5-AS level/equiv", "12" = "5-AS level/equiv",
                         "13" = "6-GCSE/equiv", "14" = "7-CSE", "15" = "6-GCSE/equiv", "16" = "8-Other school cert")) %>%
  mutate(qfhigh = factor(qfhigh))

# Why does this not work?
dclean <- dna %>%
  mutate(qfhigh = recode(qfhigh, "1" = "Higher degree", "2" = "1st degree or equivalent", c("3", "4", "5", "6") = "Higher qualification",
                         c("7", "8", "9") = "A level or equivalent", c("10", "11", "12") = "AS Level or equivalent",
                         c("13", "15") = "GCSE or equivalent", "14" = "CSE", "16" = "Other school cert")) %>%
  mutate(qfhigh = factor(qfhigh)) %>%
  mutate(sclfsat1 = recode(sclfsat1, "1" = "completely dissatisfied", "2" = "mostly dissatisfied",
                           "3" = "somewhat dissatisfied", "4" = "neither satisfied or dissatisfied",
                           "5" = "somewhat satisfied", "6" = "mostly satisfied", "7" = "completely satisfied")) %>%
  mutate(sclfsat1 = factor(sclfsat1))

# Removing unnecessary data files. These are taking up memory and will not be needed for the rest of the analysis.
rm(d, dmelt, dna)


##### Visualising relationships #####
# First, to explore our data, we will look at the distribution of wellbeing in each wave to see for any overall trends NOT SURE TO KEEP THIS
ggplot(dclean, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

# Comparing wave 1 against wave 7 - we are going to look at either end of the data, as this is where the most obvious differences should be.

# Tables of numbers of education levels and wellbeing.
table(W1$sclfsat1, W1$qfhigh)
table(W7$sclfsat1, W7$qfhigh)

# Subsetting the waves.
W1 <- subset(dclean, wave == 'a',
                  select = c(qfhigh, sclfsat1))

W7 <- subset(dclean, wave == 'g',
             select = c(qfhigh, sclfsat1))

# Looking at differences across time
ggplot(W1, aes(x = qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill')
  coord_polar()

ggplot(W7, aes(qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill')
  coord_polar()

# Showing proportions of wellbeing in education levels for these two waves
prop.table(table(W1$qfhigh))
prop.table(table(W7$qfhigh))

# Looking at the differences in the two most extreme education levels
High <- subset(dclean, qfhigh == '1-Higher degree',
                     select = c(wave, sclfsat1))

CSE <- subset(dclean, qfhigh == '7-CSE',
                     select = c(wave, sclfsat1))

ggplot(High, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')
  coord_polar()

ggplot(CSE, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')
  coord_polar()

# Make a data frame like excel graph
a <- data.frame(tapply(W1$sclfsat1, W1$qfhigh, mean), b, c, e, f, g, h)
b <- data.frame(tapply(W2$sclfsat1, W2$qfhigh, mean))
c<- data.frame(tapply(W3$sclfsat1, W3$qfhigh, mean))
e<-data.frame(tapply(W4$sclfsat1, W4$qfhigh, mean))
f<-data.frame(tapply(W5$sclfsat1, W5$qfhigh, mean))
g<-data.frame(tapply(W6$sclfsat1, W6$qfhigh, mean))
h<-data.frame(tapply(W7$sclfsat1, W7$qfhigh, mean))
B$wave<-1
names(B)[1] <- "Wave"
row.names(B) <- "Wave"
B <- t(a)
for(g in dcont){
  dcont$wave
  print(tapply(dcont$sclfsat1, dcont$qfhigh, mean)) 
}

tapply(dcont$sclfsat1, dcont$qfhigh, mean)
# Making a line-graph for changes in average wellbeing in education categories over the waves
ggplot(line, aes(x = Wave, y = Wellbeing, group = Education, colour = Education)) +
  geom_line()

# Remove 'other' qualification level from dataset

##### Analysis #####
w1 <- subset(dcont, wave == 'a',
             select = c(qfhigh, sclfsat1))

w7 <- subset(dcont, wave == 'g',
             select = c(qfhigh, sclfsat1))

model <- glm(sclfsat1 ~ factor(qfhigh), data = w1)
model2 <- glm(sclfsat1 ~ factor(qfhigh), data = w7)
summary(model2)

invlogit(predict(model, data.frame(qfhigh)))
invlogit(5.17639)

# https://stats.idre.ucla.edu/other/mult-pkg/whatstat/
# Could show predicted for each education level? Probs not.