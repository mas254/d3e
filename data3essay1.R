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
# a_fedlik - likelihood of entering further education
# a_paedqf - father's qualifications
# a_maedqf - mother's qualifications
# a_jbstat - job status
# a_jspayu - average income
# a_jspytx - income figure given before or after tax

##### Variable levels for sex #####
# Value label information for a_sex
# Value = 1.0	Label = male
# Value = 2.0	Label = female
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -2.0	Label = refused

##### Variable levels for race #####
# Value label information for a_racelo_code
# Value = 1.0	Label = british/english/scottish/welsh/northern irish
# Value = 2.0	Label = irish
# Value = 3.0	Label = gypsy or irish traveller
# Value = 4.0	Label = any other white background
# Value = 5.0	Label = white and black caribbean
# Value = 6.0	Label = white and black african
# Value = 7.0	Label = white and asian
# Value = 8.0	Label = any other mixed background
# Value = 9.0	Label = indian
# Value = 10.0	Label = pakistani
# Value = 11.0	Label = bangladeshi
# Value = 12.0	Label = chinese
# Value = 13.0	Label = any other asian background
# Value = 14.0	Label = caribbean
# Value = 15.0	Label = african
# Value = 16.0	Label = any other black background
# Value = 17.0	Label = arab
# Value = 97.0	Label = not elsewhere codable
# Value = -2.0	Label = refused
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -1.0	Label = don't know

##### Variable levels for birthplace #####
# Value label information for a_ukborn
# Value = 1.0	Label = yes, england
# Value = 2.0	Label = yes, scotland
# Value = 3.0	Label = yes, wales
# Value = 4.0	Label = yes, northern ireland
# Value = 5.0	Label = not born in the uk
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

# Value label information for a_plbornc
# Value = 5.0	Label = republic of ireland
# Value = 6.0	Label = france
# Value = 7.0	Label = germany
# Value = 8.0	Label = italy
# Value = 9.0	Label = spain
# Value = 10.0	Label = poland
# Value = 11.0	Label = cyprus
# Value = 12.0	Label = turkey
# Value = 13.0	Label = australia
# Value = 14.0	Label = new zealand
# Value = 15.0	Label = canada
# Value = 16.0	Label = u.s.a
# Value = 17.0	Label = china/hong kong
# Value = 18.0	Label = india
# Value = 19.0	Label = pakistan
# Value = 20.0	Label = bangladesh
# Value = 21.0	Label = sri lanka
# Value = 22.0	Label = kenya
# Value = 23.0	Label = ghana
# Value = 24.0	Label = nigeria
# Value = 25.0	Label = uganda
# Value = 26.0	Label = south africa
# Value = 27.0	Label = jamaica
# Value = 97.0	Label = other country
# Value = -2.0	Label = refused
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -1.0	Label = don't know

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
#   Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused

# Value label information for a_jspayu
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing

# Value label information for a_jspytx
# Value = 1.0	Label = yes, before tax
# Value = 2.0	Label = no, after tax
# Value = -1.0	Label = don't know
# Value = -9.0	Label = missing
# Value = -8.0	Label = inapplicable
# Value = -7.0	Label = proxy respondent
# Value = -2.0	Label = refused


##### Selecting variables #####
vars <- c('pidp', 'sex', 'racel', 'ukborn', 'plbornc', 'dvage', 'qfhigh', 'fenow', 'fednt', 'fedlik', 'paedqf', 'maedqf', 'jbstat', 'jspayu', 'jspytx')

##### Joining waves #####
# Firstly, we create a character vector with the full paths to the individual indresp files from all the waves.

files <- dir("data/UKDA-6614-tab/tab",
             pattern="indresp",
             recursive = TRUE,
             full.names=TRUE)

# This includes files from the BHPS that we don't need so let us drop them. We can use the function
# **str_detect()** (part of the stringr package) to identify files from the Understanding Society only and then keep only
# their names in the vector **files**.

str_detect(files, "us")

files <- files[str_detect(files, "us")]

# Checking we were successful

files

# Create the data file with all 7 waves with the selected variables

for (i in 1:7) {
  varsToSelect <- paste(letters[i], vars, sep = "_")
  varsToSelect <- c("pidp", varsToSelect)
  data <- fread(files[i], select = varsToSelect)
  if (i == 1) {
    d <- data  
  }
  else {
    d <- full_join(all7, data, by = "pidp")
  }
  rm(data)
}

rm(d)

write_tsv(d, "myData/d.tab")

