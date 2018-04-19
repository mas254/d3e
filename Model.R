# Model

##### Setup #####

library(tidyverse)
library(plm)
library(readr)

# Reads data
library(data.table)

dI <- fread('myData/dI.tab')

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
  mutate(racel = ifelse(racel < 96, racel, NA)) %>%
  mutate(racelo = ifelse(racelo > 0, racelo, NA)) %>%
  mutate(racelo = ifelse(racelo < 96, racelo, NA))

dnaI <- subset(dnaI, , -c(racel, racelo, fimngrs))

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
  mutate(sex = factor(sex)) %>% 
  mutate(year = recode(wave, "a" = "2009",
                       "b" = "2010",
                       "c" = "2011",
                       "d" = "2012",
                       "e" = "2013",
                       "f" = "2014",
                       "g" = "2015")) %>%
  mutate(year = as.numeric(year))

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

rm(dI, dmeltI, dnaI)

##### Recoding for model #####
W7 <- subset(dcontI, wave == 'g',
             select = c(1:8))

##### Model #####

# Add visualisation for average wellbeing over age - also for income - just final wave
W7 %>%
  ggplot(aes(x = dvage, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

W7p <- subset(W7, dvage < 65,
            select = c(1:8))

W7p %>%
  ggplot(aes(x = dvage, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

W7 %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

W7r <- subset(W7, fimnnet > 0,
              select = c(1:8))

W7r %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

W7r1 <- subset(W7, fimnnet > 800,
              select = c(1:8))

W7r1 %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

6.7*40*45*.8

quantile(dcontI$fimnnet[dcontI$fimnnet < 1000], seq(0, 1, 0.2))

s <- dcontI %>% 
  filter(fimnnet > 1000) %>% 
  mutate(income = ifelse(fimnnet >= 1000.002 & fimnnet <= 1277.346, 20,
                         ifelse(fimnnet > 1277.346 & fimnnet <= 1581.000, 40,
                                ifelse(fimnnet > 1581.000 & fimnnet <= 1961.228, 60,
                                       ifelse(fimnnet > 1961.228 & fimnnet <= 2564.095, 80,
                                              ifelse(fimnnet > 2564.095 & fimnnet <= 15000.000, 100, NA)))))) %>% 
  mutate(income = as.factor(income))

s %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

s %>%
  group_by(year, income, sex) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = income, linetype = sex)) +
  geom_point() +
  geom_line()

z <- dcontI %>%
  filter(fimnnet < 1000)

z %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth()

# Most recent year

m1 <- lm(sclfsat1 ~ factor(qfhigh) + fimnnet + dvage + factor(sex), W7)
summary(m1)

# Model in final wave

me <- lm(sclfsat1 ~ factor(qfhigh) + factor(sex) + dvage, W7)
summary(me)

mi <- lm(sclfsat1 ~ fimnnet + factor(sex) + dvage, W7)
summary(mi)
install.packages("stargazer")
library(stargazer)
stargazer(me, mi, type = 'html')
stargazer(attitude)
# Longitudinal effects

m <- plm(sclfsat1 ~ fimnnet + factor(qfhigh) + dvage + factor(sex), data = dcontI, model = "within", index = c("pidp", "year"),
          effect = "twoways")
summary(m)
# 
# Twoways effects Within Model
# 
# Call:
#   plm(formula = sclfsat1 ~ fimnnet + factor(qfhigh) + dvage + factor(sex), 
#       data = dcontI, effect = "twoways", model = "within", index = c("pidp", 
#                                                                      "year"))
# 
# Unbalanced Panel: n = 46963, T = 1-7, N = 177626
# 
# Residuals:
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -5.374663 -0.484193  0.022765  0.670766  5.043696 
# 
# Coefficients:
#   Estimate  Std. Error t-value Pr(>|t|)  
# fimnnet                            2.1370e-06  4.1451e-06  0.5156  0.60616  
# factor(qfhigh)2-Degree/equiv       6.0078e-02  5.6452e-02  1.0642  0.28722  
# factor(qfhigh)3-Higher ed          5.2794e-02  6.8329e-02  0.7726  0.43974  
# factor(qfhigh)4-A level/equiv     -1.9952e-02  5.7700e-02 -0.3458  0.72950  
# factor(qfhigh)5-AS level/equiv     3.9335e-02  6.5372e-02  0.6017  0.54737  
# factor(qfhigh)6-GCSE/equiv         1.0535e-01  6.2251e-02  1.6924  0.09057 .
# factor(qfhigh)7-CSE                1.3995e-01  1.6027e-01  0.8733  0.38252  
# factor(qfhigh)8-Other school cert -3.2499e-01  2.6031e-01 -1.2484  0.21187  
# dvage                             -1.3639e-02  1.2042e-02 -1.1326  0.25740  
# factor(sex)male                    4.4054e-01  2.8797e-01  1.5298  0.12607  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    239230
# Residual Sum of Squares: 239200
# R-Squared:      0.00013513
# Adj. R-Squared: -0.3594
# F-statistic: 1.76567 on 10 and 130647 DF, p-value: 0.06105
# stargazer(m, omit = c("pidp", "year"), type = "html")

# This means there is no change over time for these variables and their effect on wellbeing - this is good for us as it means their effects on individual
# wellbeing are constant