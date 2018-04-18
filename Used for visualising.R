# Used for visualising

library(tidyverse)
library(data.table)
library(readr)
library(readxl)
require(reshape2)
library(plyr)
library("fmsb")
library(arm)

# Opening

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

# Analysis

ggplot(dcleanI, aes(x = year, fill = sclfsat1)) +
  geom_bar(position = 'fill')

Wc <- subset(dcleanI, wave == c('a', 'd', 'g'),
             select = c(1:7))

ggplot(Wc, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

dcontI %>%
  group_by(year) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb)) +
  geom_point() +
  geom_line()

fe <- subset(dcontI, sex == 'female')

m <- subset(dcontI, sex == 'male')

ggplot(dcleanI, aes(x = sex, fill = sclfsat1)) +
  geom_bar(position = 'fill') +
  coord_polar(theta = 'y')

dcontI %>%
  group_by(year, sex) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = sex, linetype = sex, shape = sex)) +
  geom_point() +
  geom_line()

# http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization

a <- ggplot(fe, aes(x = wave, y = sclfsat1, fill = wave)) +
  geom_violin()

b <- ggplot(m, aes(x = wave, y = sclfsat1, fill = wave)) +
  geom_violin()

grid.newpage()

pushViewport(viewport(layout = grid.layout(1, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1))
print(b, vp = vplayout(1, 2))

table(fe$sclfsat1, fe$wave)

table(m$sclfsat1, m$wave)

dcontI %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line()

ggplot(dcleanI, aes(x = qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill')

Wd <- subset(dcleanI, qfhigh == c('1-Higher degree', '4-A level/equiv', '7-CSE'),
             select = c(1:7))

ggplot(Wd, aes(x = qfhigh, fill = sclfsat1)) +
  geom_bar(position = 'fill') +
  coord_polar(theta = 'y')

High <- subset(dcleanI, qfhigh == '1-Higher degree',
               select = c(wave, sclfsat1))

A <- subset(dcleanI, qfhigh == '4-A level/equiv',
            select = c(wave, sclfsat1))

CSE <- subset(dcleanI, qfhigh == '7-CSE',
              select = c(wave, sclfsat1))

c <- ggplot(High, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

d <- ggplot(A, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

e <- ggplot(CSE, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill')

grid.newpage()

pushViewport(viewport(layout = grid.layout(1, 3)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(c, vp = vplayout(1, 1))
print(d, vp = vplayout(1, 2))
print(e, vp = vplayout(1, 3))

f <- fe %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line()

g <- m %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(4.25, 5.25))

library(grid)

grid.newpage()

pushViewport(viewport(layout = grid.layout(, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(f, vp = vplayout(1, 1))
print(g, vp = vplayout(1, 2))

dcontI %>%
  group_by(year, qfhigh, sex) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh, linetype = sex, shape = sex)) +
  geom_point() +
  geom_line()

quantile(dcleanI$fimnnet, seq(0, 1, 0.2))

sc <- dcleanI %>% 
  mutate(income = ifelse(fimnnet >= -17741.3379 & fimnnet <= 518.9808, 20,
                         ifelse(fimnnet > 518.9808 & fimnnet <= 1051.6666, 40,
                                ifelse(fimnnet > 1051.6666 & fimnnet <= 1502.5000, 60,
                                       ifelse(fimnnet > 1502.5000 & fimnnet <= 2150.7295, 80,
                                              ifelse(fimnnet > 2150.7295 & fimnnet <= 15000.0000, 100, NA)))))) %>% 
  mutate(income = as.factor(income))

ggplot(sc, aes(x = income, fill = sclfsat1)) +
  geom_bar(position = 'fill')

table(sc$sclfsat1, sc$income)

quantile(dcontI$fimnnet, seq(0, 1, 0.2))

t <- dcontI %>% 
  mutate(income = ifelse(fimnnet >= -17741.3379 & fimnnet <= 518.9808, 20,
                         ifelse(fimnnet > 518.9808 & fimnnet <= 1051.6666, 40,
                                ifelse(fimnnet > 1051.6666 & fimnnet <= 1502.5000, 60,
                                       ifelse(fimnnet > 1502.5000 & fimnnet <= 2150.7295, 80,
                                              ifelse(fimnnet > 2150.7295 & fimnnet <= 15000.0000, 100, NA)))))) %>% 
  mutate(income = as.factor(income))

t %>%
  group_by(year, income) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = income)) +
  geom_point() +
  geom_line()

quantile(dcontI$fimnnet[dcontI$fimnnet > 0], seq(0, 1, 0.2))

s <- dcontI %>% 
  filter(fimnnet > 0) %>% 
  mutate(income = ifelse(fimnnet >= 1.506607e-04 & fimnnet <= 6.706440e+02, 20,
                         ifelse(fimnnet > 6.706440e+02 & fimnnet <= 1.125058e+03, 40,
                                ifelse(fimnnet > 1.125058e+03 & fimnnet <= 1.567589e+03, 60,
                                       ifelse(fimnnet > 1.567589e+03 & fimnnet <= 2.200000e+03, 80,
                                              ifelse(fimnnet > 2.200000e+03 & fimnnet <= 1.500000e+04, 100, NA)))))) %>% 
  mutate(income = as.factor(income))

s %>%
  group_by(year, income) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = income)) +
  geom_point() +
  geom_line()

y <- subset(s, dvage < '65',
           select = c(1:9))

table(y$dvage)

y1 <- subset(y, dvage!= 100)

table(y1$dvage)

y1 %>%
  group_by(year, income) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = income)) +
  geom_point() +
  geom_line()

y1 %>%
  group_by(year, income, sex) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = income, linetype = sex)) +
  geom_point() +
  geom_line()

y1 %>%
  group_by(year, income, sex, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = income, linetype = sex, shape = qfhigh)) +
  geom_point() +
  geom_line()




av <- data.frame(tapply(dcontI$sclfsat1, dcontI$wave, mean))
Wave <- c('1', '2', '3', '4', '5', '6', '7')
df <- data.frame(av, Wave)
colnames(df)[1] <- 'Wellbeing'
ggplot(d = df, aes(x = Wave, y = Wellbeing)) +
  geom_point()
