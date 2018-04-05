# Used for visualising

# Opening

dI <- fread('myData/dI.tab')

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

ggplot(Wc, aes(x = wave, fill = sclfsat1)) +
  geom_bar(position = 'fill') +
  coord_polar(theta = 'y')

dcontI %>%
  group_by(year) %>%
  summarise(
    meanWb = mean(sclfsat1)
  ) %>%
  ggplot(aes(x = year, y = meanWb)) +
  geom_point() +
  geom_line()

dcontI %>%
  group_by(year, sex) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = sex, linetype = sex, shape = sex)) +
  geom_point() +
  geom_line()

f1 <- subset(dcontI, sex == 'female')

m1 <- subset(dcontI, sex == 'male')

ggplot(f1, aes(x = wave, y = sclfsat1, fill = wave)) +
  geom_violin()

ggplot(m1, aes(x = wave, y = sclfsat1, fill = wave)) +
  geom_violin(

grid.newpage()

pushViewport(viewport(layout = grid.layout(, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1))
print(b, vp = vplayout(1, 2))

table(f$sclfsat1, f$wave)

dcontI %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line()

c <- f %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line()

d <- m %>%
  group_by(year, qfhigh) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh)) +
  geom_point() +
  geom_line() +
  coord_cartesian(ylim = c(4.25, 5.25))

grid.newpage()

pushViewport(viewport(layout = grid.layout(, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(c, vp = vplayout(1, 1))
print(d, vp = vplayout(1, 2))

dcontI %>%
  group_by(year, qfhigh, sex) %>%
  summarise(
    meanWb = mean(sclfsat1, na.rm = TRUE)
  ) %>%
  ggplot(aes(x = year, y = meanWb, colour = qfhigh, linetype = sex, shape = sex)) +
  geom_point() +
  geom_line()

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