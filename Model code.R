W7 <- subset(dcontI, wave == 'g', select = c(1:8))
age <- lm(sclfsat1 ~ dvage, W7)
summary(age)

W7 %>%
  ggplot(aes(x = dvage, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

W7p <- subset(W7, dvage < 65, select = c(1:8))

agepen <- lm(sclfsat1 ~ dvage, W7p)
summary(agepen)
W7p %>%
  ggplot(aes(x = dvage, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)


income <- lm(sclfsat1 ~ fimnnet, W7)
summary(income)
W7 %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)

W7r1 <- subset(W7, fimnnet > 800, select = c(1:8))
W7r1 %>%
  ggplot(aes(x = fimnnet, y = sclfsat1)) +
  geom_smooth() +
  geom_smooth(method = "lm", colour = "red", se = FALSE)
inc <- lm(sclfsat1 ~ fimnnet, W7r1)
summary(inc)
6.7*40*45*.8

edu <- lm(sclfsat1 ~ factor(qfhigh), W7)
summary(edu)
W7 %>%
  ggplot(aes(x = qfhigh, y = sclfsat1)) +
  geom_bar(stat = "summary", fun.y = "mean")

sex <- lm(sclfsat1 ~ factor(sex), W7)
summary(sex)
W7 %>%
  ggplot(aes(x = sex, y = sclfsat1)) +
  geom_bar(stat = "summary", fun.y = "mean")

all <- lm(sclfsat1 ~ dvage + fimnnet + factor(qfhigh) + factor(sex), W7)
summary(all)



long <- plm(sclfsat1 ~ fimnnet + factor(qfhigh) + dvage + factor(sex), data = dcontI, model = "within", index = c("pidp", "year"), effect = "twoways")
summary(long)