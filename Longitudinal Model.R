# Longitudinal models

welllong <- lm(sclfsat1 ~ factor(year), d = dcontI)
summary(welllong)

sexlong <- lm(sclfsat1 ~ as.factor(year) * sex, data = dcontI)
summary(sexlong)

edlong <- lm(sclfsat1 ~ as.factor(year) * factor(qfhigh), data = dcontI)
summary(edlong)

agelong <- plm(sclfsat1 ~ dvage, data = dcontI, model = "within", index = c("pidp", "year"), effect = "twoways")
summary(agelong)

inlong <- plm(sclfsat1 ~ fimnnet, data = dcontI, model = "within", index = c("pidp", "year"), effect = "twoways")
summary(inlong)



```{r longitudinal model, cache = TRUE, message = FALSE, warning = FALSE}
library(plm)

long <- plm(sclfsat1 ~ fimnnet + factor(qfhigh) + dvage + factor(sex), data = dcontI, model = "within", index = c("pidp", "year"), effect = "twoways")

summary(long)
```