library(mosaic)
doe <- read.csv("../data/heli_doe.csv")
head(doe); tail(doe)

doe <- doe %>%
  mutate(RotorL = if_else(RotorL == 5.5, "-", "+") %>% as.factor(),
         RotorW = if_else(RotorW == 3, "-", "+") %>% as.factor())
head(doe); tail(doe)

doe.lm1 <- lm(Time ~ ., data = doe)
summary(doe.lm1)

doe.lm2 <- lm(Time ~ RotorL + Paper, data = doe)
summary(doe.lm2) %>% coef()

mean(~ Time, data = doe)

mean(Time ~ RotorL, data = doe)

newdata <- data.frame(RotorL = "+", Paper = "Light")
predict(doe.lm2, newdata = newdata)

predict(doe.lm2, newdata = newdata, interval = "prediction")
