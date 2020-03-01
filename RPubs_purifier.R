

library(ggplot2)
library(ggthemes)

purifier_df <- read.csv("purifier.csv")

str(purifier_df)

summary(purifier_df)

ggplot(data=purifier_df) +
  geom_point(aes(x=purifier_df$purifier, y=purifier_df$as_time, color=purifier_df$purifier), size=3) +
  theme_classic() +
  theme(axis.text.x=element_text(size=10)) +
  theme(axis.text.y=element_text(size=10)) +
  xlab("총 정수기 다여 대수(전월)") +
  ylab("A/S 시간(당월)")

cor(purifier_df$purifier, purifier_df$as_time)

ggplot(data=purifier_df) +
  geom_point(aes(x=purifier_df$old_purifier, y=purifier_df$as_time, color=purifier_df$purifier),size=3) +
  theme_classic() +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  xlab("노후 정수기 대여 대수(전월)") +
  ylab("A/S 시간(당월)")

cor(purifier_df$old_purifier, purifier_df$as_time)

str(cars)
lm_result_1 <- lm(formula=dist ~ speed, data=cars)
summary(lm_result_1)

coef(lm_result_1)

confint(lm_result_1)

deviance(lm_result_1)

fitted(lm_result_1)

residuals(lm_result_1)

ggplot(data=cars) + 
  geom_point(aes(cars$speed, cars$dist, color=cars$speed), size=2) + 
  theme_light() +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  xlab("속도") +
  ylab("제동거리")

lm_result_1 <- lm(formula = dist ~ speed, data = cars)

ggplot(data = lm_result_1) +
  geom_point(aes(x=lm_result_1$model$speed, y=lm_result_1$model$dist,  color = lm_result_1$model$speed)) +
  geom_abline(intercept=-17.579095, slope=3.932409) +
  theme_light() +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  xlab("속도") +
  ylab("제동거리")

Resi_Fitt <- ggplot(data = lm_result_1) +
  geom_point(aes( x = lm_result_1$fitted.values, y = lm_result_1$residuals, color = lm_result_1$residuals)) +
  theme_light() +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  xlab("속도") +
  ylab("제동거리")


Theore_Stdliz <- ggplot(data = lm_result_1) +
  geom_point(aes( x = lm_result_1$fitted.values, y = lm_result_1$residuals, color = lm_result_1$residuals)) +
  theme_light() +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10)) +
  xlab("속도") +
  ylab("제동거리")

plot(lm_result_1) #콘솔창에서 엔터 누르기

par(mfrow=c(2,2))

lm_result_1 <- lm(formula = dist ~ speed, data = cars)

speed <- c(50, 60, 70, 80, 90, 100)

df_input <- data.frame(speed)

df_input

predict(lm_result_1, df_input)

predict_dist <- predict(lm_result_1, df_input)

str(predict_dist)

cbind(df_input, predict_dist)

predict_dist <- predict(lm_result_1, df_input, interval = "confidence", level = 0.95)

predict_dist

cbind(df_input, predict_dist)

predict_dist <- predict(lm_result_1, df_input, interval = "prediction", level = 0.95)

cbind(df_input, predict_dist)

summary(purifier_df)

cor(purifier_df$purifier, purifier_df$old_purifier)

cor((purifier_df$purifier - purifier_df$old_purifier), purifier_df$old_purifier)

str(purifier_df)

purifier_df$new_purifier <- purifier_df$purifier - purifier_df$old_purifier

str(purifier_df)

lm_result <- lm(as_time ~ new_purifier + old_purifier, data = purifier_df)

summary(lm_result)

input_predict <- data.frame(new_purifier = 300000, old_purifier = 70000)

predict_as_time <- predict(lm_result, input_predict)

predict_as_time

predict_as_time / (8*20)

predict_as_time <- predict(lm_result, input_predict, interval = "confidence", level = 0.95)

predict_as_time
