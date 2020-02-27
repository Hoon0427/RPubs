

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
