install.packages("formattable")


library(ggplot2)
library(formattable)
library(tidyverse)

before_study<-c(34, 76, 76, 63, 73, 75, 67, 78, 81, 53, 58, 81, 77, 80, 43, 65, 76, 63, 54, 64, 85, 54, 70, 71, 71, 55, 40, 78, 76, 100, 51, 93, 64, 42, 63, 61, 82, 67, 98, 59, 63, 84, 50, 67, 80, 83, 66, 86, 57, 48)
after_study <- c (74, 87, 89, 98, 65, 82, 70, 70, 70, 84, 56, 76, 72, 69, 73, 61, 83, 82, 89, 75, 48, 72, 80, 66, 82, 71, 49, 54, 70, 65, 74, 63, 65, 101, 82, 75, 62, 83, 90, 76, 87, 90, 78, 63, 59, 79, 74, 65, 77, 74)

#boxplot(before_study, after_study, names = c("수강 전", "수강 후),
#       col=c("blue", "red"), main="수강전후 성적비교", xlab= "Status", ylab= "Score")
study_compare<-data.frame(before_study, after_study)
study_compare<-cbind("No"=rownames(study_compare), study_compare)

##tidy gather
study_compare_2 <- gather(study_compare, 'before_study', 'after_study',
                          key="compare", value="score")
study_compare_2$No<-study_compare_2$No %>% as.character() %>% as.numeric()

study_compare_2$compare<-factor(study_compare_2$compare, levels= c("before_study","after_study"))

#ggplot boxplot
ggplot(study_compare_2, aes(x=compare, y=score, fill=compare)) + geom_boxplot()

#ggplot line
ggplot(study_compare_2, aes(x=No, y=score, color=compare)) + geom_line()

#증감표 테이플

study_compare %>% 
  head(5) %>% 
  mutate(Difference = after_study - before_study) %>%
  mutate(Change = case_when(
    Difference > 0 ~ "증가",
    Difference < 0 ~ "감소",
    TRUE ~ "변동없음"
  )) %>%
  formattable(., list(
    Difference = formatter("span", style = x ~ ifelse(x > 0,
                                                      style(color = "Green", font.weight = "bold"), ifelse(x < 0,
                                                                                                           style(color = "red"),NA))),
    Change = formatter("span", style = x ~ ifelse(x == "증가",
                                                  style(color = "Green", font.weight = "bold"), ifelse(x == "감소",
                                                                                                       style(color = "red"),NA)))
  )
  )

var_test_vector1 <- c(75,67,78,81,53,71,71,55,40,78,76,42,67,98,59,63,84,50,67,80,83)
var_test_vector2 <- c(58,81,77,80,76,63,54,64,85,54,70,71,71,55,40,78,76,100,51,42,63,61,82,57,48)


var_test_data <- var.test(var_test_vector1,var_test_vector2)


t.test(before_study, after_study, paired = TRUE)

t.test(before_study, after_study, paired = TRUE, alternative = "less")

t.test(before_study, after_study, paired = TRUE, alternative = "greater")


study_compare %>% 
  mutate(Difference = after_study - before_study) %>%
  mutate(Change = case_when(
    Difference > 0 ~ "증가",
    Difference < 0 ~ "감소",
    TRUE ~ "변동없음"
  )) %>%
  formattable(., list(
    Difference = formatter("span", style = x ~ ifelse(x > 0,
                                                      style(color = "Green", font.weight = "bold"), ifelse(x < 0,
                                                                                                           style(color = "red"),NA))),
    Change = formatter("span", style = x ~ ifelse(x == "증가",
                                                  style(color = "Green", font.weight = "bold"), ifelse(x == "감소",
                                                                                                       style(color = "red"),NA)))
  )
  )
