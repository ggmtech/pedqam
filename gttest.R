# install.packages("gtsummary")
library(gtsummary)
head(trial)
trial2 <- trial %>% select(trt, age, grade)
trial2 %>% tbl_summary()

trial2 %>% tbl_summary(by = trt) %>% add_p()


#install.packages("reticulate")
