#Install ucidata package from github 
#devtools::install_github("coatless/ucidata")
library("tidyverse"); packageVersion("tidyverse")
library("rms"); packageVersion("rms")
library("cowplot"); packageVersion("cowplot")
#devtools::install_github("coatless/ucidata")
library("ucidata"); packageVersion("ucidata")


data(wine)  #Load wine data

#Recode outcome
mydata <- wine %>%
  dplyr::mutate(red_wine = ifelse(color == "Red", 1, 0)) %>%
  dplyr::select(-color) 

Hmisc::describe(mydata)

d <- Hmisc::describe(mydata)
html(d, size = 80, scroll = TRUE)

p <- plot(d)
p$Continuous

# Hmisc::summaryM() summarizes variables listed in an S formula, computing descriptive statistics and diffs.
# 
Hmisc::summaryM(fixed_acidity + volatile_acidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide+ density + pH + sulphates + alcohol + quality  ~ red_wine, data = mydata, overall = TRUE, test = TRUE, continuous = 5)

s <- Hmisc::summaryM(fixed_acidity + volatile_acidity + citric_acid + residual_sugar + chlorides + free_sulfur_dioxide + total_sulfur_dioxide+ density + pH + sulphates + alcohol + quality  ~ red_wine, data = mydata, overall = TRUE, test = TRUE, continuous = 5)

html(s, caption='Predictors according to wine type',
     exclude1 = TRUE, npct = 'both', digits = 2,
     prmsd = TRUE, brmsd = TRUE, msdsize = mu$smaller2)


dd <- datadist(mydata)
options(datadist = "dd")

a <- ggplot(mydata, aes(x = alcohol, y = red_wine)) +
  Hmisc::histSpikeg(red_wine ~ alcohol, lowess = TRUE, data = mydata) +
  labs(x = "\nAlcohol Content", y = "Probability(Red Wine)\n")

b <- ggplot(mydata, aes(x = citric_acid, y = red_wine)) +
  Hmisc::histSpikeg(red_wine ~ citric_acid, lowess = TRUE, data = mydata) +
  labs(x = "\nCitric Acid", y = "Probability(Red Wine)\n")

c <- ggplot(mydata, aes(x = sulphates, y = red_wine)) +
  Hmisc::histSpikeg(red_wine ~ sulphates, lowess = TRUE, data = mydata) +
  labs(x = "\nSulphates", y = "Probability(Red Wine)\n")

cowplot::plot_grid(a, b, c,  nrow = 1, ncol = 3, scale = .9, labels = "AUTO")


# Visualizing  missing data with Hmisc::naplot and Hmisc::naclus
missing_df <- mydata %>%
                dplyr::mutate(
                fixed_acidity = ifelse(row_number() %in% c(1:100), NA, fixed_acidity),
                volatile_acidity = ifelse(row_number() %in% c(1:200), NA, volatile_acidity),
                citric_acid = ifelse(row_number() %in% c(50:400), NA, citric_acid),
                residual_sugar = ifelse(row_number() %in% c(1000:1050), NA, residual_sugar),
                chlorides = ifelse(row_number() %in% c(1000:1100), NA, chlorides)
                )



Hmisc::naclus(missing_df)
par(mfrow = c(1,1)) # was c(1,2)
na_patterns <- Hmisc::naclus(missing_df)
Hmisc::naplot(na_patterns, 'na per var')
plot(na_patterns)

# incorporate non-linear terms and interactions into a generalized linear model framework complexity  
# 
# y ~ a:b, : indicates the interaction of a and b
# y ~ a*b, equivalent to y ~ a+b+a:b
# y ~ (a+b)^2, equivalent to y ~ (a+b)*(a+b)
# and  restricted interaction term %ia% that for non-linear predictors is not doubly nonlinear 

lrm(red_wine ~ rcs(fixed_acidity, 4) + rcs(volatile_acidity, 4) + rcs(citric_acid, 4) + rcs(residual_sugar, 4) + rcs(chlorides, 4) + rcs(free_sulfur_dioxide, 4) + rcs(total_sulfur_dioxide, 4) + rcs(density, 4) + rcs(pH, 4) + rcs(sulphates, 4) + rcs(alcohol, 4) + rcs(quality, 3),
  data = mydata, x = TRUE, y = TRUE)

m0 <- lrm(red_wine ~ rcs(fixed_acidity, 4) + rcs(volatile_acidity, 4) + rcs(citric_acid, 4) + rcs(residual_sugar, 4) + rcs(chlorides, 4) + rcs(free_sulfur_dioxide, 4) + rcs(total_sulfur_dioxide, 4) + rcs(density, 4) + rcs(pH, 4) + rcs(sulphates, 4) + rcs(alcohol, 4) + rcs(quality, 3),
          data = mydata, x = TRUE, y = TRUE)
print(m0, coef = FALSE)
plot(anova(m0))


lrm(red_wine ~ rcs(pH, 3) + rcs(sulphates, 3), data = mydata, x = TRUE, y = TRUE)
print(m2)
m2 <- lrm(red_wine ~ rcs(pH, 3) + rcs(sulphates, 3), data = mydata, x = TRUE, y = TRUE)
print(m2)
anova(m2)
p1 <- ggplot(Predict(m2, pH))
p2 <- ggplot(Predict(m2, sulphates))
p3 <- ggplot(Predict(m2, pH, fun = plogis))
p4 <- ggplot(Predict(m2, sulphates, fun = plogis))
cowplot::plot_grid(p1, p2, p3, p4, nrow = 2, ncol = 2, scale = .9)


lrm(red_wine ~ (rcs(pH, 3) + rcs(sulphates, 3))^2, data = mydata, x = TRUE, y = TRUE)

m3 <- lrm(red_wine ~ (rcs(pH, 3) + rcs(sulphates, 3))^2, data = mydata, x = TRUE, y = TRUE)
print(m3, coef = FALSE)
anova(m3)

pred_intx <- Predict(m3, 'pH','sulphates', fun = plogis, np = 75)
bplot(pred_intx, yhat ~ pH + sulphates, lfun = wireframe,
      ylab = "Sulphates", zlab = "Pr(Red Wine)\n")

ggplot(Predict(m2, sulphates, pH = c(3.0, 3.2, 3.4), fun = plogis))

#%ia% is restricted interaction - not doubly nonlinear
m4 <- lrm(red_wine ~ rcs(pH, 3) + rcs(sulphates, 3) + pH %ia% sulphates, data = mydata, x = TRUE, y = TRUE)  
print(m4, coef = FALSE)
anova(m4)

pred_intx_r <- Predict(m4, 'pH','sulphates', fun = plogis, np = 75)
bplot(pred_intx_r, yhat ~ pH + sulphates, lfun = wireframe,
      ylab = "Sulphates", zlab = "Pr(Red Wine)\n")

summary(m4)
summary(m4, pH = c(2.97, 3.50)) #contrast of 5th verus 95th %tile
r <- mydata
r$fitted <- predict(m4, type = "fitted")
head(r$fitted)

# Tip 3. Validating fitted models with rms::validate() and rms:calibrate()
(val <- validate(m4, B = 200))
(c_opt_corr <- 0.5 * (val[1, 5] + 1))
