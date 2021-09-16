# survival
data <- data.frame(  x = runif(20),  
                     y = runif(20),   
                     state = rep(  c('a', 'b'),  10  )    )

##########################
library(ggfortify)
library(survival)
library(survminer)
library(KMsurv)
library(flexsurv)
library(Epi)
library(epitools)
library(plotly)
library(cowplot)
library(Cairo) # problems in Cairo

# load data 

orca <- read.table("http://www.stats4life.se/data/oralca.txt")
head(orca)
str(orca)
summary(orca)  # glimpse


### Draw line diagram
tm <- matrix(  c(NA,  NA,  
                 1,   NA),   ncol = 2   )
rownames(tm) <- colnames(tm) <- c("Alive", "Death")

tm2 <- matrix(   c(  NA, NA, NA, 
                     1, NA, NA, 
                     2, NA, NA), ncol = 3)
rownames(tm2) <- colnames(tm2) <- levels(orca$event)

par(mfrow = c(1, 2))
layout(rbind(c(1, 2, 2)))
tm
boxes.matrix(tm, boxpos = TRUE)    # Epi package
title("A)")

tm2
boxes.matrix(tm2, boxpos = TRUE)
title("B)")


#######
table(orca$event)
orca <- mutate(orca, all = event != "Alive")
table(orca$all)
orca



pp <- orca %>%    # ?factor(event) %>%     fct_infreq(event) %>%
    mutate(       text = paste("Subject ID = ", id, "<br>", "Time = ", time, "<br>", "Event = ",  
                               event, "<br>", "Age = ", round(age, 2), "<br>", "Stage = ", stage)   ) %>%
    ggplot(         aes(x   = id,  y = time,  text = text)  )          +
    geom_linerange( aes(ymin = 0, ymax = time ) , alpha= 0.4 )         +
    geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
   # scale_shape_manual(values = c(1, 3, 4)) +
    labs( y = "Time (years)" ,  x = "Subject ID" ) +
    coord_flip() + theme_classic()    
# +  tooltip = "text" )

pp

p <- plot_ly(pp)

ggplotly(g, tooltip = c("city"))

ggplotly(   # erro cairo ?
        orca %>%
        mutate(
            text = paste("Subject ID = ", id, "<br>", "Time = ", time, "<br>", "Event = ",  
                         event, "<br>", "Age = ", round(age, 2), "<br>", "Stage = ", stage)
        ) %>%
        ggplot(aes(x = id, y = time, text = text)) +
        geom_linerange(aes(ymin = 0, ymax = time)) +
        geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
        scale_shape_manual(values = c(1, 3, 4))    +
       labs(y = "Time (years)", x = "Subject ID" ) + 
        coord_flip() + theme_classic()    #  + tooltip = "text" 
       )



grid.arrange(     # good plot
    ggplot(orca, aes(x = id, y = time)) +
        geom_linerange(aes(ymin = 0, ymax = time)) +
        geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
        scale_shape_manual(values = c(1, 3, 4)) + guides(shape = F, color = F) +
        labs(y = "Time (years)", x = "Subject ID") + coord_flip() + theme_classic(),
    orca %>%
        mutate(age_orig = age,
               age_end = age + time) %>%
        ggplot(aes(x = id, y = age_end)) +
        geom_linerange(aes(ymin = age_orig, ymax = age_end)) +
        geom_point(aes(shape = event, color = event), stroke = 1, cex = 2) +
        scale_shape_manual(values = c(1, 3, 4)) + guides(fill = FALSE) +
        labs(y = "Age (years)", x = "Subject ID") + coord_flip() + theme_classic(),
    ncol = 2
)




# A survival object is defined by the pair (y,δ ), 
# i.e. the time variable and the failure or status indicator. 
# the Surv() function in the survival package can be used to create

su_obj <- Surv(orca$time, orca$all)
str(su_obj)
su_obj

# There are several equivalent ways to characterize the probability distribution of a survival random variable:
#    1. the density function f(t)=limΔt→01ΔtPr(t≤T≤t+Δt);
#    2. the cumulative distribution function F(t)=P(T≤t)=∫t0f(u)du #, i.e. the probability of dying within a certain time t
#    3. the survivor function S(t)=P(T>t)=∫∞tf(u)du  , i.e. the probability of surviving longer than time t
#    4. the hazard function λ(t)=limΔt→01ΔtPr(t≤T≤t+Δt|T≥T)=f(t)S(t), usually referred to as instantaneous failure rate, the force of mortality;
#    5. the cumulative hazard function Λ(t)=∫t0λ(u)du


# Estimating the Survival Function - survival or the hazard:
#    a. an empirical estimate of the survival function (i.e., non-parametric estimation);
#    b. a parametric model for λ(t) based on a particular density function f(t)

#   Kaplan–Meier estimator -
# most common estimator and can be explained using different strategies (product limit estimator, likelihood justification).
#   Ŝ(t)=∏j:τj≤t(1−djrj) 
# with τj = distinct death times observed in the sample, dj
# = number of deaths at τj , and rj = number of individuals at risk right before the jth death time (rj=rj−1−dj−1−cj−1, cj
# = number of censored observations between the jth and (j+1) st death times).

# A survival curve is based on a tabulation of the number at risk and number of events at each unique death times.
# The survfit() function of the survival package creates (estimates) the survival curves using different methods (see ?survfit).
# Using the created survival object su_obj as response variable in the formula, the survfit() function will return the default calculations for a Kaplan–Meier analysis of the (overall) survival curve.

fit_km <- survfit(Surv(time, all) ~ 1, data = orca)
fit_km
print(fit_km, print.rmean = TRUE)

# The print() function returns just a summary of the estimated survival curve. 
# The fortify() function of the ggfortify package is useful to extract the whole survival table in a data.frame.

dat_km <- fortify(fit_km)   # ???
head(dat_km)


# The ggsurvplot() is a dedicated function in the survminer package to give an informative illustration 
# of the estimated survival curve(s). See the help page ?ggsurvplot for a description of the different possibilities (arguments).

ggsurvplot(fit_km, risk.table = TRUE, xlab = "Time (years)", censor = T)   # good

#help page ?survfit.formula for a description of the different methods for constructing confidence intervals 

# The default KM plot presents the survival function. 
# Several alternatives/functions are available (see the help page ?plot.survfit or ?ggsurvplot).

glist <- list(
                ggsurvplot(fit_km, fun = "event", main = "Cumulative proportion"),
                ggsurvplot(fit_km, fun = "cumhaz",  main = "Cumulative Hazard"),
                ggsurvplot(fit_km, fun = "cloglog", main = "Complementary log−log")
            )

arrange_ggsurvplots(glist, print = TRUE, ncol = 3, nrow = 1)


# Lifetable or actuarial estimator # Lifetable method very common in actuary and demography. 
# It is particularly suitable for grouped data.
# Ŝ(tj)=∏l≤jp̂l    with p̂j=1−q̂j(conditional probability of survivin
# g), q̂j=dj/r′j (conditional probability of dying), and r′j=rj−cj/2

# In order to show this method on the actual example,
# we need first to create aggregated data, 
# i.e. divide the follow-up in groups and calculate in each strata the number of people at risk, events, and censored.


cuts <- seq(0, 23, 1)
lifetab_dat <- orca %>%
    mutate(time_cat = cut(time, cuts)) %>%
    group_by(time_cat) %>%
    summarise(nlost = sum(all == 0),
              nevent = sum(all == 1))
lifetab_dat

# Based on the grouped data, we will estimate the survival curve using the lifetab() in the KMsurv package. 
# See the help page ?lifetab for a description of the arguments and example.

dat_lt <- with(lifetab_dat, lifetab(tis = cuts, ninit = nrow(orca), nlost = nlost, nevent = nevent))
dat_lt
round(dat_lt, 3)

# Nelson-Aalen estimator
# The focus of the Nelson-Aalen estimator is on the cumulative hazard at time t (Λ̂NA(t)).
# Λ̂NA(t)=∑j:τj≤tdjrj
# Once we have Λ̂(t)NA, we can derive the Fleming-Harrington estimator of S(t)
# ŜFH=exp(−Λ̂(t)NA)

fit_fh <- survfit(su_obj ~ 1, data = orca, type = "fleming-harrington", conf.type = "log-log")
dat_fh <- fortify(fit_fh)  # ???? data frame 
## for the Nelson-Aalen estimator of the cumulative hazard
## dat_fh <- fortify(fit_fh, fun = "cumhaz")
head(dat_fh)


# Graphical comparison 

# # Data dat_km from fortyfy cause error
ggplotly(
        ggplot() +     
        geom_step(data = dat_km, aes(x = time, y = surv, colour = "K-M")) +
        geom_step(data = dat_fh, aes(x = time, y = surv, colour = "N-A")) +
        geom_step(data = dat_lt, aes(x = cuts[-length(cuts)], y = surv, colour = "LT")) +
        labs(x = "Time (years)", y = "Survival", colour = "Estimator") +
        theme_classic()
       )

# Measures of central tendency such as quantiles can be derived from the estimated survival curves.

(  mc <- data.frame(q = c(.25, .5, .75), km = quantile(fit_km), fh = quantile(fit_fh))   )

#A graphical presentation of the estimated quantities (based on the survival curve using K-M).

ggsurvplot(fit_km, xlab = "Time (years)", censor = F)$plot +
    geom_segment(data = mc, aes(x = km.quantile, y = 1-q, xend = km.quantile, yend = 0), lty = 2) +
    geom_segment(data = mc, aes(x = 0, y = 1-q, xend = km.quantile, yend = 1-q), lty = 2)


# Parametric estimators
# As opposed to a non-parametric approach, a parametric one assumes a distribution for the survival distribution. The family of survival distributions can be written by introducing location and scale changes of the form
# log(T)=μ+σW   where the parametric assumption is made on W.

# The flexsurvreg() function in the flexsurv package estimates parametric accelerated failure time (AFT) models. 
# See the help page ?flexsurvreg for a description of different parametric distributions for W.

# consider three common choices: the exponential, the Weibull, and the log-logistic models. 
# In addition, the flexible parametric modelling of time-to-event data using the spline model of Royston and Parmar (2002) is also considered.
#   Model	                Hazard	        Survival
#   Exponential	            λ(t)=λ          S(t)=exp(−λt)
#   Weibull             	λ(t)=λpptp−1    S(t)=exp((−λt)p)
#   Log logistic	λ(t)=λp(λt)p−11+(λt)p   S(t)=11+(λt)p


fit_exp <- flexsurvreg(su_obj ~ 1, data = orca, dist = "exponential")
fit_exp

fit_w  <- flexsurvreg(su_obj ~ 1, data = orca, dist = "weibull")
fit_ll <- flexsurvreg(su_obj ~ 1, data = orca, dist = "llogis")
fit_sp <- flexsurvspline(su_obj ~ 1, data = orca, k = 1, scale = "odds")


# different approaches can be graphically compared (estimated survival curves or hazard functions). 
# OBS ggsurvplot() has not yet implemented graphical functions for object of class flexsurvreg. We can create the plot of our self with some extra lines of code.

grid.arrange(
    ggplot(data.frame(summary(fit_exp)), aes(x = time)) + 
        geom_line(aes(y = est, col = "Exponential")) +
        geom_line(data = data.frame(summary(fit_w)), aes(y = est, col = "Weibull")) +
        geom_line(data = data.frame(summary(fit_ll)), aes(y = est, col = "Log-logistic")) +
        geom_line(data = data.frame(summary(fit_sp)), aes(y = est, col = "Flex splines")) +
        labs(x = "Time (years)", y = "Survival", col = "Distributions") + theme_classic(),
    ggplot(data.frame(summary(fit_exp, type = "hazard")), aes(x = time)) + 
        geom_line(aes(y = est, col = "Exponential")) +
        geom_line(data = data.frame(summary(fit_w, type = "hazard")), aes(y = est, col = "Weibull")) +
        geom_line(data = data.frame(summary(fit_ll, type = "hazard")), aes(y = est, col = "Log-logistic")) +
        geom_line(data = data.frame(summary(fit_sp, type = "hazard")), aes(y = est, col = "Flex splines")) +
        labs(x = "Time (years)", y = "Hazard", col = "Distributions") + theme_classic(),
    ncol = 2
)


# Comparison of survival curves
# A common research question is to compare the survival functions between 2 or more groups. 
# Several alternatives (as well as packages) are available. 
# Have a look at the testing section in the survival analysis task view.

# Tumor stage, for example, is an important prognostic factor in cancer survival studies. 
# We can estimate and plot separate survival curves for the different groups (stages) with different colors.
# epitools

#ci.exp(glm(all ~ 0 + stage, data = orca, family = "poisson", offset = log(time)))
group_by(orca, stage) %>%
    summarise(   D = sum(all),   Y = sum(time)    ) %>%
    cbind(     pois.approx(x = .$D, pt = .$Y)     )


#In general, patients diagnostic with a lower stage tumor has a lower (mortality) rate as compared to patients with high stage tumor. 
# An overall comparison of the survival functions can be performed using the survfit() function.

su_stg  <- survfit(su_obj ~ stage, data = orca)
su_stg

# As the incidence rates are lower for low tumoral stages, the median survival times also decrease for increasing levels of tumoral stage. 
# The same behavior can be observed plotting the K-M survival curves separately for the different tumoral stages.

ggsurvplot(su_stg, fun = "event", censor = F, xlab = "Time (years)")


# It is also possible to construct the whole survival table for each stage level. Here the first 3 lines of the survival table in each tumoral stage.

lifetab_stg <- fortify(su_stg)       # fortyfy data ??
lifetab_stg %>%
    group_by(strata) %>%
    do(head(., n = 3))

# Alternatively, the cumulative hazards and the log-cumulative hazards for the different stages can be presented.

glist <- list(
                ggsurvplot(su_stg, fun = "cumhaz"),
                ggsurvplot(su_stg, fun = "cloglog")
             )
# plot(su_stg, fun = "cloglog")
arrange_ggsurvplots(glist, print = TRUE, ncol = 2, nrow = 2)

# continue




## ????

library(caret)
fitControl <- trainControl(method = "cv", number = 10) #5folds

tune_Grid <-  expand.grid(interaction.depth = 2,  n.trees = 500,   shrinkage = 0.1,  n.minobsinnode = 10)
set.seed(825)
fit <- train(y_train ~ ., data = train, method = "gbm",
             trControl = fitControl,
             verbose = FALSE,
             tuneGrid = gbmGrid)
predicted= predict(fit,test,type= "prob")[,2] 


# Survival  from https://rpubs.com/alecri/258589

pkg <- c("tidyverse", "survival", "ggfortify", "survminer", "plotly", "gridExtra", 
         "Epi", "KMsurv", "gnm", "cmprsk", "mstate", "flexsurv", "splines",
         "epitools", "eha", "shiny", "ctqr", "scales")

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)






######## wikipedia

# Install and load the survival package

#install.packages("survival")
library(survival)

# sort the aml data by time
aml <- aml[order(aml$time),]

print(aml)

# Create graph of length of time that each subject was in the study
with(aml, plot(time, type="h"))

# Create the life table survival object for aml
# The functions survfit() and Surv() create a life table survival object.
# The life table object is passed to the plot() function to create the KM plot.
aml.survfit <- survfit(Surv(time, status == 1) ~ 1, data=aml)

# Plot the Kaplan-Meier curve for aml.
# By default, R includes the confidence interval. 
plot(aml.survfit, xlab = "Time (weeks)", ylab="Proportion surviving", main="Survival in AML")

# The summary() function displays the life table
summary(aml.survfit)

# Create aml life tables and KM plots broken out by treatment (x,  "Maintained" vs. "Not maintained")
surv.by.aml.rx <- survfit(Surv(time, status == 1) ~ x, data = aml)

summary(surv.by.aml.rx)

# Plot KM 
plot(surv.by.aml.rx, xlab = "Time", ylab="Survival",col=c("black", "red"), lty = 1:2, main="Kaplan-Meier Survival vs. Maintenance in AML")

# Add legend
legend(100, .6, c("Maintained", "Not maintained"), lty = 1:2, col=c("black", "red"))

# Perform the log rank test using the R function survdiff().

surv.diff.aml <- survdiff(Surv(time, status == 1) ~ x, data=aml)

surv.diff.aml

# Cox Proportional Hazards regression
# melanoma data set from ISwR package, described in Dalgaard Chapter 12. 
# install the ISwR package and load the library into R.
# The ISwR package currently only appears to be available for older versions of R

#install.packages("ISwR")

library(ISwR)

help(melanom) # description of the melanoma data

# The log rank test is a special case of the cox proportional hazard regression analysis.
# The same analysis can be performed using the R function coxph().
# melanoma example using a log-rank test.
surv.diff.sex <- survdiff(Surv(days, status == 1) ~ sex, data = melanom)

surv.diff.sex

# melanoma analysis using Cox proportional hazards regression
coxph.sex <- coxph(Surv(days, status == 1) ~ sex, data = melanom)

summary(coxph.sex)

# melanoma Cox analysis including covariate ulcer thickness

# Plot the thickness values and log(thickness)
hist(melanom$thick)

hist(log(melanom$thick))

# The Cox PH analysis of melanoma data including covariate log(thick)

coxph.sex.thick <- coxph(Surv(days, status == 1) ~ sex + log(thick), data = melanom)

summary(coxph.sex.thick)

# Examine thickness by sex
boxplot(log(melanom$thick) ~ melanom$sex)

t.test(log(melanom$thick) ~ melanom$sex)

# Test of proportional hazards assumption
coxph.sex <- coxph(Surv(days, status == 1) ~ sex, data = melanom)

cox.zph(coxph.sex)



# Survival tree analysis using the rpart package[edit]
# Rpart and the example are described in the PDF document "An Introduction to Recursive Partitioning Using the RPART Routines". Terry M. Therneau, Elizabeth J. Atkinson, Mayo Foundation. September 3, 1997.

#install.packages("rpart")
library(rpart)

head(stagec)

# Pass a survival object from Surv() to the function rpart() to perform the analysis.
fit <- rpart(Surv(pgtime, pgstat) ~ age + eet + g2 + grade + gleason + ploidy, data=stagec)

# plot the resulting tree

plot(fit, uniform=T, branch=.4, compress=T)
text(fit, use.n=T)
# The print() function provides details of the tree not shown above
print(fit)



##############  survHE
## http://www.statistica.it/gianluca/software/survhe/
install.packages("survHE",
                 repos=c("http://www.statistica.it/gianluca/R",
                         "https://cran.rstudio.org",
                         "https://www.math.ntnu.no/inla/R/stable"),
                 dependencies=TRUE
)

# in linux or mac
#install.packages("devtools")
#devtools:install_github("giabaio/survHE")

??survHE

# Loads the package
library(survHE)
#?install.packages('INLA', repos='https://www.math.ntnu.no/inla/R/stable')
# Loads some data (Breast Cancer from the package 'flexsurv')
data(bc)
# Fits a model with treatment arm ('group') as the only covariate and 
# accounting for censoring ('censrec') using MLE, INLA or HMC

mle=fit.models(formula=Surv(recyrs,censrec)~group,data=bc,distr="exp",method="mle")
inla=fit.models(formula=Surv(recyrs,censrec)~group,data=bc,distr="exp",method="inla")
hmc=fit.models(formula=Surv(recyrs,censrec)~group,data=bc,distr="exp",method="hmc")
# Now prints & plots the output
print(mle); print(inla); print(hmc)
#plot(mle,inla,hmc,labs=c("MLE","INLA", "HMC"))

plot(mle,hmc,labs=c("MLE", "HMC"))



################
# install.packages('devtools')
devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(gapminder)

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    facet_wrap(~continent) +
    # Here comes the gganimate specific bits
    labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
    transition_time(year) +
    ease_aes('linear')


last_animation()
anim_save()

