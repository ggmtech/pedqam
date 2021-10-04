# statistics




# business depends on its future estimates.
# Forecasting is not prediction projections like Regression, TS, extrapolation, index numbers
# forecasting is foretelling the course of business activity based on the analysis of past and present data.
# Forecasts based on stat analysis  more reliable basis Historical analysis and current economic conditions
# business forecasting: (1) Naïve Method (2) Barometric Methods (3) Analytical Methods


# Naïve methods : time series data of own company and forecasts on the basis of projections, not reliable.
# Barometric methods : Specific historical analogy as “history repeat itself” with adjustments
# Lead Lag relationship (aka sequence theory) changes in business in succession, not simultaneously.eg fe x-change with  change in wholesale prices 
# Diffusion Index  different factors, affecting business, do not attain their peaks simultaneously.
# Action-Reaction theory : always a recession after prosperity and prosperity after recession.
# The analytical methods : 
# Random Forest Model in R

# The factor listing method : factors  likely to influence the business  identified. Each factor is studied closely and measure the impact of the factor upon aggregate business activity is favorable or not
# Cross-Cut analysis theory: opposite of the historical analogy theory. past cycles cannot guide future # Basis this theory current policies, demands, availability of inputs, etc. change with the lapse of time.

# Exponential Smoothing  for improving trends by moving the average method. greater weights to recent data.
# Econometric methods involve economics, statistics, and mathematics.  analyze econometric system eco theory
# Sample Size calculation formula
# Opinion Polling :  present attitudes of the people towards business can be real guide in near future.
#            from business experts, consumers and sales forces not expensive for short term forecasts.


# Characteristic of an Index number
# Index numbers as %, comparable  at any two timing or places, sort of weighted averages, indirect
# Laspeyre’s Price and Quantity Index Numbers,  base year method, upward bias.
# Paasche’s Price and Quantity Index Numbers,   given year method index. downward bias.
# Drobish-Bowley PI, Geometric Cross Formula, Arithmetically Crossed-Weight Formula, Kelly’s fixed weight Formula

# Naive Bayes Classification: Bayes’ Theorem and assumed predictor independence. simple huge data sets.

# Binomial Distribution : James Bernoulli' 1713. two parameters n and p.
# n ( finite) Bernoulli trials  “p” and “q” ,  probability of x success out of n Bernoulli trials 
# f(x)=(ncx)pxqn-x where x=0, 1 , 2, …..,n. 0<=p<=1 and p+q=1.    
# If n=1, -> Bernoulli distribution.
#  mean  np, variance npq, moment (q+pet)n, characteristic function of b (n, p) is (q+peit)n

#  dbinom(x, size, prob)
#  plot(x, y, type = ‘h’) to plot the probability mass function.
#  dbinom() function.
# eg  binomial distribution with size = 50 and prob = 0.45, success <- 0:50
# plot(success, dbinom(success, size=50, prob=.45),type='h')
success <- 0:50
plot(success, dbinom( success, size=50, prob=.45 ),
     type='h', 
     main='Binomial Distribution (n=50, p=0.45)',
     ylab='Probability',
     xlab ='Successes',
     lwd=3)




# Sample Size Calculation and Power Clinical Trials
# # significance ,  power,  basic rules and processes and sample size estimation examples.
# The outcome of a response variable is determined by the mean, variance, or proportion of individuals.
# The difference between test and control can be considered as clinically meaningful differences.
# The significance level is at 0.05 (95% confidence level)
# The power of the test is above 80% is acceptable
qnorm(0.85)
# Case1: Comparing two proportions
power<-85
p1<-0.55 # (Probability of success group 1)
p2<-0.42 # (Probability of success group 2)
dropout<-0.20
alpha<-0.05
p1p2<-p1-p2
if(alpha==0.05){ zalpha <- 1.96}
if(power==95){ zbeta <- qnorm(power)}
if(power==90){ zbeta <- qnorm(power)}
if(power==85){ zbeta <- qnorm(power) }
if(power==80){ zbeta <- qnorm(power)}
zbeta <- qnorm(85/100)
zbeta
n1<-(( zalpha + zbeta )^2*((p1*(1-p1))+(p2*(1-p2))))/(p1-p2)^2
n<-n1*2
Total<-n+n*dropout
??Total

######
# Case 2: Comparing two means
mean1<-8
mean2<-6
sd<-2
power<-85
alpha<-0.05
dropout<-0.20
if(alpha==0.05){zalpha<-1.96}
if(power==95){zbeta<-qnorm(power)}
if(power==90){zbeta<-qnorm(power)}
if(power==80){zbeta<-qnorm(power)}
zbeta<-qnorm(0.85)
n<-(((zalpha+zbeta)^2)*(2*(sd*sd)))/(mean1-mean2)^2
n<-n1*2
Total<-n+n*dropout
??Total     # 546.7774





