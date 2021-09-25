# Weibull distr p.d.f  fx(x; α, β)= α/βα [x α-1e(-x/ β)^α       ]For x>0, α, β>0.
#  reliability theory. Corrosion, alloy weight loss, and metal tensile strength follow Weibull
# IEC 61649, Edition 2, Weibull Analysis # (the official international standard)
# http://weibullnews.com/Weibull-Engineering-Basics-and-Introduction.html
# management wants how long any particular battery of the same design should be used before replacement if the chance of failure is limited to only 2 percent (%). Plus conservativeness

# Plot battery test data on Weibull CDF scalingordered lowest to highest.
#  place data points on the Weibull plot horizontally by increasing data value, and vertically by increasing failure probability estimated with order statistics. 
#  fit a straight line (on this scaling) to the data is a standard Weibull solution using graphical `rank regression` (rr).
#   Another way to get a solution repeatedly searches for highest data probability, giving a `maximum likelihood estimate` (mle). The resulting plot models the variability of battery life capability. 
#   The Weibull CDF plot of the battery test results (Figure 3 below) shows graphical p-value estimate (pve%) of 79.51 for goodness-of-fit. 
#   A pve% value can go from 0 to 100, and a value of 50 is nominal for data sampled 
#   A pve% of 10 or higher is usually acceptable.  The fit line has Weibull slope Beta of 3.02
#   Slope > 1 indicates wear out as the type of occurrence mechanism older failing faster 
#   A nominal life capability estimate of 75 hours comes by reading the horizontal value of the fit line where it crosses 2% on the vertical scale (2% failure probability).
#  conservative estimate. A lower estimate line, CI line, curved confidence line on the plot is a 90% lower estimate of life capability. 
#  The code on the plot, W/rr/c%=pv-90, means that the solution for the straight line is Weibull using the rank regression method with lower(-) 90% confidence estimated by Pivotal (pv) Monte Carlo. 
#  When read from the lower confidence line, the conservative estimate of life capability goes down to 30 hours (horizontal scale). 
#  Note that 2% failure occurrence probability equates to 98% reliability. 
#  With additional similar battery test data, the lower confidence line may get closer to the nominal line. This might give an even higher estimate of life capability for the same 98% reliability.
# Weibull-Engineering-Basics-Battery-New.jpg
# Adding one cost factor for planned replacement (usually lower ) and a second cost factor for emergency replacement due to failure () at higher cost) for optimum replacement in non-safety-related items.

install.packages(WeibullR)
library(WeibullR)
# weibull 
dweibull(x, shape, scale = 1) #to create the probability density function.
curve(function, from = NULL, to = NULL) #to plot the probability density function.
curve(dweibull(x, shape=2, scale = 2), from=0, to=5)

curve(dweibull(x, shape=2, scale = 2), from=0, to=5,
      main = 'Weibull Distribution (shape = 2, scale = 2)',
      ylab = ' dWeibull gives the density',
      lwd = 2,
      col = 'pink')

#add more than one curve in the same plot
curve(dweibull(x, shape=2, scale = 2), from=0, to=5,
      main = 'Weibull Distribution',
      ylab = ' dWeibull gives the density',
      lwd = 2,
      col = 'pink')
curve(dweibull(x, shape=1.05, scale = 2), from=0, to=5, col='green', add=TRUE)
# add legends
legend(2, .3, legend=c("shape=2, scale=2", "shape=1.5, scale=2"),
col=c("green", "blue"), lty=1, cex=1.2)

curve(dweibull(x, shape=1, scale = 2), from=0, to=5, col='black', add=TRUE)
curve(dweibull(x, shape=0.71, scale = 2), from=0, to=5, col='blue', add=TRUE)



rivet_failures<-c(30, 49, 82, 90, 96)
rivet_failures<-c(
  30, 49, 82, 90, 96)


# Now let’s get a fit using WeibullR’s
MRRw2p(rivet_failures) # Quick Fit method MRRw2p:


# Using the wblr object
obj1<-wblr(rivet_failures) # Creates a wblr object labeled obj1.
obj1<-wblr.fit(obj1, col="red") #Adds a default fit that will appear as ared line on a chart.
obj1<-wblr.conf(obj1, lwd=1) # Adds default CI boundsusing a single width line.
plot(obj1)

help(package="WeibullR")


# Quick Fit Functions :  MRRw2p, MLEln3p and permutations 
# wblr Object Model  wblr, wblr.fit, and wblr.conf  with more user control,permits multi plots
# Backend functions are mostly employed by advanced users anddevelopers.
#Using the wblr object
obj2<-wblr.conf(wblr.fit(wblr(rivet_failures,rivet_suspensions, col="purple"),),lwd=1)
# Here a nested object method used to create similar obj2 incorporating rivet suspension data.
# (Notice that the color specification wasapplied inside the wblr parenthesis, not the fit.)
# plot both items on a single chart , added as a list argument to plot.wblr
plot.wblr(list(obj1, obj2))

# WeibullR functions (exc Quick Fit), can take a primary argument formed as a df with time and event columns.  event column  1’s and 0’s defining failures and suspensions.can be created as
rivet_data <- data.frame(time=c(rivet_failures, rivet_suspensions), event=c(rep(1,length(rivet_failures)), rep(0,length(rivet_suspensions))))
rivet_data

#Data can be read from locally or known web sources.
# agcread.csv( file.choose())

agc<-read.csv("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/data/acid_gas_compressor.csv", header=T)$agc

agc

#Running a script
# source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/contour_challenge.r")

daf<-read.csv("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/data/contour_challenge/daTEST.csv", header=FALSE)
das<-read.csv("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/data/contour_challenge/dasuspendedTEST.csv", header=FALSE)

fdf<-as.data.frame(table(daf[,1]))
ft<- as.numeric(levels(fdf[,1]))
fq<-fdf[  , 2 ]
sdf<-as.data.frame(table(das[,1]))
st<-as.numeric(levels(sdf[,1]))
sq<-sdf[,2]
fail_edata<-data.frame(time=ft, event=rep(1, length(ft)), qty=fq)
sus_edata <-data.frame(time=st, event=rep(0, length(st)), qty=sq)
teq_frame<-rbind(fail_edata, sus_edata)
teq_frame

require(WeibullR)
contour(   WeibullR::wblr(teq_frame),  col="grey")

source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/manipulate_contour_challenge.r")

# Replicating Figure3.13 from “The New Weibull Handbook”
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/Fig3.13ln.r")

#A Multi-Distribution plot on Weibull Canvas
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/Fig3.13multi.r")

#Bathtub Life Data
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/bathtub_life.r")

#Life Data Division as Competing Modes
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/competing_modes.r")

# Linearized 3p fits by t0 modification
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/linearized3p.r")

# Compare two data sets by likelihood contour
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/compare_6mp.r")

# Inspection Data for Cracks in Parts in Service - Probit Analysis
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/probit_1.r")

# Interval Analysis on Parts Cracking Data (need WeibullR version > 1.0.11.4 on R-Forge)
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/inspection_intervals.r")

# Comparing the Simple Weibayes Function to a Challenging MLE Contour
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/weibayes_study.r")

# Contour to Bounds
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/contour2bounds.r")

# Likelihood Ratio Bounds on a 3-Parameter Model
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/LRbounds_3p.r")

# Fisher Matrix Bounds on a 3-Parameter Model
source("https://raw.githubusercontent.com/openrelia/WeibullR.gallery/master/scripts/three_parameter_FM2.r")

