# 7 QC tools,- Kaoru Ishikawa,professor, U Tokyo influence of W. Edwards Deming . ~ 95% of all company problems 
# 1 Pareto Chart
# 2 Run Chart - run-sequence plot
# 3 Histogram : centering, variation , data ; underlying distribution ; future prediction of process performance; process capabability
# 4 Cause-and-effect diagram : Ishikawa fishbone diagram : root causes for an effect, problem or undesirable outcome while sorting them into six Ms: measurement, material, machine, method, manpower and mother nature.  brainstorming ask “why does this happen?” to generate potential causes; having multiple layers of causes is a common practice, where the layers indicate causal relationships
# 5 Scatter plots : X-Y graph for data relationship but correlation does not necessarily imply causation
# 6 Control charts : samples or processes meeting intended specifications and, if not, the degree by which they vary from those specifications. 
# see if a process is under statistical control; help visualize variation; find and correct problems when they occur; predict expected ranges if outcomes; and analyze patterns of process variation from special causes. two main groups charts:  monitoring discrete data — further divided by the sample size (e.g. X-MR chart, I-MR chart, x-bar chart, R-chart and s-chart)— and the other for monitoring continuous data — which is further divided by the numbers of defects per unit and then by the sample size (e.g. c-chart, u-chart, np-chart and p-chart). 
# x-bar chart
# R chart
# S chart
# c chart
# u chart
# np chart
# p chart

# 7 Check Sheet : defect concentration diagram, structured form for collecting and analyzing data in real time as data is generated. Quantitative/ Qualitative. by making marks (i.e. checks)as effective way of displaying data; easy to use; can identify the root cause of a problem; can be used to substantiate or refute allegations; and represent the first step in the construction of other graphical tools. Let’s take a look at the following R code to generate a basic check sheet.

# qcc  statistical quality control charts provides:
# Shewhart quality control charts for continuous, attribute and count data #run-sequence plot- observed data in a time sequence.
# Cusum and EWMA charts
# Operating characteristic curves
# Process capability analysis
# Pareto chart (and Pareto distribution diagram) Italian engineer Vilfredo Pareto,80% of the output in a given system is produced by 20% of the input (i.e. defects, mistakes, errors).vital few (~20%) inputs from the insignificant many (~80%)
# cause-and-effect chart
# Multivariate control charts.

# ISO terminology
# Ac	acceptance number
# CR (β)	consumer´s risk
# CRQ	consumer’s risk quality
# D	number of nonconforming items (or nonconformities) in the population or lot
# d	number of nonconforming items (or nonconformities) found in a sample from a lot
# LQ	limiting quality
# N	lot size
# n	sample size
# OC	operating characteristic
# p	lot proportion nonconforming or average number of nonconformities per item in the lot
# P	probability
# Pa	probability of acceptance
# PR (α)	producer´s risk
# PRQ	producer´s risk quality
# σ2	variance of a statistical distribution
# µ	mean of a statistical distribution

# AcceptanceSampling::find.plan()  can be used for finding single sampling plans.
library(AcceptanceSampling)
find.plan(PRP=c(0.05,0.95),CRP=c(0.15,0.20),type="hypergeom",N=500)

plan<-OC2c(51,5,type="hypergeom", N=500, pd=seq(0,.25,.01))
plot(plan, type='l')
grid()
# 
assess(OC2c(51,5), PRP=c(0.05, 0.95), CRP=c(0.15,0.20)) # how close to plan
find.plan(PRP=c(0.05,0.95),CRP=c(0.08,0.20),type="hypergeom",N=500)  # to find perticuler plan
plot(plan, type='l') #xx
grid()
# Double sampling plan: If the number nonconforming in the first sample is between c1+1  and  r1−1, a second sample of size n2 is taken. 
# If the sum of the number of nonconforming in the first and second samples is less than or equal to  c2, the lot is accepted.


library(qcc)

# Cause and effect diagram  : But better visual is  of SixSigma funciton ss.ceDiag()
cause.and.effect(cause = list(Measurements = c("Micrometers", "Microscopes", "Inspectors"),
                              Materials    = c("Alloys", "Lubricants", "Suppliers"),
                              Personnel    = c("Shifts", "Supervisors", "Training", "Operators"),
                              Environment  = c("Condensation", "Moisture"),
                              Methods      = c("Brake", "Engager", "Angle"),
                              Machines     = c("Speed", "Lathes", "Bits", "Sockets")),
                 effect = "Surface Flaws")


#Pareto chart

names(defect) = c("price code", "schedule date", "supplier code", "contact num.", "part num.")
defect        = c(   80,              27,              66,           94,             33)

defect    # Defect wise summary()
pareto.chart(defect, ylab = "Error frequency")












data(pistonrings)
pistonrings   # attach(pistonrings)  # not if not using like db$var 

diameter <- qcc.groups(pistonrings$diameter, pistonrings$sample)
diameter


qcc(diameter[1:25,], type="xbar")
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])

q <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)

plot(q, chart.all=FALSE)

qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], nsigmas=2)
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], confidence.level=0.99)

qcc(diameter[1:25,], type="R")
qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])

qcc(diameter[1:25,], type="S")
qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])

# add warning limits at 2 std. deviations
q <- qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,], plot=FALSE)
(warn.limits <- limits.xbar(q$center, q$std.dev, q$sizes, 2))
plot(q, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")

# variable control limits
out <- c(9, 10, 30, 35, 45, 64, 65, 74, 75, 85, 99, 100)
diameter <- qcc.groups(pistonrings$diameter[-out], sample[-out])
qcc(diameter[1:25,], type="xbar")
qcc(diameter[1:25,], type="R")
qcc(diameter[1:25,], type="S")
qcc(diameter[1:25,], type="xbar", newdata=diameter[26:40,])
qcc(diameter[1:25,], type="R", newdata=diameter[26:40,])
qcc(diameter[1:25,], type="S", newdata=diameter[26:40,])

detach(pistonrings)

# OC curves ???
data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)
beta <- oc.curves.xbar(qcc(diameter, type="xbar", nsigmas=3, plot=FALSE))
print(round(beta, digits=4))
# or to identify points on the plot use
## Not run: oc.curves.xbar(qcc(diameter, type="xbar", nsigmas=3, plot=FALSE), identify=TRUE)
detach(pistonrings)

data(orangejuice)
attach(orangejuice)
beta <- oc.curves(qcc(D[trial], sizes=size[trial], type="p", plot=FALSE))
print(round(beta, digits=4))
# or to identify points on the plot use
## Not run: oc.curves(qcc(D[trial], sizes=size[trial], type="p", plot=FALSE), identify=TRUE)
detach(orangejuice)

data(circuit)
attach(circuit)
q <- qcc(x[trial], sizes=size[trial], type="c", plot=FALSE)
beta <- oc.curves(q)
print(round(beta, digits=4))
# or to identify points on the plot use
## Not run: oc.curves(qcc(x[trial], sizes=size[trial], type="c", plot=FALSE), identify=TRUE)
detach(circuit)


data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
q = qcc(diameter, type="xbar", nsigmas=3, plot=FALSE)
beta = oc.curves.xbar(q)
vignette("qcc")


data(pistonrings)
diameter <- qccGroups(data = pistonrings, diameter, sample)
q <- qcc(diameter, type = "xbar", nsigmas = 3)
(beta <- ocCurves(q))




##  Attribute data 
data(orangejuice)
attach(orangejuice)
qcc(D[trial], sizes=size[trial], type="p")

# remove out-of-control points (see help(orangejuice) for the reasons)
inc <- setdiff(which(trial), c(15,23))
q1 <- qcc(D[inc], sizes=size[inc], type="p")
qcc(D[inc], sizes=size[inc], type="p", newdata=D[!trial], newsizes=size[!trial]) 
detach(orangejuice)

data(orangejuice2)
attach(orangejuice2)
names(D) <- sample
qcc(D[trial], sizes=size[trial], type="p")
q2 <- qcc(D[trial], sizes=size[trial], type="p", newdata=D[!trial], newsizes=size[!trial])
detach(orangejuice2)

# put on the same graph the two orange juice samples
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(5,5,3,0))
plot(q1, title="First samples", ylim=c(0,0.5), add.stats=FALSE, restore.par=FALSE)
par("mar"=c(5,0,3,3), yaxt="n")
plot(q2, title="Second samples", add.stats=FALSE, ylim=c(0,0.5))
par(oldpar)

data(circuit)
attach(circuit)
qcc(x[trial], sizes=size[trial], type="c")
# remove out-of-control points (see help(circuit) for the reasons)
inc <- setdiff(which(trial), c(6,20))
qcc(x[inc], sizes=size[inc], type="c", labels=inc)
qcc(x[inc], sizes=size[inc], type="c", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
qcc(x[inc], sizes=size[inc], type="u", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
detach(circuit)

data(pcmanufact)
attach(pcmanufact)
qcc(x, sizes=size, type="u")
detach(pcmanufact)

data(dyedcloth)
attach(dyedcloth)
qcc(x, sizes=size, type="u")
# standardized control chart
q <- qcc(x, sizes=size, type="u", plot=FALSE)
z <- (q$statistics - q$center)/sqrt(q$center/q$size)
plot(z,  type="o", ylim=range(z,3,-3), pch=16)
abline(h=0, lty=2)
abline(h=c(-3,3), lty=2)
detach(dyedcloth)


##  Continuous one-at-time data 

# viscosity data (Montgomery, pag. 242)
x <- c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20, 33.62, 33.00, 33.54, 33.12, 33.84)
qcc(x, type="xbar.one")
qcc(x, type="xbar.one", std.dev = "SD")




# Shewhart charts
# x-bar chart
data(pistonrings)
diameter <- qcc.groups( pistonrings$diameter, pistonrings$sample)
head(diameter)

(q1 <- qcc(diameter[1:25,], type = "xbar",   newdata = diameter[26:40,]))

plot(q1, chart.all = FALSE)
plot(q1, add.stats = FALSE)

plot(qcc(diameter[1:25,], type = "xbar", 
         newdata = diameter[26:40,], 
         confidence.level = 0.99))


q1 <- qcc(diameter[1:25,], type = "xbar",   
          newdata = diameter[26:40,], 
          rules = 1:4)
plot(q1, fill = FALSE)


(dates <- seq(Sys.Date() - nrow(diameter)+1, Sys.Date(), by = 1))
q = qcc(diameter[1:25,], type = "xbar", newdata = diameter[26:40,])
plot(q, xtime = dates)

# Further fine-tuning is also available as shown in the example below:
# library(scales)
library(tidyverse)
plot(q, xtime = dates, xlab = NULL, add.stats = FALSE) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b")


# R chart
(q2 <- qcc(diameter[1:25,], type = "R"))
plot(q2)
(q3 <- qcc(diameter[1:25,], type = "R", newdata = diameter[26:40,]))

#S chart
(q4 <- qcc(diameter[1:25,], type = "S"))
plot(q4)


(q5 <- qcc(diameter[1:25,], type = "S", newdata = diameter[26:40,]))
plot(q5)


# Variable control limits in control charts
out <- c(9, 10, 30, 35, 45, 64, 65, 74, 75, 85, 99, 100)
# error fn # 
diameter2 <- qcc::qccGroups(data = pistonrings[-out,], diameter, sample)
plot(qcc(diameter2[1:25,], type = "xbar"))


plot(qcc(diameter2[1:25,], type = "R"))
q = qcc(diameter2[1:25,], type = "xbar", rules = 1:4)
plot(qcc(diameter2[1:25,], type = "xbar", newdata = diameter2[26:40,], rules = 1:4))


# p and np charts
data(orangejuice)
(q = with(orangejuice, qcc(D[trial], sizes = size[trial], type = "p")))
plot(q)

(q = with(orangejuice, qcc(D[trial], sizes = size[trial], type = "np")))
plot(q)

# Remove out-of-control points (see help(orangejuice) for the reasons):
inc <- setdiff(which(orangejuice$trial), c(15,23))
(q = with(orangejuice, qcc(D[inc], sizes = size[inc], type = "p", newdata = D[!trial], newsizes = size[!trial])))

plot(q)



##################################
# An operating characteristic curve graphically provides information about the probability of not detecting a shift in the process. 
# The function oc.curves() is a generic function which calls the proper function depending on the type of input 'qcc' object.
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))
q = qcc(diameter, type="xbar", nsigmas=3, plot=FALSE)
beta = oc.curves.xbar(q)

data(orangejuice)
q = with(orangejuice, qcc(D[trial], sizes=size[trial], type="p", plot=FALSE))
beta = oc.curves(q)
print(round(beta, digits=4))

data(circuit)
q = with(circuit, qcc(x[trial], sizes=size[trial], type="c", plot=FALSE))
beta = oc.curves(q)
print(round(beta, digits=4))

#############
# cusum chart


#######################
# Process capability analysis
data(pistonrings)
diameter = with(pistonrings, qcc.groups(diameter, sample))

q1 = qcc(diameter[1:25,], type="xbar", nsigmas=3, plot=FALSE)
process.capability(q1, spec.limits=c(73.95,74.05))


process.capability(q1, spec.limits=c(73.95,74.05), target=74.02)
process.capability(q1, spec.limits=c(73.99,74.01))
process.capability(q1, spec.limits = c(73.99, 74.1))


# Multivariate Quality Control Charts
# Multivariate subgrouped data from Ryan (2000, Table 9.2) with p=2 variables, m=20 samples, and n=4 sample size for each sample:

X1 = matrix(c(72, 56, 55, 44, 97, 83, 47, 88, 57, 26, 46, 
              49, 71, 71, 67, 55, 49, 72, 61, 35, 84, 87, 73, 80, 26, 89, 66, 
              50, 47, 39, 27, 62, 63, 58, 69, 63, 51, 80, 74, 38, 79, 33, 22, 
              54, 48, 91, 53, 84, 41, 52, 63, 78, 82, 69, 70, 72, 55, 61, 62, 
              41, 49, 42, 60, 74, 58, 62, 58, 69, 46, 48, 34, 87, 55, 70, 94, 
              49, 76, 59, 57, 46), ncol = 4)
X2 = matrix(c(23, 14, 13, 9, 36, 30, 12, 31, 14, 7, 10, 
              11, 22, 21, 18, 15, 13, 22, 19, 10, 30, 31, 22, 28, 10, 35, 18, 
              11, 10, 11, 8, 20, 16, 19, 19, 16, 14, 28, 20, 11, 28, 8, 6, 
              15, 14, 36, 14, 30, 8, 35, 19, 27, 31, 17, 18, 20, 16, 18, 16, 
              13, 10, 9, 16, 25, 15, 18, 16, 19, 10, 30, 9, 31, 15, 20, 35, 
              12, 26, 17, 14, 16), ncol = 4)
X = list(X1 = X1, X2 = X2) # a list of matrices, one for each variable

q = mqcc(X, type = "T2")
summary(q)

ellipseChart(q)
ellipseChart(q, show.id = TRUE)
q = mqcc(X, type = "T2", pred.limits = TRUE)
q1 = qcc(X1, type = "xbar", confidence.level = q$confidence.level^(1/2))
summary(q1)
q2 = qcc(X2, type = "xbar", confidence.level = q$confidence.level^(1/2))
summary(q2)
# Generate new “in control” data:

Xnew = list(X1 = matrix(NA, 10, 4), X2 =  matrix(NA, 10, 4))
for(i in 1:4)
{ x = MASS::mvrnorm(10, mu = q$center, Sigma = q$cov)
Xnew$X1[,i] = x[,1]
Xnew$X2[,i] = x[,2] 
}
qq = mqcc(X, type = "T2", newdata = Xnew, pred.limits = TRUE)
summary(qq)

ellipseChart(qq, show.id = TRUE)
# Generate new “out of control” data:

Xnew = list(X1 = matrix(NA, 10, 4), X2 =  matrix(NA, 10, 4))
for(i in 1:4)
{ x = MASS::mvrnorm(10, mu = 1.2*q$center, Sigma = q$cov)
Xnew$X1[,i] = x[,1]
Xnew$X2[,i] = x[,2] 
}
qq = mqcc(X, type = "T2", newdata = Xnew, pred.limits = TRUE)







##########################################
# low level grid package used
# https://towardsdatascience.com/7-basic-tools-of-quality-using-r-49fef5481e07
library(SixSigma)      # Import the SixSigma package
x <- rnorm(50 ,15, 5)  # Create 50 random data points

# Build the run chart
plot(x,                        
     type = "b",              
     pch  = 16,                 
     ylim = c(0,30),           
     axes = FALSE,             
     main = "Run Chart Title", 
     sub  = "Chart Subtitle",  
     xlab = "Run",            
     ylab = "y (measure)")     
axis(1, at = 0:50,    cex.axis = 0.7)           
axis(2)                        
box()                          
grid()                         
abline(h = 15 ,    lwd = 2)



# Histogram
x <- rnorm(100, 15, 5) # Create 50 random data points

# Build the histogram
hist(x,                             
     breaks = "FD",                 
     main = "Histogram Title",      
     sub = "Histogram Subtitle",    
     xlab = "Measure",              
     col = "lightgrey",             
     border = "black")              
grid()                            
box()            

# Cause effect Diagram
b.effect <- "Delay"        # Specify the effect to be analyzed
b.groups <- c("Personnel", "Weather", "Suppliers", "Planning", "testingdata")  # Create groups
b.causes <- c(vector(mode = "list", length = length(b.groups))) # vectorise groups
# list of each groups
b.causes[1] <- list(c("Training", "Inadequate"))
b.causes[2] <- list(c("Rain", "Temperature", "Wind"))
b.causes[3] <- list(c("Materials", "Delays", "Rework"))
b.causes[4] <- list(c("Customer", "Permissions", "Errors"))
b.causes[5] <- list(c("evenwithoutit", "Permissions", "Errors"))
# Create the cause-and-effect diagram
ss.ceDiag(b.effect,
          b.groups,
          b.causes,
          main = "Cause-and-Effect Diagram (SixSigma package)",
          sub = "Construction Example")


# simplified test roiugh
# Cause effect Diagram
b.effect <- "Delay"        # Specify the effect to be analyzed
b.groups <- c("Personnel", "Weather", "Suppliers", "Planning", "testingdata")  # Create groups
b.causes <- c(vector(mode = "list", length = length(b.groups))) # vectorise groups
# list of each groups
b.causes[1] <- list(c("Training", "Inadequate"))
b.causes[2] <- list(c("Rain", "Temperature", "Wind"))
b.causes[3] <- list(c("Materials", "Delays", "Rework"))
b.causes[4] <- list(c("Customer", "Permissions", "Errors"))
b.causes[5] <- list(c("evenwithoutit", "Permissions", "Errors"))

# Create the cause-and-effect diagram
SixSigma::ss.ceDiag(
                      "Delay Cause",
                      c("Personnel", "Weather", "Suppliers", "Planning", "testingdata"),
                      c(  c("Training", "Inadequate"), 
                          c("Rain", "Temperature", "Wind"), 
                          c("Materials", "Delays", "Rework"), 
                          c("Customer", "Permissions", "Errors"), 
                          c("evenwithoutit", "Permissions", "Errors") 
                        ),
                      main = "Cause-and-Effect Diagram (SixSigma package)",
                      sub = "Construction Example")



# Check sheets # Warning!! create using some other package!
library(grid)  # Import the grid package, low level for making like ggplot , lattice etc
# Make inspection check sheet
grid.rect(width = 0.5,  height = unit(9, "inches"), x = 0.5)  # Build external box
grid.text("CHECK SHEET", x = 0.5, y = 0.95, just = "top")     # Add a title
grid.text("Item No.:___________________________________ Customer:___________________________________",   x = 0.28, y = 0.875, just = "left")
grid.text("Item description:________________________________________________________________________",  x = 0.28,  y = 0.825,just = "left")
grid.rect(width = 0.45,  height = unit(6.8, "inches"), x = 0.5, y = 0.42) # Build internal box
grid.text("INSPECTION CHECK POINTS - FILE REVIEW & QUALITY ASSURANCE",x = 0.5, y = 0.74, just = "top")
grid.text("Description                                                     YES               NO               N/A               Comments",
          x = 0.285,  y = 0.68, just = "left")
grid.text("1._________________________\n\n
           2._________________________\n\n
           3._________________________\n\n
           4._________________________\n\n
           5._________________________\n\n
           6._________________________\n\n
           7._________________________\n\n
           8._________________________\n\n
           9._________________________\n\n
          10.________________________", x = 0.285, y = 0.4,just = "left")
grid.text("INSPECTOR NAME:___________________________ SIGNATURE:___________________________", x = 0.5, y = 0.1, just = "center")



