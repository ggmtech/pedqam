#Linear optimization using R

# Load Packages
# install.packages("lpSolve")
library(lpSolve)

#Set the coefficients of the decision variables
Objective.in<-c(25,20)

# Constraint Matrix

Const.mat<-matrix(c(20,12,4,4),nrow=2,byrow=TRUE)

# Define the constraints

Time_constraint<-8*60
Resouce_constraint<-1800
#RHS for the constraints
Const.rhs<-c(Resouce_constraint, Time_constraint)

# Constraints direction
Const.dir<-c("<","<=")

# Optimal Solution
Optimum<-lp(direction="max",Objective.in,Const.mat,Const.dir,Const.rhs)

# Optimal values for x1 and x2 are 45, 75
# The value of the objective function at an optimal point is
Optimum$objective










