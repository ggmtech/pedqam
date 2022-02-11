# Type I errors :  False Positive, alpha error, False alarm, Producer’s risk 
# Type II errors : False negative, beta error, Misdetection, Consumer’s risk

# Type I error() is p of rejecting a true null hypothesis.
# Type II error() is p of failing to reject a false null hypothesis.
# Type I error() is p of telling you things are wrong, given that things are correct.
# Type II error() is p of telling that things are correct, given they are wrong.


# qa calibration
# qcc is a contributed R package for statistical quality control charts which provides:
# Shewhart quality control charts for continuous, attribute and count data
# Cusum and EWMA charts
# Operating characteristic curves
# Process capability analysis
# Pareto chart and cause-and-effect chart
# Multivariate control charts.

# MAD plot - John Tukey  or  Bland–Altman plot : both measure same parameter good corr
# John Tukey (bit, FFT, boxplot, lambda distr, )

library(qcc)
data(pistonrings)
pistonrings
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)  # This groups by sample by making them columns of one run
diameter
qcc( diameter[1:25,  ], type="xbar" )
qcc( diameter[1:25,  ], type="xbar",     newdata=diameter[26:40,]   )

grDevices::dev.off()
q <- qcc( diameter[1:25,], type="xbar",  newdata=diameter[26:40,] ,   plot=TRUE )
q
plot(q)
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


##  Attribute data 


data(orangejuice)
attach(orangejuice)
qcc(  D[trial], sizes=size[trial], type="p" )

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

##
##  Continuous one-at-time data 
##

# viscosity data (Montgomery, pag. 242)
x <- c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20,
       33.62, 33.00, 33.54, 33.12, 33.84)
qcc(x, type="xbar.one")
qcc(x, type="xbar.one", std.dev = "SD")



#################################################
#################################################
#################################################
# Bland-Altman plot in R
# Plot Differences in Two Measurements-Bland-Altman Plot in R
# between two different instruments or measurement methodologies are visualized using a Bland-Altman plot.
data <- data.frame(A=c(6, 5, 3, 5, 6, 6, 5, 4, 7, 8, 9, 10, 11, 13, 10, 4, 15, 8, 22, 5),
                   B=c(5, 4, 3, 5, 5, 6, 8, 6, 4, 7, 7, 11, 13, 5, 10, 11, 14, 8, 9, 4))

data$avg <- rowMeans(data)
data$diff <- data$A - data$B
mean_diff <- mean(data$diff)

# find lower and upper 95% confidence interval limits
lower <- mean_diff - 1.96*sd(data$diff)
upper <- mean_diff + 1.96*sd(data$diff)

# Average difference turns out to be 0.85 and the 95% CI for average difference is [-6.997089, 8.697089].
#Create the Bland-Altman Plot
ggplot2::ggplot(data, aes(x = avg, y = diff)) +
  geom_point(size=2) +
  geom_hline(yintercept = mean_diff) +
  geom_hline(yintercept = lower, color = "red", linetype="dashed") +
  geom_hline(yintercept = upper, color = "red", linetype="dashed") +
  ggtitle("Bland-Altman Plot") +
  ylab("Difference Between Instruments") +
  xlab("Average")+theme_bw()



# Cosine Similarity = ΣAiBi / (√ΣAi2√ΣBi2)
x <- c(33, 33, 43, 55, 48, 37, 43, 24)
y <- c(37, 38, 42, 46, 46, 59, 41, 50)
library(lsa)
lsa::cosine(x, y)

x <- c(23, 24, 34, 35, 22, 25, 33, 24)
y <- c(10, 10, 22, 26, 16, 22, 11, 20)
z <- c(14, 15, 35, 16, 11, 23, 10, 41)
matrix <- cbind(x, y, z)
lsa::cosine(matrix)


##############
#install.packages("diagram")
#library(diagram)
plotmat(transition_matrix[1:3,1:3])




# Approach 2:-  load the package transition plot function from Gmisc package.
library(Gmisc)
library(grid)

# create a matrix for visualization,
no_boxes <- 3
transition_matrix      <- matrix(NA, nrow = no_boxes, ncol = no_boxes)
transition_matrix[1, ] <- 200 * c(.5, .25, .25)
transition_matrix[2, ] <- 540 * c(.75, .10, .15)
transition_matrix[3, ] <- 340 * c(0, .2, .80)

transition_matrix

# Transition plot in R
dev.off()
Gmisc::transitionPlot(transition_matrix,
               box_txt = c("First", "Second", "Third"),
               type_of_arrow = "simple",
               min_lwd = unit(1, "mm"),
               max_lwd = unit(6, "mm"),
               overlap_add_width = unit(1, "mm"))

##################
##################
data<- read_excel("file.xls") # data <- read_excel("file.xlsx")
data <- read_excel("my_file.xlsx", sheet = "sheetname")
data <- read_excel("file.xlsx", na = "---") # for missing values

file.list <- list.files(pattern='*.xlsx', recursive = TRUE) # files recurrsive dir

df.list <- lapply(file.list, readxl::read_excel)  # for multiple files

library("xlsx")# java-based solution
read.xlsx(file, sheetIndex, header=TRUE)  
read.xlsx2(file, sheetIndex, header=TRUE) # bigger files then read.xlsx2() 
data <- read.xlsx(file.choose(), 1)  # read first sheet
data <- read.xlsx("file.xlsx", 1)  # read first sheet
data <- read.xlsx(file.xlsx, sheetName=Sheet1)  # read the data contains in Sheet1
#clipboard

data <- read.table(file = "clipboard", sep = "\t", header=TRUE)
data <- read.table( pipe("pbpaste"),   sep="\t", header = TRUE) # macOS
# openxlsx::read.xlsx(file_path) # alternative to readxl 
XLConnect::readWorksheetFromFile(file_path, sheet = "list-column",
                                 startRow = 1, endRow = 10,  startCol = 1, endCol = 3)

# Reading several named regions
load <- loadWorkbook(file_path)
data <- readNamedRegion(load, name_Region_1, ...)
data2 <- readNamedRegion(load, name_Region_2, ...)

data <- read_excel(file.choose())
# Sys.getenv("JAVA_HOME")  # if java errors
# Sys.setenv(JAVA_HOME = "path_to_jre_java_folder")

# create a subfolder ,  paste command and can set the working directory.
setwd(paste0(getwd(), "/SubFolderName"))
list.files (path = "D:/RStudio/Foldername/") # full.name = TRUE)
list.files(recursive = TRUE)
list.files(path = choose.dir())
file.exists("rawdata.csv")
file.create("new_word.docx")
sapply(paste0("file", 1:N, ".csv"), file.create)
file_chmod() # Change file permissions
file_chown() # Change owner or group of a file
file.copy("D:/RStudio/source_file_tocopy.txt", "D:/RStudio/NewFolder/")
# dir_copy(), link_copy(): Copy files, directories or links
list.files("D:/path/to/somewhere/else", recursive = TRUE)
list.files(pattern = ".csv", recursive = TRUE)
ClubbedFile <- lapply(list.files(pattern = ".csv"), read.csv)

snapshot <- fileSnapshot()
file.info("myfile.csv")
unlink("myfile.csv")
file.remove("myfile.csv")
unlink("some_directory", recursive = TRUE)
#file_delete(), dir_delete(), link_delete(): Delete files, directories, or links

# stringr
statement<-c("R", "is powerful", "tool", "for data", "analysis")

str_length(statement)
str_c(statement,collapse=" ")  # concate strings
str_c("test",1:10, sep="-")

str_replace_na(c("My Name", NA, "Jhon"),"N/A.") # Replace na
str_sub(statement,1,5)
str_split(statement,pattern=" ") # split based on pattern
str_subset(colors(),pattern="^orange|red$")
str_extract(list,pattern="[a-z]+")  # full word

str_to_lower(statement)
str_to_upper(statement)
str_to_title(statement) 
# apply family in r apply(), lapply(), sapply(), mapply() and tapply()


ifelse( !dir.exists("Images") ,   dir.create("Images") ,  "Folder exists already" )











####################


### Load the Needed Libraries
library(ggplot2)
library(ggQC)

### Make up some demo data (load your file here instead)
set.seed(5555)
Process_Data <-  data.frame(  Process=rep(c("A"), each = 30), #Process A 
                              Run_Number=c(1:30),             #Run Order    
                              Value = c(rnorm(n = 30, mean = 30.5, sd = 1)) #Process A Random Data
                           )

### Make the plot
XmR_Plot <- 
  ggplot(Process_Data, aes(x = Run_Number, y = Value)) + #init ggplot
  geom_point() + geom_line() + # add the points and lines
  stat_QC(method = "XmR",      # specify QC charting method
          auto.label = T,      # Use Autolabels
          label.digits = 2,    # Use two digit in the label
          show.1n2.sigma = T   # Show 1 and two sigma lines
  ) +  
  scale_x_continuous(expand =  expansion(mult = .15))  # Pad the x-axis

### Draw the plot - Done
XmR_Plot

#load your data
Data4Pareto <- data.frame(
  KPI = c("Customer Service Time", "Order Fulfillment", "Order Processing Time",
          "Order Production Time", "Order Quality Control Time", "Rework Time",
          "Shipping"),
  Time = c(1.50, 38.50, 3.75, 23.08, 1.92, 3.58, 73.17)
) 

#make the plot
ggplot(Data4Pareto, aes(x=KPI, y=Time)) +
  stat_pareto(point.color = "red",
              point.size = 3,
              line.color = "black",
              bars.fill = c("blue", "orange")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

#done




# Load Libraries ----------------------------------------------------------
require(ggQC)
require(ggplot2)

# Setup Data --------------------------------------------------------------
set.seed(5555)
Process1 <- data.frame(processID = as.factor(rep(1,100)),
                       metric_value = rnorm(100,0,1),
                       subgroup_sample = rep(1:20, each=5),
                       Process_run_id = 1:100)
set.seed(5556)
Process2 <- data.frame(processID = as.factor(rep(2,100)),
                       metric_value = rnorm(100,5, 1),
                       subgroup_sample = rep(1:10, each=10),
                       Process_run_id = 101:200)

Both_Processes <- rbind(Process1, Process2)

#############################
#  Example 1:  XmR Chart    #
#############################


EX1.1 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
  geom_point() + geom_line() + stat_QC(method="XmR") +
  stat_QC_labels(method="XmR", digits = 2) +
  facet_grid(.~processID, scales = "free_x")
#EX1.1

EX1.2 <- ggplot(Both_Processes, aes(x=Process_run_id, y = metric_value)) +
  stat_mR() + ylab("Moving Range") +
  stat_QC_labels(method="mR", digits = 2) +
  facet_grid(.~processID, scales = "free_x")
#EX1.2

#############################
#  Example 2:  XbarR Chart  #
#############################

EX2.1 <- ggplot(Both_Processes, aes(x = subgroup_sample,
                                    y = metric_value,
                                    group = processID)) +
  stat_summary(fun.y = "mean", color = "blue", geom = c("point")) +
  stat_summary(fun.y = "mean", color = "blue", geom = c("line")) +
  stat_QC(method = "xBar.rBar") + facet_grid(.~processID, scales = "free_x")
#EX2.1

EX2.2 <- ggplot(Both_Processes, aes(x = subgroup_sample,
                                    y = metric_value,
                                    group = processID)) +
  stat_summary(fun.y = "QCrange", color = "blue", geom = "point") +
  stat_summary(fun.y = "QCrange", color = "blue", geom = "line") +
  stat_QC(method = "rBar") +
  ylab("Range") +
  facet_grid(.~processID, scales = "free_x")
#EX2.2

#############################
#  Example 3:  p Chart      #
#############################
# p chart Setup -----------------------------------------------------------
set.seed(5556)
bin_data <- data.frame(
  trial=1:30,
  Num_Incomplete_Items = rpois(30, lambda = 30),
  Num_Items_in_Set = runif(n = 30, min = 50, max = 100))
bin_data$Proportion_Incomplete <- bin_data$Num_Incomplete_Items/bin_data$Num_Items_in_Set

# Plot p chart ------------------------------------------------------------
EX3.1 <- ggplot(data = bin_data, aes(x=trial,
                                     y=Proportion_Incomplete,
                                     n=Num_Items_in_Set)) +
  geom_point() + geom_line() +
  stat_QC(method = "p")
#EX3.1

#############################
#  Example 4:  u Chart      #
#############################
# u chart Setup -----------------------------------------------------------
set.seed(5555)
bin_data <- data.frame(
  trial=1:30,
  Num_of_Blemishes = rpois(30, lambda = 30),
  Num_Items_Inspected = runif(n = 30, min = 50, max = 100)
)
bin_data$Blemish_Rate <- bin_data$Num_of_Blemishes/bin_data$Num_Items_Inspected

# Plot u chart ------------------------------------------------------------
EX4.1 <- ggplot(data = bin_data, aes(x=trial,
                                     y=Blemish_Rate,
                                     n=Num_Items_Inspected)) +
  geom_point() + geom_line() +
  stat_QC(method = "u")
#EX4.1

#############################
#  Example 5:  np Chart     #
#############################
# np chart Setup -----------------------------------------------------------
set.seed(5555)
bin_data <- data.frame(
  trial=1:30,
  NumNonConforming = rbinom(30, 30, prob = .50))
Units_Tested_Per_Batch <- 60

# Plot np chart ------------------------------------------------------------
EX5.1 <- ggplot(data = bin_data, aes(trial, NumNonConforming)) +
  geom_point() +
  stat_QC(method = "np", n = Units_Tested_Per_Batch)
#EX5.1

#############################
#  Example 6:  c Chart     #
#############################
# c chart Setup -----------------------------------------------------------
set.seed(5555)
Process1 <- data.frame(Process_run_id = 1:30,
                       Counts=rpois(n = 30, lambda = 25),
                       Group = "A")
Process2 <- data.frame(Process_run_id = 1:30,
                       Counts = rpois(n = 30, lambda = 5),
                       Group = "B")

all_processes <- rbind(Process1, Process2)
# Plot C Chart ------------------------------------------------------------

EX6.1 <- ggplot(all_processes, aes(x=Process_run_id, y = Counts)) +
  geom_point() + geom_line() +
  stat_QC(method = "c", auto.label = TRUE, label.digits = 2) +
  scale_x_continuous(expand =  expand_scale(mult = .25)) +
  facet_grid(.~Group)
# EX6.1



################## CUSUM chart

library(tidyverse)
library(data.table)
library(cusumcharter)
library(ggExtra)


# make the link dynamic
part1 <- "https://www.opendata.nhs.scot/dataset/"
part2 <- "b318bddf-a4dc-4262-971f-0ba329e09b87/"
part3 <- "resource/427f9a25-db22-4014-a3bc-893b68243055/"
part4 <- "download/trend_ca_"
part5 <- ".csv"
today <- gsub('-','',as.character(Sys.Date()))

link <- paste0(part1, part2, part3, part4, today, part5, sep = '')


dates <- seq.Date(as.Date(Sys.Date()-27), as.Date(Sys.Date()), by = '1 day')

DT <- data.table::fread(link)
DT[, Date := as.character(Date)]
DT[, Date := as.IDate(Date, format = "%Y%m%d")]
positives <- DT[Date >= as.Date(Sys.Date() -28),.(Date, CAName, DailyPositive)][]


ggplot(positives,aes(Date, DailyPositive)) + 
  geom_line() + 
  geom_point() +
  facet_wrap(~ CAName, ncol = 4, scales = "free_y") + 
  theme_minimal() + 
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX()



p <- positives %>% 
  group_by(CAName) %>% 
  group_modify(~ cusum_control(.$DailyPositive), .keep = TRUE) %>% 
  ungroup() %>% 
  group_by(CAName) %>% 
  mutate(Date = dates) %>% 
  ungroup() %>% 
  cusum_control_plot(.,
                     xvar = Date,
                     facet_var = CAName, 
                     facet_scales = 'free_y',
                     title_text = "CUSUM Rolling 28 Day Positive Cases")

p <- p + facet_wrap(~CAName, ncol = 4, scales = 'free_y')
print(p)
