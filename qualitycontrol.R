# qa calibration
# qcc is a contributed R package for statistical quality control charts which provides:
# Shewhart quality control charts for continuous, attribute and count data
# Cusum and EWMA charts
# Operating characteristic curves
# Process capability analysis
# Pareto chart and cause-and-effect chart
# Multivariate control charts.

library(qcc)

data(pistonrings)
attach(pistonrings)
diameter <- qcc.groups(diameter, sample)
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

##
##  Attribute data 
##

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