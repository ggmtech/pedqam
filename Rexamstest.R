#install.packages("exams", dependencies = TRUE)
install.packages("tinytex")
# modern operating systems and browsers are setup to trust hundreds of companies and recent years several such CAs have been found untrustworthy.
#
tinytex::install_tinytex()
options(exams_tex = "tools") # since unable to install ,using my latex

library("exams")
exams::exams_skeleton(markup = "markdown", encoding = "UTF-8",
               writer = c("exams2html", "exams2pdf", "exams2moodle")) #intall examples
getwd()
dir()

# Exam q file in Rmd
exams2html("swisscapital.Rmd")
exams2pdf("swisscapital.Rmd")


getwd()

library("exams")

set.seed(1090)
exams2html("ttest.Rmd")
set.seed(1090)
exams2pdf("ttest.Rmd")

exams2html("demo-all.R")

# splitting files ,  check out >man split
# split -l 5000 -d --additional-suffix=.txt $FileName file
# where
# -l 5000: split file into files of 5,000 lines each.
# -d: numerical suffix makes the suffix go from 00 to 99 by default instead of aa to zz.
# --additional-suffix: lets you specify the suffix, here the extension
# $FileName: name of the file to be split.
# file: prefix to add to the resulting files.
# for Mac, insead install the GNU version : brew install coreutils
# use the -t flag which splits on a user-specified delimiter instead of a newline.
#  You can then use the -l flag to specify how many splits you want to group together 
