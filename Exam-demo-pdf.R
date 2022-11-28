## exams ----------------------------------------------------------------------------
library("exams")   ## load package

## exam with a simple vector of exercises in R/Markdown (.Rmd) format
myexam <- c("tstat.Rmd", "tstat2.Rmd", "boxplots.Rmd", "function.Rmd", "boxhist.Rmd", "currency8.Rnw")
## note that the currency exercise is in UTF-8 encoding


## exams2pdf ------  PDF output (1 file per exam)
## -> typically used for quickly checking if an exercise can be converted to PDF
## -> or customized via suitable templates

## generate PDF version of a single exercise , PDF viewer) with default settings
exams2pdf("exercises/tstat.Rmd")

## generate a single PDF exam (shown in PDF viewer)
## with specification of a template (for an exam) %s encoding
exams2pdf(myexam, n = 1,
  encoding = "UTF-8",
  edir = "exercises",
  template = "templates/solution.tex")

## generate three PDF exams and corresponding solutions in output directory
## (with the header used to set a custom Date and ID for the exam)
exams2pdf(myexam, n = 3, name = c("pdf-exam", "pdf-solution"),
  encoding = "UTF-8",
  dir = "output",
  edir = "exercises",
  template = c(#"templates/fruite.tex",
               "templates/solution.tex"),
  header = list(
    Date = "2015-01-01",
    ID = function(i) formatC(i, width = 5, flag = "0")
  ))


## ----------------------------------------------------------------------------------


library("exams")
myexam <- list(
  "tstat2.Rnw",
  "ttest.Rnw",
  "relfreq.Rnw",
  "anova.Rnw",
  c("boxplots.Rnw", "scatterplot.Rnw"),
  "cholesky.Rnw"
)

set.seed(403)
ex1 <- exams2nops(myexam, n = 2,
                  dir = "nops_pdf", name = "demo", date = "2015-07-29",
                  points = c(1, 1, 1, 2, 2, 3), showpoints = TRUE)
getwd()
dir("nops_pdf")
getwd()
## [1] "demo.rds"  "demo1.pdf" "demo2.pdf"
