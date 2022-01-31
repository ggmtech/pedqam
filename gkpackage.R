


# package dev
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
library("devtools", "roxygen2", "testthat", "knitr")
devtools::has_devel()   # Your system is ready to build packages!

# Open a new Rstudio session and create a new project in a suitable directory (File>New Project). 

# Next  use create_package() to create the basic file structure of the package. 
# At their essence, R packages are nothing more than directories (folders) and text files

# Create the file structure
create_package("~/projects/packages/qamqc") ## write the path to your WD

use_git()


# Write a function
# Functions are the main component of your R package. 
# You can use R functions, but sometimes more powerful code in another language (such as C or C#). 
# use devtools create the file for us with the use_r fuction:
  
use_r("hello") # Will create a file called R/hello.R    

# The use_r code will create a new file inside R/ called hello.R. 

# Copy this code into the file and save.
hello <- function(name){
  message <- paste0("Hello ", name, "!")
  
  return(message)
}
                                                                                                   
# The functions you write will live in an Rscript file (a file that ends in .R). 
# Your function files will be stored in the R/ directory. Don’t place any other files there. 
# While you can have more than one function per file, a common convention is to use only one function per file.
# 

# To test your new function, you could source that file, # NOt good
# beter actually loaded a package, use the load_all function. 
# You can then run your function and you should get the following result.
load_all()
hello("Patricio")
#> [1] "Hello Patricio!"
#Now commit your changes using the Git panel or the terminal (it is a good idea to commit your changes often).
#
#R has an integrated mechanism to check whether everything  is working correctly. 
#We can run this using the check function.

check()

# DESCRIPTION file
# The DESCRIPTION file is one of the most important files in the R package,
#  it is there to summarise the main information about the package and it is what tells R that this directory is a package.
# When you open it, you will see a template with some information. Update the Title, Author and Description fields. (Don’t worry about the other fields )


# use the roxygen2 package to automatically create documentation for our functions.
# open hello.R, place the cursor inside the function, and click on Code>Insert Roxygen Skeleton. 
# You will see new lines of code starting with #'. 
# These lines are special comments that are ‘read’ by the document function to create the documentation files. 
# Edit the information on the hello.R file to something like this:

#' hello
#'
#' @param name A name
#'
#' @return A message
#' @export
#'
#' @examples hello("Alice")

# Save the file and run the document function. 
# If you then run ?hello, you should get the function’s documentation in Rstudio’s Help viewer

document()
?hello

# check()  to see no errors
# You install your package with the install function 
install()

# Tests are not strictly necessary, but it is very good practice to test your functions.
# testthat package makes it very easy to add built-in tests to your package. 
# By calling the use_testthat function we add testthat to the package dependencies (more on that later), as well as create the directories and files for our tests.

use_testthat()

use_test("hello") # creates tests/testthat/test-hello.R
#Then edit the new file and write the following:
test_that("hello works", {
    test_name <- "Alice"
    test_message <- paste0("Hello ", test_name, "!")
    
    expect_identical(test_message, hello(test_name))
  })

# Now run the test function and see if your functions pass your tests.
test()


# Importing functions from another package
# it makes more sense to use that instead of developing your own function.
# 
# Use Github
# By now you could install your toypackage in your system and call it day. But we will now see how to publish our package on Github. This is not necessary to make your package work, but it is a great tool if you ever want anyone else to use your package (or even to install it in a different computer).

# Use this command if you have SSH keys associated with Github
use_github(protocol = "ssh")

# Use this command if you don't
use_github()

# Next we will create a README file so that visitors to your Github repo know what your package is about.
# use_readme_rmd()
