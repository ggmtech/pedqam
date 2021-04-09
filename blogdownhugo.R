## Install from CRAN
install.packages("blogdown")
## Or, install from GitHub
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("rstudio/blogdown")

library(blogdown)

blogdown::install_hugo()
#For macOS users, install_hugo() uses the package manager Homebrew (https://brew.sh) 
#if it has already been installed, otherwise it just downloads the Hugo binary directly.

blogdown::hugo_version()

# To upgrade or reinstall Hugo, 
# blogdown::update_hugo()  # equivalent to install_hugo(force = TRUE)

# create a new project under a new directory in the RStudio IDE 
# File -> New Project
# and call the function in the R console of the new project:
blogdown::new_site()
