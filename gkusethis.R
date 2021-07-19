# Dev
library(devtools)   ## or limited library(usethis)
use_devtools()

usethis::edit_r_profile()  # open for edit

options(
    usethis.full_name = "Gopal kumar",
    usethis.protocol  = "ssh",
    usethis.description = list(  "Authors@R" = utils::person( "Gopal", 
                                                              "Kumar",
                                                              email   = "ggmtech@yahoo.co,in",
                                                              role    =  c("aut", "cre"),
                                                               comment = c(ORCID = "JANE'S-ORCID-ID")
                                                             ),
                                    Version = "0.0.0.1000"   
                                 ),
    usethis.destdir = "~/pedqam/projects",
    usethis.overwrite = TRUE
)

use_git_config(core.editor = "nano")
# git config --global core.editor emacs

#library(usethis) ## or library(devtools)
use_git_config(user.name = "ggmtech", user.email = "ggmtech@yahoo.co.in")
# The code chunk above is doing the equivalent of this:
# git config --global user.name 'Jane Doe'
# git config --global user.email 'jane@example.com'
# git config --global --list

# GitHub personal access token (PAT)  required  to use use_github(), create_from_github(..., fork = TRUE) etc


# check by running a git situation-report: 

proj_sitrep() #  info about the active usethis project, working directory, and the active RStudio Project. 
git_sitrep()  #  prints info about your current Git, gert, and GitHub setup.

# instal dev 
# devtools::install_github("OWNER/REPO").

pkgbuild::check_build_tools()
# On mac xcode-select --install. For installing almost anything else, consider using Homebrew.
# Windows: Install Rtools

