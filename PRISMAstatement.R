# PRISMA and CONSORT (Self-generating CONSORT diagram below)
# PRISMAstatement package for the R statistical software (R Core Team 2018) enables construction of a correct PRISMA flow diagram which can be generated as part of a reproducible workflow in R.
# PRISMAstatement it is now possible to use a reproducible workflow up-to-date PRISMA flow diagram of publication quality.
# To evaluate the reporting quality of randomized controlled trials (RCTs)

#install.packages("PRISMAstatement")
library(PRISMAstatement)

prisma
DiagrammeR::grViz(
   prisma_graph(
                found = found, 
                found_other = found_other, 
                no_dupes = no_dupes, 
                screened = screened, 
                screen_exclusions = 
                screen_exclusions, 
                full_text = full_text, 
                full_text_exclusions = full_text_exclusions, 
                qualitative = qualitative, 
                quantitative = quantitative, 
                labels = labels, 
                extra_dupes_box = extra_dupes_box, 
                dpi = dpi, 
                font_size = font_size, #  ...
             )
  )



  
prisma(found = 750,
       found_other = 123,
       no_dupes = 776, 
       screened = 776, 
       screen_exclusions = 13, 
       full_text = 763,
       full_text_exclusions = 17, 
       qualitative = 746, 
       quantitative = 319,
       width = 800, height = 800)


# stick closely to the PRISMA statement, but small deviations are common
# option of adding a box which simply calculates the number of duplicates removed.

prisma(found = 750,
       found_other = 123,
       no_dupes = 776, 
       screened = 776, 
       screen_exclusions = 13, 
       full_text = 763,
       full_text_exclusions = 17, 
       qualitative = 746, 
       quantitative = 319,
       extra_dupes_box = TRUE)



prisma(1000, 20, 270, 270, 10, 260, 20, 240, 107, font_size = 6)


# for publications separate high-quality PDF of the PRISMA flow chart.
prsm <- prisma(  found = 750,
                 found_other = 123,
                 no_dupes = 776,
                 screened = 776,
                 screen_exclusions = 13,
                 full_text = 763,
                 full_text_exclusions = 17,
                 qualitative = 746,
                 quantitative = 319,
                 width = 200, height = 200,
                 dpi = 36)

tmp_pdf <- tempfile()

PRISMAstatement:::prisma_pdf(prsm, tmp_pdf)
knitr::include_graphics(path = tmp_pdf)
unlink(tmp_pdf)



# CONSORT stands for Consolidated Standards of Reporting Trials 
#  encompasses initiatives by CONSORT Group 
#  to alleviate the problems arising from inadequate reporting of randomized controlled trials.

# Self-generating CONSORT diagram
library(consort) # easy to create CONSORT diag for transparent reporting of participant allocation in randomized, controlled clinical trials

# To generate consort diagram with data.frame, one should prepare a disposition data.frame.
consort_plot(data,
             orders,
             side_box,
             allocation = NULL,
             labels = NULL,
             coords = NULL,
             dist = 0.05,
             cex = 0.8,
             text_width = NULL,
             widths = c(0.1, 0.9))

#Single arm
out <- consort_plot(data = df,
                    orders = c(trialno = "Population",
                               exc1    = "Excluded",
                               arm     = "Allocated",
                               fow1    = "Lost of Follow-up",
                               trialno = "Finished Followup",
                               fow2    = "Not evaluable for the final analysis",
                               trialno = "Final Analysis"),
                    side_box = c("exc1", "fow1", "fow2"),
                    cex = 0.9)
plot(out)

#Two arms
out <- consort_plot(data = df,
                    orders = c(trialno = "Population",
                               exc    = "Excluded",
                               arm     = "Randomized patient",
                               fow1    = "Lost of Follow-up",
                               trialno = "Finished Followup",
                               fow2    = "Not evaluable",
                               trialno = "Final Analysis"),
                    side_box = c("exc", "fow1", "fow2"),
                    allocation = "arm",
                    coords = c(0.4, 0.6),
                    labels = c("1" = "Screening", "2" = "Randomization",   "5" = "Final"))

plot(out)

#Three arms
g <- consort_plot(data = df,
                  orders = c(trialno = "Population",
                             exc    = "Excluded",
                             arm3     = "Randomized patient",
                             fow1    = "Lost of Follow-up",
                             trialno = "Finished Followup",
                             fow2    = "Not evaluable",
                             trialno = "Final Analysis"),
                  side_box = c("exc", "fow1", "fow2"),
                  allocation = "arm3",
                  labels = c("1" = "Screening", "2" = "Randomization",
                             "5" = "Final"))
plot(g)



# Provide text human efforts
library(grid)
options(txt_gp = gpar(cex = 0.8)) # Might want to change some settings

txt1      <- "Population (n=300)"
txt1_side <- "Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)"

# supports pipeline operator
g <- add_box(txt = txt1) |>
  add_side_box(txt = txt1_side) |> 
  add_box(txt = "Randomized (n=200)") |> 
  add_split(txt = c("Arm A (n=100)", "Arm B (n=100)")) |> 
  add_side_box(txt = c("Excluded (n=15):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)\n\u2022 Other (n=8)",
                       "Excluded (n=7):\n\u2022 MRI not collected (n=3)\n\u2022 Tissues not collected (n=4)")) |> 
  add_box(txt = c("Final analysis (n=85)", "Final analysis (n=93)")) |> 
  add_label_box(txt = c("1" = "Screening",
                        "3" = "Randomized",
                        "4" = "Final analysis"))
plot(g)
