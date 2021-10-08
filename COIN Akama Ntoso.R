# Akoma Ntoso :“linked hearts” in Akan language W.Africa  defines simple technology-neutral electronic representations in XML format of parliamentary, legislative and judiciary documents. OASIS:AUGUST 2018


library(COINr) # https://cran.r-project.org/web/packages/COINr/vignettes/Overview.html
ASEM <- COINr::build_ASEM()
print(class(ASEM))
View(ASEM)

COINr::plotframework(ASEM)  # nice pie !
COINr::indDash(ASEM)        # simply displays illustrative distributions of existing indicators.

# get stats
ASEM <- COINr::getStats(ASEM, dset = "Raw", out2 = "COIN") # getStats() returns statistics

stat_tab <- ASEM$Analysis$Raw$StatTable[1:8]  # display stats table, first few columns/rows only
roundDF(head(stat_tab, 5))

# create denominated data set
ASEM <- COINr::denominate(ASEM, dset = "Raw", specby = "metadata")

# imputing mising data
ASEM <- COINr::impute(ASEM, dset = "Denominated", imtype = "indgroup_mean", groupvar = "Group_GDP")

COINr::iplotBar(ASEM, dset = "Aggregated", isel = "Index", aglev = 4, stack_children = T)
# sub-index values can be plotted against each other as follows:
plt <- COINr::iplotIndDist2(ASEM, dsets = "Aggregated", icodes = c("Conn", "Sust"), aglevs = 3)
plotly::layout(plt, xaxis = list(title = "Connectivity"), yaxis = list(title = "Sustainability"))

COINr::iplotMap(ASEM, dset = "Aggregated", isel = "Conn")
rslts <- COINr::getResults(ASEM, tab_type = "Summary")
head(rslts, 10)

# Export entire COIN to Excel
COINr::coin2Excel(ASEM, "ASEM_results.xlsx")

# This will take a few minutes to run
SAresults <- sensitivity(ASEM, v_targ = "Index",
                         SA_specs = SAspecs,
                         N = 500,
                         SA_type = "SA", Nboot = 1000)

COINr::plotSARanks(SAresults)
# plot bar chart
COINr::plotSA(SAresults, ptype = "bar")
# plot bar chart
COINr::plotSA(SAresults, ptype = "pie") + ggplot2::theme(text = ggplot2::element_text(size = 10))
COINr::plotSA(SAresults, ptype = "box") + ggplot2::theme(text = ggplot2::element_text(size = 8))
