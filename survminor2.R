library("survival")
library("survminer")

# ggsurvplot(), ggsurvplot_list() , ggsurvplot_facet(), ggsurvplot_group_by(), ggsurvplot_add_all(), ggsurvplot_combine()

fit1 <- survfit( Surv(time,  status) ~ 1,                  data = lung)
fit2 <- survfit( Surv(time,  status) ~ sex,                data = lung)
fit3 <- survfit( Surv(time,  status) ~ sex + rx + adhere,  data = colon )
fit4 <- survfit( Surv(time,  status) ~ rx + adhere,        data = colon )


###### Fit a Cox proportional hazards model

# ?surv_object <- Surv(time,  status)
# ?fit.coxph <- survfit( coxph(surv_object, status) ~ 1,   data = ovarian)
#? ggforest(fit.coxph, data = ovarian)
#
#################################################
ggsurvplot(fit2 )   # most basic plot of fitted data
ggsurvplot_facet(fit2 , data = colon ,  facet.by = "adhere") + theme_bw()  


# Customise   Change color, linetype by strata, risk.table color by strata

ggsurvplot(fit4, 
            # fun = "event",
            fun = "cumhaz",        #fun = function(y) y*100 ,
            pval = TRUE, conf.int = TRUE,
           
            risk.table = TRUE, # Add risk table
            risk.table.col = "strata", # Change risk table color by groups
           
            linetype = "strata", # Change line type by groups
            # palette = c("#E7B800", "#2E9FDF")  # match the variables
            ggtheme = theme_bw() # Change ggplot2 theme
           )

############################################################
ggsurvplot_facet(fit2 , data = colon , facet.by = "adhere",  palette = "jco", pval = TRUE) + theme_bw()
ggsurvplot_facet(fit2 , data = colon , facet.by = "adhere" ) + theme_bw()  

colon
ggplot(data = colon) + #geom_dotplot( aes( y= age, x = time) )
                       geom_point(aes( y= age, x = time, col = status) ) 
    

####### Combine curves 
fit <- list(PFS = fit1, OS = fit2)
ggsurvplot_combine(fit, lung)



############################################################

ggsurvplot(
            fit2,                     # survfit object with calculated statistics.
            # fun = "event",
            # fun = "cumhaz",
            fun = function(y) y*100 ,
            linetype = "strata",     # change line type by groups
            size = 1,                # change line size
            data = lung,             # data used to fit survival curves.

   
            pval = TRUE,             # show p-value of log-rank test.
            conf.int = TRUE,         # show confidence intervals for  point estimates of survival curves.
            # ?conf.int.fill = "blue",
            # palette = c("#E7B800", "#2E9FDF"), # custom color palette, match varibles
            palette = "Dark2",
            xlim = c(0,500),         # present narrower X axis, but not affect survival estimates.
            xlab = "Time in days",   # customize X axis label.
            break.time.by = 100,     # break X axis in time intervals by 500.
            #ggtheme = theme_light(), # customize plot and risk table with a theme.
            ggtheme = theme_bw(),
            
            ncensor.plot = TRUE,      # plot the number of censored subjects at time t
            ncensor.plot.height = 0.25,
            conf.int.style = "step",  # customize style of confidence intervals
   
            font.main = c(16, "bold", "darkblue"),
            font.x = c(14, "bold.italic", "red"),
            font.y = c(14, "bold.italic", "darkred"),
            font.tickslab = c(12, "plain", "darkgreen"),
           # legend = "bottom", 
            legend = c(0.2, 0.2),
            legend.title = "Sex",
            legend.labs = c("Male", "Female"),
              
            # surv.median.line = "hv",  # add the median survival pointer. c("none", "hv", "h", "v")
            # legend = "bottom" , 
            # legend.labs =      c("Male", "Female"),    # change legend labels.
   
            risk.table = TRUE,       # show risk table.
            # tables.theme = theme_cleantable(),
            risk.table.col = "strata",
            risk.table.y.text.col = T,# colour risk table text annotations.
            risk.table.height = 0.25, # the height of the risk table
            risk.table.y.text = FALSE # show bars instead of names in text annotations in legend of risk table.
        )    -> ggsurv

ggsurv

# Changing Labels
# %%%%%%%%%%%%%%%%%%%%%%%%%%
# Labels for Survival Curves (plot)
ggsurv$plot <- ggsurv$plot + labs(     title    = "Survival curves",                     
                                       subtitle = "Based on Kaplan-Meier estimates",  
                                       caption  = "created with survminer"          )

# Labels for Risk Table 
ggsurv$table <- ggsurv$table + labs(   title    = "Note the risk set sizes",          
                                       subtitle = "and remember about censoring.", 
                                       caption  = "source code: website.com"        )

# Labels for ncensor plot 
ggsurv$ncensor.plot <- ggsurv$ncensor.plot + labs(   title    = "Number of still not failed / censured", 
                                                     subtitle = "over the time.",
                                                     caption  = "source code: website.com"  )

ggsurv

# Changing the font size, style and color
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Applying the same font style to all the components of ggsurv:
# survival curves, risk table and censor part

ggsurv <- ggpar(  
    ggsurv,
    font.title    = c(16, "bold", "darkblue"),         
    font.subtitle = c(15, "bold.italic", "purple"), 
    font.caption  = c(14, "plain", "orange"),        
    font.x        = c(14, "bold.italic", "red"),          
    font.y        = c(14, "bold.italic", "darkred"),      
    font.xtickslab = c(12, "plain", "darkgreen"),
    legend = "top"
)

ggsurv


# Using specific fonts for risk table and ncensor plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Font for Risk Table
ggsurv$table <- ggpar(      ggsurv$table,
                            font.title    = c(13, "bold.italic", "green"),
                            font.subtitle = c(15, "bold", "pink"),
                            font.caption  = c(11, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(11, "bold.italic", "darkgreen"),
                            font.xtickslab = c(9, "bold", "red")
                       )


# Font for ncensor plot
ggsurv$ncensor.plot <- ggpar(
                         ggsurv$ncensor.plot,
                            font.title    = c(13, "bold.italic", "green"),
                            font.subtitle = c(15, "bold", "pink"),
                            font.caption  = c(11, "plain", "darkgreen"),
                            font.x        = c(8, "bold.italic", "orange"),
                            font.y        = c(11, "bold.italic", "darkgreen"),
                            font.xtickslab = c(9, "bold", "red")
                    )

ggsurv

print(ggsurv)



###########
ggsurvplot()          #: Draws survival curves , 'number at risk' table,  cumulative number of events table and the cumulative number of censored subjects table.
arrange_ggsurvplots() #: Arranges multiple ggsurvplots on the same page.
ggsurvevents()        #: Plots the distribution of event's times.
surv_summary()        #: Summary of a survival curve. Compared to the default summary() function, surv_summary() creates a data frame containing a nice summary from survfit results.
surv_cutpoint()       #: Determines the optimal cutpoint for one or multiple continuous variables at once. Provides a value of a cutpoint that correspond to the most significant relation with survival.
pairwise_survdiff()   #: Multiple comparisons of survival curves. Calculate pairwise comparisons between group levels with corrections for multiple testing.

# Diagnostics of Cox Model
ggcoxzph()          #: Graphical test of proportional hazards. Displays a graph of the scaled Schoenfeld residuals, along with a smooth curve using ggplot2. Wrapper around plot.cox.zph().
ggcoxdiagnostics()  #: Displays diagnostics graphs presenting goodness of Cox Proportional Hazards Model fit.
ggcoxfunctional()   #: Displays graphs of continuous explanatory variable against martingale residuals of null cox proportional hazards model. It helps to properly choose the functional form of continuous variable in cox model.
# Summary of Cox Model
ggforest()          #: Draws forest plot for CoxPH model
ggadjustedcurves()  #: Plots adjusted survival curves for coxph model.
# Competing Risks
ggcompetingrisks() #: Plots cumulative incidence curves for competing risks.

# more at http://www.sthda.com/english/rpkgs/survminer/, and check out the documentation and usage 

###################################################################

# Facet by one grouping variables: rx

fit <- survfit(  Surv(time, status) ~ sex,  data = colon ) 

ggsurvplot_facet(fit,  colon,   facet.by = "rx"  ,    palette = "jco", pval = TRUE)


# Facet by two grouping variables: rx and adhere
ggsurvplot_facet(fit,  colon,  facet.by = c("rx", "adhere"),   palette = "jco", pval = TRUE)


# Another fit
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::
fit2 <- survfit( Surv(time, status) ~ sex + rx,  data = colon )
ggsurvplot_facet(fit2, colon, facet.by = "adhere",  palette = "jco", pval = TRUE)


# Faceting
ggsurv <- ggsurvplot(fit3, data = colon,
                        fun = "cumhaz", conf.int = TRUE,
                        risk.table = TRUE, risk.table.col="strata",
                        ggtheme = theme_bw())
ggsurv                     
# Faceting survival curves
curv_facet <- ggsurv$plot + facet_grid(rx ~ adhere)
curv_facet
                     # 
# # Faceting risk tables:
# # Generate risk table for each facet plot item

ggsurv$table + facet_grid(rx ~ adhere, scales = "free") +   theme(legend.position = "none")

#  # Generate risk table for each facet columns
tbl_facet <- ggsurv$table + facet_grid(.~ adhere, scales = "free")
                            tbl_facet  + theme(legend.position = "none")
                     # 
# # Arrange faceted survival curves and risk tables
 g2 <- ggplotGrob(curv_facet)
 g3 <- ggplotGrob(tbl_facet)
min_ncol <- min(ncol(g2), ncol(g3))
g <- gridExtra::rbind.gtable(g2[, 1:min_ncol], g3[, 1:min_ncol], size="last")
g$widths <- grid::unit.pmax(g2$widths, g3$widths)
grid::grid.newpage()
grid::grid.draw(g)
                     # 

