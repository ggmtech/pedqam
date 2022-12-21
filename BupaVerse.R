# some package tets

# install.packages("bupaverse")
# bupaR: Core package for business process analysis.
# edeaR: Exploratory and descriptive analysis of event-based data.
# eventdataR: Repository of sample process data.
# processcheckR: Rule-based conformance checking and filtering.
# processmapR: Visualise event-based data using, i.a., process maps.)
library(tidyverse)
library(bupaverse)

patients %>% View()

patients %>%
  processing_time(level = "activity") %>%
  plot()

patients %>%
  process_map()

# Dotted chart
patients %>%
  dotted_chart(x = "absolute", sort = "start")

processmapR::trace_explorer
processmapR::resource_matrix %>% plot 

# bupaR::eventlog(eventlog = data,
#                 case_id = “order_number”,
#                 activity_id = “activity”,
#                 activity_instance_id = “activity_nr”,
#                 timestamp = “time”,
#                 lifecycle_id = “status”,
#                 resource_id = “originator”)
# bupaR::simple_eventlog(eventlog = data,
#                        case_id = “order_number”,
#                        activity_id = “activity”,
#                        timestamp = “time”)
# bupaR::isimple_eventlog(eventlog = data)
# bupaR::ieventlog(eventlog = data)

# # An eventlog-object can be created using the eventlog function. This function needs as arguments a data.frame
# and the column names of the appropriate fields describing the case identifier, activity identifier, timestamp, lifecycle transition, resource and an activity instance identifier.
# An event log with minimal requirements (timestamp, case and activity identifier) can be created with the simple_eventlog function.
# Both functions have an alternative, prefixed with the letter i for interface, in order to configure the identifiers
# with a GUI. In that case only the data needs to be provided as argument. 

# install.packages(“bupaR”)
install.packages(eventdataR)
# install.packages(“xesreadR”)
# install.packages(“edeaR”)
# install.packages(“processmapR”)
# install.packages(“processmonitR”)
#devtools::install_github("gertjanssenswillen/<package>")
library(bupaR)

eventlog %>%   group_by(label, gender) %>%   throughput_time("log")

bupaR::summary(eventlog)
bupaR::mutate(eventlog, ...)
bupaR::traces(eventlog)
bupaR::cases(eventlog)
bupaR::activities(eventlog)
bupaR::resources(eventlog)
bupaR::mapping(eventlog)
bupaR::n_activities(eventlog)
bupaR::n_activity_instances(eventlog)
bupaR::n_cases(eventlog)
bupaR::n_events(eventlog)
bupaR::n_resource(eventlog)
bupaR::n_traces(eventlog)
bupaR::activity_id(eventlog)
bupaR::activity_instance_id(eventlog)
bupaR::case_id(eventlog)
bupaR::lifecycle_id(eventlog)
bupaR::resource_id(eventlog)
bupaR::timestamp(eventlog)

