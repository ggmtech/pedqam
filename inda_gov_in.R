# Data from data.gov.in portal using API title, description, organization type, organization (ministry), sector and sources. 
library(datagovindia)
get_list_of_organizations() %>%  View() #  head
get_list_of_sectors() %>% View()
##Single Criteria
search_api_by_title(title_contains = "railways") %>% View() #head(2)

##Multiple Criteria
dplyr::intersect(search_api_by_title(title_contains = "pollution"),
                 search_api_by_organization(organization_name_contains = "pollution"))  %>% View()

# API  “index_name” of that API, eg “0579cf1f-7e3b-4b15-b29a-87cf7b7c7a08” corresponds to the API for “Details of Comprehensive Environmental Pollution Index (CEPI) Scores .. 

# index_name will be essential for both getting to know more about the API or to even get data from it.
get_api_info(api_index = "0579cf1f-7e3b-4b15-b29a-87cf7b7c7a08")
get_api_fields(api_index = "0579cf1f-7e3b-4b15-b29a-87cf7b7c7a08")

get_api_fields(api_index = "3eee26fe-d259-460e-a912-feb293f6dcf8") # for Railways elephants

# The function get_api_data is really the powerhouse in this package which 
# allows one to do things over and above a manually constructed API query can do by utilizing the data.frame structure of the underlying data.
#  one command through the wrapper can make multiple requests and append the results from these requests at the same time.

# we first need to validate our API key relieved from data.gov.in. 
# To get the key, you need to register first register and then get the key from your “My Account” page after logging in. 
# More instruction can be found on this official guide. Once you get your API key, you can validate it as follows (only need to do this once per session) :
##Using a sample key
register_api_key("579b464db66ec23bdd000001cdd3946e44ce4aad7209ff7b23ac571b")
#> Connected to the internet
#> The API key is valid and you won't have to set it again

# Once you have your key registered, you are ready to extract data from a chosen API. Here is what each argument means :
# api_index : index_name of the chosen API (found by using search functions)
# results_per_req : Results per request sent to the server ; can take integer values or the string “all” to get all of the available data
# filter_by : A named character vector of field id (not the name) - value(s) pairs ; can take multiple fields as well as multiple comma separated values
# field_select : A character vector of fields to select only a subset of variables in the final data.frame
# sort_by : Sort by one or multiple fields



