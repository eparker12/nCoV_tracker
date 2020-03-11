## Covid-2019 interactive mapping tool: automated data processing script
## Quentin LeClerc & Edward Parker, London School of Hygiene & Tropical Medicine, March 2019

## data extracted from WHO situation reports obtained from following Github repository
# https://github.com/eebrown/data2019nCoV

# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# load latest Covid-2019 case data
WHO_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data-raw/WHO_SR.csv"))
write.csv(WHO_cases, "input_data/who_data.csv")

# load China region deaths
china_region_deaths = as.data.frame(data.table::fread("input_data/china_region_deaths.csv"),row.names = 1) # use when testing script offline
if (all(china_region_deaths$SituationReport == WHO_cases$SituationReport)) {
  WHO_cases = cbind(WHO_cases, china_region_deaths[,c("China-Taipei-deaths","China-HongKongSAR-deaths","China-Macao-deaths")])
} else { stop("Error: data incomplete for Hong Kong, Macao, and Taiwan") }

# load exsiting dataset
cv_cases = read.csv("input_data/coronavirus.csv", check.names = F, encoding = "UTF-8", stringsAsFactors = F)
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
#cv_cases = cv_cases %>% filter(situation_report <= 43) # remove one or more reports when testing script

# identify situation report(s) not currently included in data
new_report = setdiff(unique(WHO_cases$SituationReport), unique(cv_cases$situation_report))

if (length(new_report)!=0) {
  # loop to add new data for each new situation report
  for (i in c(1:length(new_report))) {
    
    # collect data from previous day
    old_data = tail(cv_cases, length(unique(cv_cases$country))) 
    
    # build new dataframe to add updated data
    new_data = tail(cv_cases, length(unique(cv_cases$country))) # copy all existing countries
    new_data$new_cases = new_data$new_deaths = new_data$deaths = 0
    new_data$cases = NA
    
    # update the date and situation report number in new_data
    new_data$date = format(as.Date(WHO_cases$Date[new_report[i]]),"%d/%m/%Y")
    new_data$situation_report = new_report[i]
    
    # select columns with numbers of cases/deaths
    new_WHO_cases = WHO_cases[new_report[i],c(which(names(WHO_cases)=="China"):which(names(WHO_cases)=="InternationalConveyance"),
                                              which(names(WHO_cases)=="China-Macao"),which(names(WHO_cases)=="China-HongKongSAR"),which(names(WHO_cases)=="China-Taipei"))]
    new_WHO_deaths= WHO_cases[new_report[i],c(which(names(WHO_cases)=="China-deaths"):which(names(WHO_cases)=="InternationalConveyance-deaths"),
                                              which(names(WHO_cases)=="China-Macao-deaths"),which(names(WHO_cases)=="China-HongKongSAR-deaths"),which(names(WHO_cases)=="China-Taipei-deaths"))]
    
    # update column names in new_WHO_deaths to include country names only
    colnames(new_WHO_deaths) = colnames(new_WHO_deaths) %>% str_replace_all(., "-deaths", "") 
    
    # stop script if there is country in the WHO database without mapping reference data
    if (all(colnames(new_WHO_cases) %in% countries$WHO_ID)==FALSE) {
      stop(paste0("Error: mapping data lacking for the following countries: ",colnames(new_WHO_cases)[colnames(new_WHO_cases) %in% countries$WHO_ID==FALSE]))
    }
    
    # loop to update cases
    for (country_name in colnames(new_WHO_cases)) {
      
      # select reference row from new_data
      country_number = which(new_data$WHO_ID == country_name)
      
      # update cases for countries with existing cases
      if (length(country_number) > 0) {
        
        # error catching
        if (length(country_number) > 1) { stop("Error updating data: more than one occurrence of ", country_name, " detected") }
        
        # update case numbers
        new_data$new_cases[country_number] = new_WHO_cases[,country_name] - old_data$cases[country_number]
        new_data$cases[country_number] = new_WHO_cases[,country_name]
        
        # update death numbers
        if (country_name %in% colnames(new_WHO_deaths)) {
          new_data$new_deaths[country_number] = new_WHO_deaths[,country_name] - old_data$deaths[country_number]
          new_data$deaths[country_number] = new_WHO_deaths[,country_name]
        }
        
      } else {
        
        new_country_name = countries$country[which(countries$WHO_ID == country_name)]
        
        # add the new country to the dataset
        new_country_data = head(new_data, 1) # copy one line of the data to keep the formatting
        
        # update case numbers
        new_country_data$country = new_country_name
        new_country_data$WHO_ID = country_name
        new_country_data$cases = new_country_data$new_cases = new_WHO_cases[,country_name]
        
        # set deaths to 0 at initiation
        new_country_data$deaths = new_country_data$new_deaths = 0
        
        # update deaths if country occurs in new_WHO_deaths dataframe
        if (new_country_name %in% colnames(new_WHO_deaths)) {
          new_country_data$deaths = new_country_data$new_deaths = new_WHO_deaths[,country_name]
        }
        print(new_country_data) # print to check
        new_data = rbind(new_data, new_country_data) # add the new country to the dataset
        
      }
    }
    
    # update data for Mainland China to exclude cases in Macao, Hong Kong, and Taiwan
    # calculate sum of cases and deaths outside mainland China
    non_mainland_china_cases = new_data %>% filter(country %in% c("Taiwan", "Hong Kong", "Macao")) %>% select(cases) %>% sum(.)
    non_mainland_china_deaths = new_data %>% filter(country %in% c("Taiwan", "Hong Kong", "Macao")) %>% select(deaths) %>% sum(.)
    
    # update mainland China counts to subtract these
    new_data$cases[which(new_data$country == "Mainland China")] = new_data$cases[which(new_data$country == "Mainland China")] - non_mainland_china_cases
    new_data$deaths[which(new_data$country == "Mainland China")] = new_data$deaths[which(new_data$country == "Mainland China")] - non_mainland_china_deaths
    
    # recalculate mainland China new_cases and new_deaths based on updated values
    new_data$new_cases[which(new_data$country == "Mainland China")] = new_data$cases[which(new_data$country == "Mainland China")] - old_data$cases[which(old_data$country == "Mainland China")]
    new_data$new_deaths[which(new_data$country == "Mainland China")] = new_data$deaths[which(new_data$country == "Mainland China")] - old_data$deaths[which(old_data$country == "Mainland China")]
    
    # allow for repatriation or reassigned cases without negative new_cases and new_deaths counts
    new_data$new_cases[new_data$new_cases<0] = 0
    new_data$new_deaths[new_data$new_deaths<0] = 0
    
    # merge new data with existing one
    cv_cases = rbind(cv_cases, new_data)
    
    # discard blank columns
    cv_cases$keep = as.numeric(cv_cases$cases>0 | cv_cases$deaths>0)
    cv_cases = subset(cv_cases, keep==1)
    cv_cases = cv_cases %>% select(-c("keep"))
  }
  
  # update indices
  rownames(cv_cases) = cv_cases$ind = 1:nrow(cv_cases)
  
  # re-order
  cv_cases = cv_cases[order(as.Date(cv_cases$date, format="%d/%m/%Y"), -cv_cases$cases, cv_cases$country),]
  
  # update time stamp
  cv_cases$last_update = NA
  cv_cases$last_update[nrow(cv_cases)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %H:00"), "GMT")
  
  # save file
  write.csv(cv_cases, "input_data/coronavirus.csv", row.names=F)
  rm(new_data, old_data, new_WHO_cases, new_WHO_deaths, country_name, country_number, WHO_cases, new_country_data, china_region_deaths, countries, cv_cases)
} else { print("Up to date!")}

