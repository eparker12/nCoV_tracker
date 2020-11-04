## Covid-2019 interactive mapping tool: script to reformat JHU data from scratch
## Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine, March 2019

## data extracted from Johns Hopkins data obtained from following Github repository
# https://github.com/CSSEGISandData/COVID-19

# load libraries
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# function to update jhu input data according to mapping base format
update_jhu = function(input_df, tag) {
  names(input_df)[1:2] = c("Province", "Country")
  input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
  input_df$Country[input_df$Province=="Macau"] = "Macao"
  input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
  input_df$Country[input_df$Country=="Korea, South"] = "RepublicofKorea"
  input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
  input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
  input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
  input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
  input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
  input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
  input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
  input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
  input_df$Country = input_df$Country %>% str_replace_all(., " ", "") 
  dates = names(input_df)[which(names(input_df)=="1/22/20"):ncol(input_df)]
  input_df = input_df %>% 
    select(-c(Province, Lat, Long)) %>% 
    group_by(Country) %>% 
    summarise_each(funs(sum)) %>%
    data.frame()
  rownames(input_df) = input_df$Country
  rownames(input_df) = paste0(input_df$Country,"_",tag)
  input_df = input_df %>% select(-c(Country)) %>% t()
  input_df = data.frame(input_df)
  input_df$Date = dates
  rownames(input_df) = 1:nrow(input_df)
  input_df$Date = format(as.Date(input_df$Date,"%m/%d/%y"))
  input_df
}

# load latest Covid-2019 data: confirmed cases
jhu_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"))
jhu_cases = subset(jhu_cases, !is.na(Lat))
jhu_cases[is.na(jhu_cases)]=0
total_cases <- sum(jhu_cases[,ncol(jhu_cases)])
jhu_cases = update_jhu(jhu_cases, "cases")
if (total_cases!=sum(jhu_cases[nrow(jhu_cases),1:(ncol(jhu_cases)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# load latest Covid-2019 data: deaths
jhu_deaths <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"))
jhu_deaths = subset(jhu_deaths, !is.na(Lat))
jhu_deaths[is.na(jhu_deaths)]=0
total_deaths <- sum(jhu_deaths[,ncol(jhu_deaths)])
jhu_deaths = update_jhu(jhu_deaths, "deaths")
if (total_deaths!=sum(jhu_deaths[nrow(jhu_deaths),1:(ncol(jhu_deaths)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# load latest Covid-2019 data: recovered
jhu_rec <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"))
jhu_rec = subset(jhu_rec, !is.na(Lat))
jhu_rec[is.na(jhu_rec)]=0
total_rec <- sum(jhu_rec[,ncol(jhu_rec)])
jhu_rec = update_jhu(jhu_rec, "recovered")
if (total_rec!=sum(jhu_rec[nrow(jhu_rec),1:(ncol(jhu_rec)-1)])) { stop(paste0("Error: incorrect processing - total counts do not match")) }

# merge dataframes 
jhu_merge = merge(jhu_cases, jhu_deaths, by = "Date")
jhu_merge = merge(jhu_merge, jhu_rec, by = "Date")
jhu_merge$Date = as.Date(jhu_merge$Date, format="%Y-%m-%d")
jhu_merge$update = 1:nrow(jhu_merge)
write.csv(jhu_merge, "input_data/jhu_data.csv")

# load country data
countries = read.csv("input_data/countries_codes_and_coordinates.csv")

# check all jhu country names have corresponding country data
jhu_country_list = names(jhu_merge)[grepl("_cases", names(jhu_merge))] %>% str_replace_all(., "_cases", "") 
if (all(jhu_country_list %in% countries$jhu_ID)==FALSE) {
  stop(paste0("Error: mapping data lacking for the following countries: ",jhu_country_list[(jhu_country_list %in% countries$jhu_ID)==FALSE]))
}

collated_data = NULL
# loop to add new data for each new situation report
for (i in c(1:nrow(jhu_merge))) {
  
  # extract subset of data for date in row i
  jhu_subset = jhu_merge[i,]
  jhu_subset_cases = jhu_subset[,which(grepl("_cases", names(jhu_subset)))]
  jhu_subset_cases = jhu_subset_cases[,colSums(jhu_subset_cases)>0]
  jhu_subset_deaths = jhu_subset[,which(grepl("_deaths", names(jhu_subset)))]
  jhu_subset_rec = jhu_subset[,which(grepl("_recovered", names(jhu_subset)))]
  
  # build new dataframe to add updated data
  new_data = data.frame(jhu_ID = names(jhu_subset_cases) %>% str_replace_all(., "_cases", ""),
                        date = format(as.Date(jhu_subset$Date[1],"%Y-%m-%d")),
                        update = i,
                        cases = NA, new_cases = 0,
                        deaths = 0, new_deaths = 0,
                        recovered = 0, new_recovered = 0)
  
  # update column names in new_jhu dataframes to include country names only
  colnames(jhu_subset_cases) = colnames(jhu_subset_cases) %>% str_replace_all(., "_cases", "") 
  colnames(jhu_subset_deaths) = colnames(jhu_subset_deaths) %>% str_replace_all(., "_deaths", "") 
  colnames(jhu_subset_rec) = colnames(jhu_subset_rec) %>% str_replace_all(., "_recovered", "")
  
  # loop to update cases
  for (j in 1:nrow(new_data)) {
    # update case numbers
    country_name = as.character(new_data$jhu_ID[j])
    new_data$cases[j] = jhu_subset_cases[,country_name]
    new_data$deaths[j] = jhu_subset_deaths[,country_name]
    new_data$recovered[j] = jhu_subset_rec[,country_name]
  }
  
  # append new data to collated dataframe
  collated_data = rbind(collated_data, new_data)
  collated_data$jhu_ID = as.character(collated_data$jhu_ID)
  
  # calculate new cases, deaths and recoveries
  if (i == 1) {
    collated_data$new_cases = collated_data$cases
    collated_data$new_deaths = collated_data$deaths
    collated_data$new_recovered = collated_data$recovered
  }
  
  if (i > 1) {
    # split it into date i and date i-1
    today = subset(collated_data, update==i)
    yesterday = subset(collated_data, update==(i-1))
    
    for (k in 1:nrow(today)) {
      country_name = today$jhu_ID[k]
      
      # if present in yesterday's data, calculate new cases by subtraction
      if (country_name %in% yesterday$jhu_ID) {
        collated_data$new_cases[collated_data$jhu_ID==country_name & collated_data$update==i] = today$cases[today$jhu_ID==country_name] - yesterday$cases[yesterday$jhu_ID==country_name] 
        collated_data$new_deaths[collated_data$jhu_ID==country_name & collated_data$update==i] = today$deaths[today$jhu_ID==country_name] - yesterday$deaths[yesterday$jhu_ID==country_name] 
        collated_data$new_recovered[collated_data$jhu_ID==country_name & collated_data$update==i] = today$recovered[today$jhu_ID==country_name] - yesterday$recovered[yesterday$jhu_ID==country_name] 
      } else {
        # if absent from yesterday's data, new observations = total observations
        collated_data$new_cases[collated_data$jhu_ID==country_name & collated_data$update==i] = today$cases[today$jhu_ID==country_name] 
        collated_data$new_deaths[collated_data$jhu_ID==country_name & collated_data$update==i] = today$deaths[today$jhu_ID==country_name]  
        collated_data$new_recovered[collated_data$jhu_ID==country_name & collated_data$update==i] = today$recovered[today$jhu_ID==country_name] 
      }
    }
  }
}
# allow for repatriation or reassigned cases without negative new_cases, new_deaths and new_recovered counts
collated_data$new_cases[collated_data$new_cases<0] = 0
collated_data$new_deaths[collated_data$new_deaths<0] = 0
collated_data$new_recovered[collated_data$new_recovered<0] = 0

# add active case data (total cases - deaths/recovered)
collated_data$active_cases = collated_data$cases - (collated_data$deaths + collated_data$recovered)

# update country names
collated_data = merge(collated_data, countries[,c("jhu_ID", "country")], by = "jhu_ID")

# re-order
collated_data = collated_data[order(as.Date(collated_data$date, format="%Y-%m-%d"), -collated_data$cases, collated_data$country),]

# update time stamp
collated_data$last_update = NA
collated_data$last_update[nrow(collated_data)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))

# add rolling 7-day and 30-day averages for new cases and new deaths
collated_data$new_deaths_rolling30 = collated_data$new_deaths_rolling7 = collated_data$new_cases_rolling30  = collated_data$new_cases_rolling7 = NA
country_list = unique(collated_data$jhu_ID)

for (i in 1:length(country_list)) {
  country_sub = subset(collated_data, jhu_ID==country_list[i])

  # add rolling 7-day average from 7th day onwards
  if (nrow(country_sub)>=7) {
    for (j in 7:nrow(country_sub)) {
      country_sub$new_cases_rolling7[j] = round(mean(country_sub[(j-6):j,"new_cases"]),0)
      country_sub$new_deaths_rolling7[j] = round(mean(country_sub[(j-6):j,"new_deaths"]),0)
    }
  }
  
  if (nrow(country_sub)>=30) {
    for (j in 30:nrow(country_sub)) {
      country_sub$new_cases_rolling30[j] = round(mean(country_sub[(j-29):j,"new_cases"]),0)
      country_sub$new_deaths_rolling30[j] = round(mean(country_sub[(j-29):j,"new_deaths"]),0)
    }
  }
  
  # integrate with parent dataframe
  collated_data$new_cases_rolling7[collated_data$jhu_ID==country_list[i]] = country_sub$new_cases_rolling7
  collated_data$new_deaths_rolling7[collated_data$jhu_ID==country_list[i]] = country_sub$new_deaths_rolling7
  collated_data$new_cases_rolling30[collated_data$jhu_ID==country_list[i]] = country_sub$new_cases_rolling30
  collated_data$new_deaths_rolling30[collated_data$jhu_ID==country_list[i]] = country_sub$new_deaths_rolling30
  
}
  

# save file
write.csv(collated_data, "input_data/coronavirus.csv", row.names=F)
rm(list = ls())
