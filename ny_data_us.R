## Covid-2019 interactive mapping tool: script to reformat New York Times US state-level data
## Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine, April 2020

## data extracted from New York Times state-level data obtained from following Github repository
# https://github.com/nytimes/covid-19-data

## state-level population information obtained from US census data
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html#par_textimage_1574439295

# load libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

# load nyt case data
ny_cases <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"))
state_pops <- read.csv("input_data/us_state_pop.csv", encoding = "UTF-8")
colnames(state_pops) <- c("state", "population")
ny_cases <- merge(ny_cases, state_pops, by="state")

# add data for new cases, new deaths, days since 100th case, and days since 10th death
ny_cases$new_deaths = ny_cases$new_cases = NA
ny_cases$days_since_case100 = ny_cases$days_since_death10 = 0

ny_cases = ny_cases[order(ny_cases$state, ny_cases$date),]
state_list = unique(ny_cases$state)

for (i in 1:length(state_list)) {
  ny_subset = subset(ny_cases, state == state_list[i])
  ny_subset = ny_subset[order(ny_subset$date),]
  
  # add starting level for new cases and deaths
  ny_subset$new_cases = ny_subset$cases[1]
  ny_subset$new_deaths = ny_subset$deaths[1]
  
  for (j in 2:nrow(ny_subset)) {
    ny_subset$new_cases[j] = ny_subset$cases[j] - ny_subset$cases[j-1]
    ny_subset$new_deaths[j] = ny_subset$deaths[j] - ny_subset$deaths[j-1]
  }
  
  # set negative new case or death counts to 0
  ny_subset$new_cases[ny_subset$new_cases<0] = 0
  ny_subset$new_deaths[ny_subset$new_deaths<0] = 0

  # data on days since 100th case or 10th death
  ny_subset$days_since_case100[ny_subset$cases>=100] = 0:(sum(ny_subset$cases>=100)-1)
  ny_subset$days_since_death10[ny_subset$deaths>=10] = 0:(sum(ny_subset$deaths>=10)-1)
  
  # fold into main dataset
  ny_cases$new_cases[ny_cases$state==state_list[i]] = ny_subset$new_cases
  ny_cases$new_deaths[ny_cases$state==state_list[i]] = ny_subset$new_deaths
  ny_cases$days_since_case100[ny_cases$state==state_list[i]] = ny_subset$days_since_case100
  ny_cases$days_since_death10[ny_cases$state==state_list[i]] = ny_subset$days_since_death10
  
}

# add normalised counts
ny_cases$per100k =  as.numeric(format(round(ny_cases$cases/(ny_cases$population/100000),1),nsmall=1))
ny_cases$newper100k =  as.numeric(format(round(ny_cases$new_cases/(ny_cases$population/100000),1),nsmall=1))
ny_cases$deathsper100k =  as.numeric(format(round(ny_cases$deaths/(ny_cases$population/100000),1),nsmall=1))
ny_cases$newdeathsper100k =  as.numeric(format(round(ny_cases$new_deaths/(ny_cases$population/100000),1),nsmall=1))

# save output
write.csv(ny_cases, "input_data/coronavirus_states.csv", row.names=F)
rm(list = ls())
