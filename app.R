## COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), February 2020

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# update data with automated script
source("jhu_data_full.R")

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
sars_cases = read.csv("input_data/sars.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
ebola_cases = read.csv("input_data/ebola.csv")
h1n1_cases = read.csv("input_data/h1n1.csv")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")





### MAP FUNCTIONS ###
# function to plot cumulative COVID cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# test function
#cumulative_plot(cv_aggregated, current_date)
#new_cases_plot(cv_aggregated, current_date)

# function to plot cumulative sars cases by date
sars_cumulative_plot = function(sars_aggregated, sars_date) {
  plot_df = subset(sars_aggregated, date<=as.Date(sars_date, format="%Y-%m-%d"))
  ggplot(plot_df, aes(x = date, y = cases)) + geom_line(colour = sars_col) + geom_point(size = 1, alpha = 0.8, colour = sars_col) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(sars_col)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    scale_y_continuous(limits=c(0,10000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new cases by date
sars_new_cases_plot = function(sars_aggregated, plot_date) {
  plot_df_new = subset(sars_aggregated, date<=plot_date)
  ggplot(plot_df_new, aes(x = date, y = new)) + 
    geom_bar(position="stack", stat="identity", fill = sars_col) + 
    ylab("new cases") + theme_bw() + ylim(0,2000) + 
    scale_fill_manual(values=c(sars_col)) +
    xlim(c(sars_min_date,sars_max_date)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5))
}

# function to plot new cases by region
country_cases_plot = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = region, 
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",new_outcome))) + 
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",new_outcome)))+
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = new_outcome, fill = region, 
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",new_outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g +
    geom_bar(position="stack", stat="identity") + 
    ylab("new") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region
country_cases_cumulative = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death")) {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) + xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_case100,"\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ", days_since_death10,"\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to plot cumulative cases by region on log10 scale
country_cases_cumulative_log = function(cv_cases, start_point=c("Date", "Day of 100th confirmed case", "Day of 10th death"))  {
  if (start_point=="Date") {
    g = ggplot(cv_cases, aes(x = date, y = outcome, colour = region, group = 1,
                             text = paste0(format(date, "%d %B %Y"), "\n", region, ": ",outcome))) +
      xlim(c(cv_min_date,current_date+1)) +
      xlab("Date")
  }
  
  if (start_point=="Day of 100th confirmed case") {
    cv_cases = subset(cv_cases, days_since_case100>0)
    g = ggplot(cv_cases, aes(x = days_since_case100, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_case100, "\n", region, ": ",outcome))) +
      xlab("Days since 100th confirmed case")
  }
  
  if (start_point=="Day of 10th death") {
    cv_cases = subset(cv_cases, days_since_death10>0)
    g = ggplot(cv_cases, aes(x = days_since_death10, y = outcome, colour = region, group = 1,
                             text = paste0("Day ",days_since_death10, "\n", region, ": ",outcome))) +
      xlab("Days since 10th death")
  }
  
  g1 = g + geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative (log10)") + theme_bw() +
    scale_y_continuous(trans="log10") +
    scale_colour_manual(values=country_cols) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

# function to render plotly of epidemic comparison depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
  epi_comp$outcome = epi_comp[,comparison] 
  epi_comp = epi_comp[order(epi_comp$outcome),]
  epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
  
  p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
    ylab("N") + xlab("") + theme_bw() + 
    scale_fill_manual(values=c("2019-COVID"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
    theme(legend.position = "")
  
  if(comparison == "cfr") { p1 = p1 + ylab("%") }
  if(comparison == "deaths") { p1 = p1 + scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e8), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}





### DATA PROCESSING: COVID-19 ###

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
if (any(grepl("/", cv_cases$date))) { 
  cv_cases$date = format(as.Date(cv_cases$date, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { cv_cases$date = as.Date(cv_cases$date, format="%Y-%m-%d") }
cv_cases$date = as.Date(cv_cases$date)
cv_min_date = as.Date(min(cv_cases$date),"%Y-%m-%d")
current_date = as.Date(max(cv_cases$date),"%Y-%m-%d")
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$newper100k = as.numeric(format(round(cv_cases$new_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$activeper100k = as.numeric(format(round(cv_cases$active_cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)

# add variable for days since 100th case and 10th death
cv_cases$days_since_case100 = cv_cases$days_since_death10 = 0
for (i in 1:length(unique(cv_cases$country))) {
  country_name = as.character(unique(cv_cases$country))[i]
  country_db = subset(cv_cases, country==country_name)
  country_db$days_since_case100[country_db$cases>=100] = 1:sum(country_db$cases>=100)
  country_db$days_since_death10[country_db$deaths>=10] = 1:sum(country_db$deaths>=10)
  cv_cases$days_since_case100[cv_cases$country==country_name] = country_db$days_since_case100
  cv_cases$days_since_death10[cv_cases$country==country_name] = country_db$days_since_death10
}

# creat variable for today's data
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_case_count_China = sum(cv_today$cases[cv_today$country=="Mainland China"])
current_case_count_other = sum(cv_today$cases[cv_today$country!="Mainland China"])
current_death_count = sum(cv_today$deaths)

# create subset for countries with at least 100 cases
cv_today_100 = subset(cv_today, cases>=100)

# write current day's data
write.csv(cv_today %>% select(c(country, date, update, cases, new_cases, deaths, new_deaths,
                                recovered, new_recovered, active_cases, 
                                per100k, newper100k, activeper100k,
                                days_since_case100, days_since_death10)), "input_data/coronavirus_today.csv")

# aggregate at continent level
cv_cases_continent = subset(cv_cases, !is.na(continent_level)) %>% select(c(cases, new_cases, deaths, new_deaths, date, continent_level)) %>% group_by(continent_level, date) %>% summarise_each(funs(sum)) %>% data.frame()

# add variable for days since 100th case and 10th death
cv_cases_continent$days_since_case100 = cv_cases_continent$days_since_death10 = 0
cv_cases_continent$continent = cv_cases_continent$continent_level
for (i in 1:length(unique(cv_cases_continent$continent))) {
  continent_name = as.character(unique(cv_cases_continent$continent))[i]
  continent_db = subset(cv_cases_continent, continent==continent_name)
  continent_db$days_since_case100[continent_db$cases>=100] = 1:sum(continent_db$cases>=100)
  continent_db$days_since_death10[continent_db$deaths>=10] = 1:sum(continent_db$deaths>=10)
  cv_cases_continent$days_since_case100[cv_cases_continent$continent==continent_name] = continent_db$days_since_case100
  cv_cases_continent$days_since_death10[cv_cases_continent$continent==continent_name] = continent_db$days_since_death10
}
write.csv(cv_cases_continent, "input_data/coronavirus_continent.csv")

# aggregate at global level
cv_cases_global = cv_cases %>% select(c(cases, new_cases, deaths, new_deaths, date, global_level)) %>% group_by(global_level, date) %>% summarise_each(funs(sum)) %>% data.frame()
cv_cases_global$days_since_case100 = cv_cases_global$days_since_death10 = 1:nrow(cv_cases_global)
write.csv(cv_cases_global, "input_data/coronavirus_global.csv")

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(alpha3 %in% worldcountry$id)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,1,10,50,100,500)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-COVID (active)", "2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID (new)", "2019-COVID (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "<small>Active cases per 100,000</small>") #%>%
#fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
names(cv_aggregated) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = 0 }
  if (i>1) { cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated$date = as.Date(cv_aggregated$date,"%Y-%m-%d")

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
cls_names = c(as.character(unique(cv_cases$country)), as.character(unique(cv_cases_continent$continent)),"Global")
country_cols = cls[1:length(cls_names)]
names(country_cols) = cls_names





### DATA PROCESSING: SARS ###

# extract dates from sars data
sars_cases$date = as.Date(sars_cases$date, format="%d/%m/%Y")
sars_min_date = min(sars_cases$date)
sars_max_date = max(sars_cases$date)
sars_max_date_clean = format(as.POSIXct(sars_max_date),"%d %B %Y")

# merge sars data with country data and extract key summary variables
sars_cases = merge(sars_cases, countries, by = "country")
sars_cases = sars_cases[order(sars_cases$date),]
sars_cases$per100k = as.numeric(format(round(sars_cases$cases/(sars_cases$population/100000),1),nsmall=1))
sars_final = subset(sars_cases, date==sars_max_date) 
sars_final_case_count = sum(sars_final$cases)

# select polygons for sars base map
sars_large_countries = sars_final %>% filter(country %in% country_geoms$countries_present)
sars_large_countries = sars_large_countries[order(sars_large_countries$alpha3),]
sars_plot_map <- worldcountry[worldcountry$id %in% sars_large_countries$alpha3, ]

# create plotting parameters for sars map
sars_pal <- colorBin("Blues", domain = sars_large_countries$per100k, bins = bins)

# creat sars interactive map (needs to include polygons and circles as slider input not recognised upon initial loading)
sars_basemap = leaflet(sars_plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2003-SARS (cumulative)", "2019-COVID", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-COVID", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_large_countries$per100k), group = "2003-SARS (cumulative)",
              label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_large_countries$country, sars_large_countries$cases, sars_large_countries$deaths, sars_large_countries$per100k) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                textsize = "15px", direction = "auto")) %>%
  
  addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
             fillOpacity = 0.2, color = sars_col, group = "2003-SARS (cumulative)",
             label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
               textsize = "15px", direction = "auto")) %>%
  
  addCircleMarkers(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5),
             fillOpacity = 0.2, color = covid_col, group = "2019-COVID",
             label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$recovered, cv_today$per100k) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
               textsize = "15px", direction = "auto"))  %>%
  
  addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4),
             fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
             label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
               textsize = "15px", direction = "auto")) %>%
  
  addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
             fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
             label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
               textsize = "15px", direction = "auto")) 
  
# sum sars case counts by date
sars_aggregated = aggregate(sars_cases$cases, by=list(Category=sars_cases$date), FUN=sum)
names(sars_aggregated) = c("date", "cases")

# add variable for new sars cases in last 7 days
for (i in 1:nrow(sars_aggregated)) { 
  if (i==1) { sars_aggregated$new[i] = NA }
  if (i>1) { 
    sars_aggregated$new[i] = sars_aggregated$cases[i] - sars_aggregated$cases[i-1] 
  }
}
sars_aggregated$new[sars_aggregated$new<0] = 0





### OUTBREAK COMPARISON DATA ###

# load epidemic comparison data
epi_comp = as.data.frame(data.table::fread("input_data/epi_comp.csv"))
epi_comp$outbreak = factor(epi_comp$outbreak, levels = epi_comp$outbreak)
epi_comp$cases[1] = current_case_count
epi_comp$deaths[1] = current_death_count
epi_comp$countries[1] = nrow(subset(cv_today, country!="Diamond Princess Cruise Ship"))
epi_comp$cfr[1] = round(epi_comp$deaths[1]/epi_comp$cases[1]*100,1)
epi_comp$cfr = round(epi_comp$cfr,2)





### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "COVID-19 tracker", id="nav",
             
             tabPanel("COVID-19 mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right"),
                                        span(h4(textOutput("reactive_recovered_count"), align = "right"), style="color:#006d2c"),
                                        span(h4(textOutput("reactive_active_count"), align = "right"), style="color:#cc4c02"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        tags$i(h6("Updated once daily. For more regular updates, refer to: ", tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins COVID-19 dashboard."))),
                                        tags$i(h6("Reported cases are subject to significant variation in testing capacity between countries.")),
                                        plotOutput("epi_curve", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderInput("plot_date",
                                                    label = h5("Select mapping date"),
                                                    min = as.Date(cv_min_date,"%Y-%m-%d"),
                                                    max = as.Date(current_date,"%Y-%m-%d"),
                                                    value = as.Date(current_date),
                                                    timeFormat = "%d %b", 
                                                    animate=animationOptions(interval = 2000, loop = FALSE))
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                          
                          
                      )
             ),
             
             tabPanel("Region plots",
                      
                      sidebarLayout(
                        sidebarPanel(
                          
                          pickerInput("level_select", "Level:",   
                                      choices = c("Global", "Continent", "Country"), 
                                      selected = c("Country"),
                                      multiple = FALSE),
                          
                          pickerInput("region_select", "Country/Region:",   
                                      choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                                      options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                      selected = cv_today_100$country,
                                      multiple = TRUE), 
                          
                          pickerInput("outcome_select", "Outcome:",   
                                      choices = c("Cases", "Deaths"), 
                                      selected = c("Cases"),
                                      multiple = FALSE),
                          
                          pickerInput("start_date", "Plotting start date:",   
                                      choices = c("Date", "Day of 100th confirmed case", "Day of 10th death"), 
                                      options = list(`actions-box` = TRUE),
                                      selected = "Date",
                                      multiple = FALSE), 
                          "Select outcome, regions, and plotting start date from drop-down menues to update plots. Countries with at least 100 confirmed cases are included."
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("New", plotlyOutput("country_plot")),
                            tabPanel("Cumulative", plotlyOutput("country_plot_cumulative")),
                            tabPanel("Cumulative (log10)", plotlyOutput("country_plot_cumulative_log"))
                          )
                        )
                      )
             ),
             
             tabPanel("SARS mapper",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("sars_map", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 80, left = 20, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        h3(textOutput("sars_reactive_case_count"), align = "right"),
                                        h4(textOutput("sars_reactive_death_count"), align = "right"),
                                        h6(textOutput("sars_clean_date_reactive"), align = "right"),
                                        h6(textOutput("sars_reactive_country_count"), align = "right"),
                                        plotOutput("sars_epi_curve", height="130px", width="100%"),
                                        plotOutput("sars_cumulative_plot", height="130px", width="100%"),
                                        span(("The final count appears to decrease as several cases initially classified as SARS were later re-assigned."),align = "left", style = "font-size:80%"),#tags$br(),
                                        span(("Circles show confirmed cases for COVID, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                                        
                                        sliderTextInput("sars_plot_date",
                                                        label = h5("Select mapping date"),
                                                        choices = format(unique(sars_cases$date), "%d %b %y"),
                                                        selected = format(sars_max_date, "%d %b %y"),
                                                        grid = TRUE,
                                                        animate=animationOptions(interval = 2000, loop = FALSE))
                          ),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                                        tags$a(href='https://www.lshtm.ac.uk', tags$img(src='lshtm_dark.png',height='40',width='80'))),
                          
                          absolutePanel(id = "logo", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                                        actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                                     onclick = sprintf("window.open('%s')", 
                                                                       "https://twitter.com/intent/tweet?text=%20@LSHTM_Vaccines%20outbreak%20mapper&url=https://bit.ly/2uBvnds&hashtags=coronavirus")))
                      )
             ),
             
             tabPanel("Outbreak comparisons",
                      
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("comparison_metric", h3("Select comparison:"),
                                       c("Cases" = "cases",
                                         "Deaths" = "deaths",
                                         "Countries/regions affected" = "countries",
                                         "Case fatality rate" = "cfr")),
                          textOutput("epi_notes_1"),
                          textOutput("epi_notes_2"),
                          textOutput("epi_notes_3")
                        ),
                        
                        mainPanel(plotlyOutput("comparison_plot"), width = 6)
                      )
             ),
             
             tabPanel("Data",
                      numericInput("maxrows", "Rows to show", 25),
                      verbatimTextOutput("rawtable"),
                      downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
                      "Adapted from timeline data published by ", tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", 
                                                                         "Johns Hopkins Center for Systems Science and Engineering.")
             ),
             
             tabPanel("About this site",
                      tags$div(
                        tags$h4("Last update"), 
                        h6(paste0(update)),
                        "This site is updated once daily. At this time of rapid escalation of the COVID-19 pandemic, the following resources offer the latest numbers of known cases:",tags$br(),
                        tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO COVID-19 dashboard"),tags$br(),
                        tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University COVID-19 dashboard"),tags$br(),
                        "The aim of this site is to complement the above resources by providing several interactive features not currently available elsewhere, including the timeline function, 
                        the ability to overlay past outbreaks, and an emphasis on normalised counts (per 100,000 individuals).",tags$br(),
                        tags$br(),tags$h4("Background"), 
                        "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                        These were caused by a new type of coronavirus, and the disease is now commonly referred to as COVID-19.
                        The number of COVID-19 cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                        This story has been rapidly evolving ever since, and each day we are faced by worrying headlines regarding the current state of the outbreak.",
                        tags$br(),tags$br(),
                        "In isolation, these headlines can be hard to interpret. 
                        How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                        This site is updated daily based on data published by Johns Hopkins University. 
                        By looking beyond the headlines, we hope it is possible to get a deeper understanding of this unfolding pandemic.",
                        tags$br(),tags$br(),
                        "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                        "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                        tags$br(),tags$br(),tags$h4("Code"),
                        "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                        tags$br(),tags$br(),tags$h4("Sources"),
                        tags$b("2019-COVID cases: "), tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", "Johns Hopkins Center for Systems Science and Engineering github page,")," with additional information from the ",tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO's COVID-19 situation reports."),
                        " In previous versions of this site (up to 17th March 2020), updates were based solely on the WHO's situation reports.",tags$br(),
                        tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
                        tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                        tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                        tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                        tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
                        substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
                        The upper limit of this range is used for illustrative purposes in the Outbreak comarisons tab.",tags$br(),
                        tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                        tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github"),tags$br(),
                        tags$br(),tags$br(),tags$h4("Authors"),
                        "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                        "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                        tags$br(),tags$br(),tags$h4("Contact"),
                        "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                        tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                      )
             )
             
  )          
)





### SHINY SERVER ###

server = function(input, output, session) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
  #  reactive = cv_cases %>% filter(date == "2020-04-07")
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(alpha3 %in% worldcountry$id)
   #large_countries = reactive %>% filter(alpha3 %in% worldcountry$id)
    worldcountry_subset = worldcountry[worldcountry$id %in% large_countries$alpha3, ]
    large_countries = large_countries[match(worldcountry_subset$id, large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(alpha3 %in% worldcountry$id)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large()$alpha3, ]
  })
  
  reactive_polygons_last24h = reactive({
    worldcountry[worldcountry$id %in% reactive_db_large_last24h()$alpha3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$cases), big.mark=","), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$death), big.mark=","), " deaths")
  })
  
  output$reactive_recovered_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$recovered), big.mark=","), " recovered")
  })
  
  output$reactive_active_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$active_cases), big.mark=","), " active cases")
  })
  
  output$reactive_case_count_China <- renderText({
    paste0("Mainland China: ", prettyNum(sum(subset(reactive_db(), country=="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new, big.mark=",")," new)")
  })
  
  output$reactive_case_count_row <- renderText({
    paste0("Other: ", prettyNum(sum(subset(reactive_db(), country!="Mainland China")$cases), big.mark=",")," (",
           prettyNum((cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new, big.mark=",")," new)")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="Diamond Princess Cruise Ship")), " countries/regions affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
    clearMarkers() %>%
    clearShapes() %>%
    addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$activeper100k)) %>% #group = "2019-COVID (cumulative)",
                #  label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed COVID cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$recovered, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                #  labelOptions = labelOptions(
                #               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                #              textsize = "15px", direction = "auto") %>%
      
      addCircleMarkers(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/5), 
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (new)",
                 label = sprintf("<strong>%s (past 24h)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths, reactive_db_last24h()$new_recovered, reactive_db_last24h()$newper100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5), 
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (cumulative)",
                 label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths,reactive_db()$recovered, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) %>%

      addCircleMarkers(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(active_cases)^(1/5), 
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID (active)",
                 label = sprintf("<strong>%s (active)</strong><br/>Confirmed cases: %g<br/>Cases per 100,000: %g<br/><i><small>Excludes individuals known to have<br/>recovered (%g) or died (%g).</small></i>", reactive_db()$country, reactive_db()$active_cases, reactive_db()$activeper100k, reactive_db()$recovered, reactive_db()$deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto"))  %>%
      
      addCircleMarkers(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                 fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
                 label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4), 
                 fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                 label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                 fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                 label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                   textsize = "15px", direction = "auto"))
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(cv_aggregated, input$plot_date)
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(cv_aggregated, input$plot_date)
  })
  
  # sars tab 
  sars_mod_date = reactive({
    format(as.Date(input$sars_plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$sars_clean_date_reactive <- renderText({
    format(as.POSIXct(sars_mod_date()),"%d %B %Y")
  })
  
  sars_reactive_db = reactive({
    sars_cases %>% filter(date == sars_mod_date())
  })
  
  sars_reactive_db_large = reactive({
    large_countries = sars_reactive_db() %>% filter(country!="Singapore" & country!="Diamond Princess Cruise Ship" & country!="Hong Kong" & country!="Macao")
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  sars_reactive_polygons = reactive({
    worldcountry[worldcountry$id %in% sars_reactive_db_large()$alpha3, ]
  })
  
  output$sars_reactive_case_count <- renderText({
    paste0(sum(sars_reactive_db()$cases), " cases")
  })
  
  output$sars_reactive_death_count <- renderText({
    paste0(sum(sars_reactive_db()$death), " deaths")
  })
  
  
  output$sars_reactive_country_count <- renderText({
    paste0(length(unique(sars_reactive_db()$country_group)), " countries/territories affected")
  })
  
  output$sars_map <- renderLeaflet({
    sars_basemap
  })
  
  observeEvent(input$sars_plot_date, {
    leafletProxy("sars_map") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = sars_reactive_polygons(), stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_reactive_db_large()$per100k), group = "2003-SARS (cumulative)",
                  label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db_large()$country, sars_reactive_db_large()$cases, sars_reactive_db_large()$deaths, sars_reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                    textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = sars_reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4), 
                 fillOpacity = 0.2, color = sars_col, group = "2003-SARS (cumulative)",
                 label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db()$country, sars_reactive_db()$cases, sars_reactive_db()$deaths, sars_reactive_db()$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/5),
                 fillOpacity = 0.1, color = covid_col, group = "2019-COVID",
                 label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Recovered: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$recovered, cv_today$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto"))  %>%
      
      addCircleMarkers(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4),
                 fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                 label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircleMarkers(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4),
                 fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
                 label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
                   textsize = "15px", direction = "auto")) 
  })
  
  output$sars_cumulative_plot <- renderPlot({
    sars_cumulative_plot(sars_aggregated, sars_mod_date())
  })
  
  output$sars_epi_curve <- renderPlot({
    sars_new_cases_plot(sars_aggregated, sars_mod_date())
  })
  
  # comparison plot
  output$comparison_plot <- renderPlotly({
    comparison_plot(epi_comp, input$comparison_metric)
  })
  
  # add footnote for cases
  output$epi_notes_1 <- renderText({
    if(input$comparison_metric=="cases") { paste0("Note that the axis is on a log10 scale so moves in 10-fold increments.
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard linear scale.") }
  })
  
  # add footnote for deaths
  output$epi_notes_2 <- renderText({
    if(input$comparison_metric=="deaths") { 
      paste0("For H1N1, the number of laboratory-confirmed deaths reported by the WHO is displayed. Subsequent modelling studies have estimated the actual number to be in the range of 123,000 to 203,000.")
    }
  })
  
  # add note for cfr
  output$epi_notes_3 <- renderText({
    if(input$comparison_metric=="cfr") { 
      paste0("For COVID-19, this displays the proportion of confirmed cases who have subsequently died. When factoring in mild or asymptomatic infections that are not picked up by case surveillance efforts, current estimates place the case fatality rate in the range of 0.3-1%.")
    }
  })
  
  # update region selections
  observeEvent(input$level_select, {
    if (input$level_select=="Global") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = "Global", selected = "Global")
    }
    
    if (input$level_select=="Continent") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = c("Africa", "Asia", "Europe", "North America", "South America"), 
                        selected = c("Africa", "Asia", "Europe", "North America", "South America"))
    }
    
    if (input$level_select=="Country") {
      updatePickerInput(session = session, inputId = "region_select", 
                        choices = as.character(cv_today_100[order(-cv_today_100$cases),]$country), 
                        selected = cv_today_100$country)
    }
  }, ignoreInit = TRUE)
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$level_select=="Global") { 
      db = cv_cases_global
      db$region = db$global_level
    }
    if (input$level_select=="Continent") { 
      db = cv_cases_continent 
      db$region = db$continent
    }
    if (input$level_select=="Country") { 
      db = cv_cases
      db$region = db$country
    }
    
    if (input$outcome_select=="Cases") { 
      db$outcome = db$cases
      db$new_outcome = db$new_cases
    }
    
    if (input$outcome_select=="Deaths") { 
      db$outcome = db$deaths 
      db$new_outcome = db$new_deaths 
    }
    
    db %>% filter(region %in% input$region_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db(), start_point=input$start_date)
  })
  
  # country-specific plots
  output$country_plot_cumulative_log <- renderPlotly({
    country_cases_cumulative_log(country_reactive_db(), start_point=input$start_date)
  })
  
  # output to download data
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", cv_today$date[1], ".csv", sep="")
    },
    content = function(file) {
      write.csv(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                       recovered, new_recovered, active_cases, 
                                       per100k, newper100k, activeper100k)), file)
    }
  )
  
  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(cv_cases %>% select(c(country, date, cases, new_cases, deaths, new_deaths,
                                     recovered, new_recovered, active_cases, 
                                     per100k, newper100k, activeper100k)), input$maxrows), row.names = FALSE)
    options(orig)
  })
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
