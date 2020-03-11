## Covid-2019 interactive mapping tool
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

# set mapping colour for each outbreak
covid_col = "#cc4c02"
covid_other_col = "#662506"
sars_col = "#045a8d"
h1n1_col = "#4d004b"
ebola_col = "#016c59"

# update data with automated script
source("update_who_data.R")

# import data
cv_cases = read.csv("input_data/coronavirus.csv")
sars_cases = read.csv("input_data/sars.csv")
countries = read.csv("input_data/countries_codes_and_coordinates.csv")
ebola_cases = read.csv("input_data/ebola.csv")
h1n1_cases = read.csv("input_data/h1n1.csv")
worldcountry = geojson_read("input_data/countries.geo.json", what = "sp")
country_geoms = read.csv("input_data/country_geoms.csv")

# extract time stamp from cv_cases
update = tail(cv_cases$last_update,1) 

# check consistency of country names across datasets
if (all(unique(cv_cases$country) %in% unique(countries$country))==FALSE) { print("Error: inconsistent country names")}

# extract dates from cv data
cv_cases$date = as.Date(cv_cases$date, format="%d/%m/%Y")
cv_min_date = min(cv_cases$date)
current_date = max(cv_cases$date)
cv_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# merge cv data with country data and extract key summary variables
cv_cases = merge(cv_cases, countries, by = "country")
cv_cases = cv_cases[order(cv_cases$date),]
cv_cases$per100k = as.numeric(format(round(cv_cases$cases/(cv_cases$population/100000),1),nsmall=1))
cv_cases$million_pop = as.numeric(cv_cases$population>1e6)
cv_today = subset(cv_cases, date==current_date) 
current_case_count = sum(cv_today$cases)
current_death_count = sum(cv_today$deaths)
write.csv(cv_today, "input_data/coronavirus_today.csv")

### plot of cases per 100k
# cv_sub = read.csv("input_data/coronavirus_per100k.csv")
# cv_sub = cv_sub[order(cv_sub$cases),]
# cv_sub$country = factor(cv_sub$country, levels = cv_sub$country)
# g1 = ggplot(cv_sub, aes(x = country, y = per100k)) + geom_bar(position="identity", stat="identity", alpha=0.8, fill=covid_col) + 
#   geom_text(aes(label = per100k), hjust=-0.3) + ylim(0,25) +
#   #geom_bar(aes(x = subset(cv_sub, date=="01/03/2020")$country, y = subset(cv_sub, date=="01/03/2020")$per100k), position="identity", stat="identity", alpha=0.7, fill=covid_other_col) +
#   coord_flip() + theme_bw() + theme(legend.title = element_blank(), legend.position = "", text = element_text(size=10)) + ylab("Cases per 100,000 individuals") + xlab("") + facet_grid(.~date)
# g2 = ggplot(cv_sub, aes(x = country, y = cases)) + geom_bar(position="identity", stat="identity", alpha=0.8, fill=covid_col) + 
#   geom_text(aes(label = round(cases,1)), hjust=-0.3) + ylim(0,100000) +
#   #geom_bar(aes(x = subset(cv_sub, date=="01/03/2020")$country, y = subset(cv_sub, date=="01/03/2020")$per100k), position="identity", stat="identity", alpha=0.7, fill=covid_other_col) +
#   coord_flip() + theme_bw() + theme(legend.title = element_blank(), legend.position = "", text = element_text(size=10)) + ylab("Confirmed cases") + xlab("") + facet_grid(.~date)
# gridExtra::grid.arrange(g2,g1,ncol=2)

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

# select large countries for mapping polygons
cv_large_countries = cv_today %>% filter(country %in% country_geoms$countries_present)
if (all(cv_large_countries$alpha3 %in% worldcountry$id)==FALSE) { print("Error: inconsistent country names")}
cv_large_countries = cv_large_countries[order(cv_large_countries$alpha3),]

# create plotting parameters for map
bins = c(0,0.5,1,5,10,20)
cv_pal <- colorBin("Oranges", domain = cv_large_countries$per100k, bins = bins)
plot_map <- worldcountry[worldcountry$id %in% cv_large_countries$alpha3, ]

# creat cv base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("2019-Covid (new)", "2019-Covid (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-Covid (cumulative)", "2003-SARS", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  addLegend("bottomright", pal = cv_pal, values = ~cv_large_countries$per100k,
            title = "Cases per 100,000")
  # fitBounds(0,-25,90,65) # alternative coordinates for closer zoom

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
    overlayGroups = c("2003-SARS (cumulative)", "2019-Covid", "2009-H1N1 (swine flu)", "2014-Ebola"),
    options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup(c("2019-Covid", "2009-H1N1 (swine flu)", "2014-Ebola"))  %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~-100,-50,~80,80) %>%
  
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_large_countries$per100k), group = "2003-SARS (cumulative)",
              label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_large_countries$country, sars_large_countries$cases, sars_large_countries$deaths, sars_large_countries$per100k) %>% lapply(htmltools::HTML),
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty, 
             fillOpacity = 0.4, color = sars_col, group = "2003-SARS (cumulative)",
             label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
               textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty,
             fillOpacity = 0.2, color = covid_col, group = "2019-Covid",
             label = sprintf("<strong>%s</strong><br/>Covid cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$per100k) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
               textsize = "15px", direction = "auto"))  %>%
  
  addCircles(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4)*3.5e4*penalty,
             fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
             label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
               textsize = "15px", direction = "auto")) %>%
  
  addCircles(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty,
             fillOpacity = 0.2, color = ebola_col, group = "2014-Ebola",
             label = sprintf("<strong>%s</strong><br/>Ebola cases: %g<br/>Deaths: %d", ebola_cases$country, ebola_cases$cases, ebola_cases$deaths) %>% lapply(htmltools::HTML),
             labelOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px", "color" = ebola_col),
               textsize = "15px", direction = "auto")) 
  
# sum cv case counts by date
cv_aggregated = aggregate(cv_cases$cases, by=list(Category=cv_cases$date), FUN=sum)
cv_aggregated_China = aggregate(subset(cv_cases, country=="Mainland China")$cases, by=list(Category=subset(cv_cases, country=="Mainland China")$date), FUN=sum)
cv_aggregated_other = aggregate(subset(cv_cases, country!="Mainland China")$cases, by=list(Category=subset(cv_cases, country!="Mainland China")$date), FUN=sum)
names(cv_aggregated) = names(cv_aggregated_China) = names(cv_aggregated_other) = c("date", "cases")

# add variable for new cases in last 24 hours
for (i in 1:nrow(cv_aggregated)) { 
  if (i==1) { cv_aggregated$new[i] = cv_aggregated_China$new[i] = cv_aggregated_other$new[i] = NA }
  if (i>1) { 
    cv_aggregated$new[i] = cv_aggregated$cases[i] - cv_aggregated$cases[i-1] 
    cv_aggregated_China$new[i] = cv_aggregated_China$cases[i] - cv_aggregated_China$cases[i-1] 
    cv_aggregated_other$new[i] = cv_aggregated_other$cases[i] - cv_aggregated_other$cases[i-1] 
  }
}

# add plotting region
cv_aggregated$region = "Global"
cv_aggregated_China$region = "Mainland China"
cv_aggregated_other$region = "Other"
cv_aggregated = rbind(cv_aggregated, cv_aggregated_China, cv_aggregated_other)
cv_aggregated$region = factor(cv_aggregated$region, levels=c("Global", "Mainland China", "Other"))

# function to plot cumulative cases by date
cumulative_plot = function(cv_aggregated, plot_date) {
  plot_df = subset(cv_aggregated, date<=plot_date & region!="Global")
  plot_df_new = subset(cv_aggregated, date<=plot_date & region=="Global")
  g1 = ggplot(plot_df, aes(x = date, y = cases, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
   ylab("cumulative cases") + theme_bw() + 
   scale_colour_manual(values=c(covid_col, covid_other_col)) +
   xlim(c(cv_min_date,current_date)) + 
   scale_y_continuous(limits=c(0,current_case_count+1000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
   theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
         plot.margin = margin(5, 12, 5, 5))
  # if(plot_date >= as.Date("2020-02-14")) {
  # g1 + annotate("text", x = as.Date("2020-02-03"), y = 81500, label = "new diagnostic criteria") + 
  # annotate("point", x = as.Date("2020-02-14"), y = 80000, colour = "black", size = 3, shape = 4) 
  # } else { g1 }
  g1
}

# function to plot new cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date & region!="Global")
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col, covid_other_col)) +
    xlim(c(cv_min_date,current_date)) + 
    scale_y_continuous(limits=c(0,23000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  if (plot_date >= as.Date("2020-02-14")) {
    g1 + annotate("text", x = as.Date("2020-02-01"), y = 20000, label = "new diagn. criteria", size=3) + 
      annotate("point", x = as.Date("2020-02-14"), y = 20000, colour = "black", size = 3, shape = 4) 
  } else { g1 }
}

# test function
# cumulative_plot(cv_aggregated, current_date)
# new_cases_plot(cv_aggregated, current_date)

# assign colours to countries to ensure consistency between plots 
cls = rep(c(brewer.pal(8,"Dark2"), brewer.pal(10, "Paired"), brewer.pal(12, "Set3"), brewer.pal(8,"Set2"), brewer.pal(9, "Set1"), brewer.pal(8, "Accent"),  brewer.pal(9, "Pastel1"),  brewer.pal(8, "Pastel2")),3)
country_cols = cls[1:length(unique(cv_cases$country))]
names(country_cols) = unique(cv_cases$country)

country_cases_plot = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = new_outcome, fill = country, 
                               text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",new_outcome))) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=country_cols) +
    xlim(c(cv_min_date,current_date+1)) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

country_cases_cumulative = function(cv_cases) {
  g1 = ggplot(cv_cases, aes(x = date, y = outcome, colour = country, group = 1,
                            text = paste0(format(date, "%d %B %Y"), "\n", country, ": ",outcome))) + 
    geom_line(alpha=0.8) + geom_point(size = 1, alpha = 0.8) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=country_cols) +
    xlim(c(cv_min_date,current_date+1)) + 
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10))
  ggplotly(g1, tooltip = c("text")) %>% layout(legend = list(font = list(size=11)))
}

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

# function to plot cumulative sars cases by date
sars_cumulative_plot = function(sars_aggregated, sars_date) {
  plot_df = subset(sars_aggregated, date<=as.Date(sars_date, format="%Y-%m-%d"))
  ggplot(plot_df, aes(x = date, y = cases)) + geom_line(colour = sars_col) + geom_point(size = 1, alpha = 0.8, colour = sars_col) +
    ylab("cumulative cases") + theme_bw() + 
    scale_colour_manual(values=c(sars_col)) + scale_x_date(date_labels = "%b", limits=c(sars_min_date,sars_max_date)) +
    scale_y_continuous(limits=c(0,10000), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 5, 5, 5)) #+
   # geom_hline(yintercept=current_case_count, color=covid_col, linetype="dotted", size=1)
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

# load epidemic comparison data
epi_comp = as.data.frame(data.table::fread("input_data/epi_comp.csv"))
epi_comp$outbreak = factor(epi_comp$outbreak, levels = epi_comp$outbreak)
epi_comp$cases[1] = current_case_count
epi_comp$deaths[1] = current_death_count
epi_comp$countries[1] = nrow(subset(cv_today, country!="International cruise ship (Japan)" & 
                                      country!="Hong Kong" & country!="Taiwan" & country!="Macao"))
epi_comp$cfr[1] = round(epi_comp$deaths[1]/epi_comp$cases[1]*100,1)
epi_comp$cfr = round(epi_comp$cfr,2)

# function to render plotly of depending on selected outcome
comparison_plot = function(epi_comp, comparison) {
  epi_comp$outcome = epi_comp[,comparison] 
  epi_comp = epi_comp[order(epi_comp$outcome),]
  epi_comp$outbreak = factor(epi_comp$outbreak, levels=epi_comp$outbreak)
  
  p1 <- ggplot(epi_comp, aes(x = outbreak, y = outcome, fill=outbreak, text = paste0(outbreak, ": ",outcome))) + geom_bar(alpha = 0.8, stat="identity") +
    ylab("N") + xlab("") + theme_bw() + 
    scale_fill_manual(values=c("2019-Covid"=covid_col, "2003-SARS"=sars_col, "2014-Ebola"=ebola_col,"2009-H1N1 (swine flu)"=h1n1_col)) +
    theme(legend.position = "")
  
  if(comparison == "cfr") { p1 = p1 + ylab("%") }
  if(comparison == "deaths") { p1 = p1 + scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  if(comparison == "cases") { p1 = p1 + scale_y_continuous(trans='log10', limits = c(1,1e8), breaks=c(1,1000,1e6,1e9), labels = function(l) {trans = l / 1000; paste0(trans, "K")}) }
  ggplotly(p1 + coord_flip(), tooltip = c("text")) %>% layout(showlegend = FALSE)
}

# create Shiny ui
ui <- navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
                 "Covid 2019 tracker", id="nav",
                 
                 tabPanel("Covid mapper",
                          div(class="outer",
                              tags$head(includeCSS("styles.css")),
                              leafletOutput("mymap", width="100%", height="100%"),
                              
                              absolutePanel(id = "controls", class = "panel panel-default",
                                            top = 80, left = 20, width = 250, fixed=TRUE,
                                            draggable = TRUE, height = "auto",
                                            
                                            h3(textOutput("reactive_case_count"), align = "right"),
                                            h4(textOutput("reactive_death_count"), align = "right"),
                                            h6(textOutput("clean_date_reactive"), align = "right"),
                                            h6(textOutput("reactive_country_count"), align = "right"),
                                            plotOutput("epi_curve", height="130px", width="100%"),
                                            plotOutput("cumulative_plot", height="130px", width="100%"),
                                            span(h6(textOutput("reactive_case_count_China"), align = "right"), style="color:#cc4c02"),
                                            span(h6(textOutput("reactive_case_count_row"), align = "right"), style="color:#662506"),
                                            span(("Circles show confirmed cases for Covid, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),

                                            sliderInput("plot_date",
                                                        label = h5("Select mapping date"),
                                                        min = cv_min_date,
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
                 
                 tabPanel("Country plots",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              pickerInput("outcome_select", "Outcome:",   
                                          choices = c("Cases", "Deaths"), 
                                          #options = list(`actions-box` = TRUE),
                                          selected = c("Cases"),
                                          multiple = FALSE), 
                              
                              pickerInput("country_select", "Country:",   
                                          choices = as.character(cv_today[order(-cv_today$cases),]$country), 
                                          options = list(`actions-box` = TRUE),
                                          selected = cv_today$country,
                                          multiple = TRUE), 
                              "Select outcome and countries from drop-down menues to update plots."
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("New", plotlyOutput("country_plot")),
                                tabPanel("Cumulative", plotlyOutput("country_plot_cumulative"))
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
                                            span(("Circles show confirmed cases for Covid, SARS, and Ebola, and estimated deaths for H1N1."),align = "left", style = "font-size:80%"),
                                           
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
                 
                 tabPanel("Summary",
                          
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("comparison_metric", h3("Select comparison:"),
                                           c("Cases" = "cases",
                                             "Deaths" = "deaths",
                                             "Countries/territories affected" = "countries",
                                             "Case fatality rate" = "cfr")),
                              textOutput("epi_notes_1"),
                              textOutput("epi_notes_2"),
                              textOutput("epi_notes_3")
                            ),
                            
                            mainPanel(plotlyOutput("comparison_plot"), width = 6)
                          )
                 ),
                 
                 tabPanel("About this site",
                          tags$div(
                            tags$h4("Last updated"), 
                            paste0(update),
                            tags$h4("Background"), 
                            "In December 2019, cases of severe respiratory illness began to be reported across the city of Wuhan in China. 
                            These were caused by a new type of coronavirus, now commonly referred to as Covid-2019.
                            The number of Covid cases started to escalate more quickly in mid-January and the virus soon spread beyond China's borders. 
                            This story has been rapidly evolving ever since, and each day we are faced by new, worrying headlines regarding the current state of the outbreak.",
                            tags$br(),tags$br(),
                            "In isolation, these headlines can be hard to interpret. 
                            How fast is the virus spreading? Are efforts to control the disease working? How does the situation compare with previous epidemics?
                            This site is updated daily based on the number of confirmed cases reported by the WHO. 
                            By looking beyond the daily headlines, we hope it is possible to get a deeper understanding of this unfolding epidemic.",
                            tags$br(),tags$br(),
                            "An article discussing this site was published in ",tags$a(href="https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422", "The Conversation. "),
                            "The map was also featured on the BBC World Service program",tags$a(href="https://www.bbc.co.uk/programmes/w3csym33", "Science in Action."),
                            tags$h4("Code"),
                            "Code and input data used to generate this Shiny mapping tool are available on ",tags$a(href="https://github.com/eparker12/nCoV_tracker", "Github."),
                            tags$h4("Sources"),
                            tags$b("2019-Covid cases: "), tags$a(href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019/situation-reports", "WHO daily situation reports")," with additional information from the ",tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO Covid-2019 dashboard."),
                            " Data are accessed from a ",tags$a(href="https://github.com/eebrown/data2019nCoV", "Github repository")," that is updated daily.",tags$br(),
                            tags$b("2003-SARS cases: "), tags$a(href="https://www.who.int/csr/sars/country/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 confirmed deaths: "), tags$a(href="https://www.who.int/csr/disease/swineflu/updates/en/", "WHO situation reports"),tags$br(),
                            tags$b("2009-H1N1 projected deaths: "), "Model estimates from ", tags$a(href="https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1001558", "GLaMOR Project"),tags$br(),
                            tags$b("2009-H1N1 cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("2009-H1N1 case fatality rate: "), "a systematic review by ", tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/24045719", "Wong et al (2009)"), "identified 
                            substantial variation in case fatality rate estimates for the H1N1 pandemic. However, most were in the range of 10 to 100 per 100,000 symptomatic cases (0.01 to 0.1%).
                            The upper limit of this range is used for illustrative purposes in the Summary tab.",tags$br(),
                            tags$b("2014-Ebola cases: "), tags$a(href="https://www.cdc.gov/flu/pandemic-resources/2009-h1n1-pandemic.html", "CDC"),tags$br(),
                            tags$b("Country mapping coordinates: "), tags$a(href="https://gist.github.com/tadast/8827699", "Github"),tags$br(),
                            tags$h4("Other resources"),
                            tags$a(href="https://experience.arcgis.com/experience/685d0ace521648f8a5beeeee1b9125cd", "WHO Covid-2019 dashboard"),tags$br(),
                            tags$a(href="https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6", "Johns Hopkins University live Covid-2019 tracker"),tags$br(),
                            tags$h4("Authors"),
                            "Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine",tags$br(),
                            "Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine",tags$br(),
                            tags$h4("Contact"),
                            "edward.parker@lshtm.ac.uk",tags$br(),tags$br(),
                            tags$img(src = "vac_dark.png", width = "150px", height = "75px"), tags$img(src = "lshtm_dark.png", width = "150px", height = "75px")
                          )
                 )
)

server = function(input, output) {
  
  # covid tab 
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(input$plot_date),"%d %B %Y")
  })
  
  reactive_db = reactive({
    cv_cases %>% filter(date == input$plot_date)
  })
  
  reactive_db_last24h = reactive({
    cv_cases %>% filter(date == input$plot_date & new_cases>0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(country %in% country_geoms$countries_present)
    large_countries = large_countries[order(large_countries$alpha3),]
    large_countries
  })
  
  reactive_db_large_last24h = reactive({
    large_countries = reactive_db_last24h() %>% filter(country %in% country_geoms$countries_present)
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
    paste0(sum(reactive_db()$cases), " cases")
  })
  
  output$reactive_death_count <- renderText({
    paste0(sum(reactive_db()$death), " deaths")
  })
  
  output$reactive_case_count_China <- renderText({
    paste0("Mainland China: ", sum(subset(reactive_db(), country=="Mainland China")$cases)," (",
           (cv_aggregated %>% filter(date == input$plot_date & region=="Mainland China"))$new," new)")
  })
  
  output$reactive_case_count_row <- renderText({
    paste0("Other: ", sum(subset(reactive_db(), country!="Mainland China")$cases)," (",
           (cv_aggregated %>% filter(date == input$plot_date & region=="Other"))$new," new)")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), country!="International cruise ship (Japan)" & 
                         country!="Hong Kong" & country!="Taiwan" & country!="Macao")), " countries/territories affected")
  })
  
  output$reactive_new_cases_24h <- renderText({
    paste0((cv_aggregated %>% filter(date == input$plot_date & region=="Global"))$new, " new in last 24h")
  })
  
  output$mymap <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(input$plot_date, {
    leafletProxy("mymap") %>% 
    clearShapes() %>%
    addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive_db_large()$per100k), #group = "2019-Covid (cumulative)",
                  label = sprintf("<strong>%s</strong><br/>Covid cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", reactive_db_large()$country, reactive_db_large()$cases, reactive_db_large()$deaths, reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                               style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                               textsize = "15px", direction = "auto")) %>%
      #addLegend(pal = cv_pal(reactive_db_large()$per100k), values = ~reactive_db_large()$per100k, position = "topright") %>%
      
      addCircles(data = reactive_db_last24h(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(new_cases)^(1/4)*3.5e4*penalty, 
                 fillOpacity = 0.4, color = covid_col, group = "2019-Covid (new)",
                 label = sprintf("<strong>%s</strong><br/>Covid cases: %g<br/>Deaths: %d", reactive_db_last24h()$country, reactive_db_last24h()$new_cases, reactive_db_last24h()$new_deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty, 
                 fillOpacity = 0.4, color = covid_col, group = "2019-Covid (cumulative)",
                 label = sprintf("<strong>%s</strong><br/>Covid cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", reactive_db()$country, reactive_db()$cases, reactive_db()$deaths, reactive_db()$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto")) %>%

      addCircles(data = sars_final, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty, 
                 fillOpacity = 0.2, color = sars_col, group = "2003-SARS",
                 label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_final$country, sars_final$cases, sars_final$deaths, sars_final$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4)*3.5e4*penalty, 
                 fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                 label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty, 
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
    large_countries = sars_reactive_db() %>% filter(country!="Singapore" & country!="International cruise ship (Japan)" & country!="Hong Kong" & country!="Macao")
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
      clearShapes() %>%
      addPolygons(data = sars_reactive_polygons(), stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.1, fillColor = ~sars_pal(sars_reactive_db_large()$per100k), group = "2003-SARS (cumulative)",
                  label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db_large()$country, sars_reactive_db_large()$cases, sars_reactive_db_large()$deaths, sars_reactive_db_large()$per100k) %>% lapply(htmltools::HTML),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                    textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = sars_reactive_db(), lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty, 
                 fillOpacity = 0.4, color = sars_col, group = "2003-SARS (cumulative)",
                 label = sprintf("<strong>%s</strong><br/>SARS cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", sars_reactive_db()$country, sars_reactive_db()$cases, sars_reactive_db()$deaths, sars_reactive_db()$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = sars_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = cv_today, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty,
                 fillOpacity = 0.2, color = covid_col, group = "2019-Covid",
                 label = sprintf("<strong>%s</strong><br/>Covid cases: %g<br/>Deaths: %d<br/>Cases per 100,000: %g", cv_today$country, cv_today$cases, cv_today$deaths, cv_today$per100k) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = covid_col),
                   textsize = "15px", direction = "auto"))  %>%
      
      addCircles(data = h1n1_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(projected_deaths)^(1/4)*3.5e4*penalty,
                 fillOpacity = 0.2, color = h1n1_col, group = "2009-H1N1 (swine flu)",
                 label = sprintf("<strong>%s</strong><br/>H1N1 deaths (confirmed): %g<br/>H1N1 deaths (estimated): %g", h1n1_cases$region, h1n1_cases$deaths, h1n1_cases$projected_deaths) %>% lapply(htmltools::HTML),
                 labelOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px", "color" = h1n1_col),
                   textsize = "15px", direction = "auto")) %>%
      
      addCircles(data = ebola_cases, lat = ~ latitude, lng = ~ longitude, weight = 1, radius = ~(cases)^(1/4)*3.5e4*penalty,
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
                                                  The 60.8 million estimated cases of H1N1 dwarf all other outbreaks of plotted on a standard scale.") }
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
      paste0("The case fatality rate for Covid-2019 may be overestimated if numerous mild or asymptomatic infections are not being picked up by case surveillance efforts.")
    }
  })
  
  # create dataframe with selected countries
  country_reactive_db = reactive({
    if (input$outcome_select=="Cases") { 
      cv_cases$outcome = cv_cases$cases
      cv_cases$new_outcome = cv_cases$new_cases
      }
    if (input$outcome_select=="Deaths") { 
      cv_cases$outcome = cv_cases$deaths 
      cv_cases$new_outcome = cv_cases$new_deaths 
    }
    cv_cases %>% filter(country %in% input$country_select)
  })
  
  # country-specific plots
  output$country_plot <- renderPlotly({
    country_cases_plot(country_reactive_db())
  })
  
  # country-specific plots
  output$country_plot_cumulative <- renderPlotly({
    country_cases_cumulative(country_reactive_db())
  })
  
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
