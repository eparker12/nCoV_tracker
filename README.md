## COVID-19 interactive mapping tool

This github page contains the code and input data for the [interactive COVID-19 mapping tool](https://vac-lshtm.shinyapps.io/ncov_tracker/) developed by the Vaccine Centre at the London School of Hygiene and Tropical Medicine.

Input data are obtained from the [Johns Hopkins Center for Systems Science and Engineering github page](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series).

The Shiny app, first launched on [11th February 2020](https://theconversation.com/coronavirus-outbreak-a-new-mapping-tool-that-lets-you-scroll-through-timeline-131422), aims to complement existing COVID-19 mapping dashboards (such as those developed by the [WHO](https://covid19.who.int) and [Johns Hopkins University](https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6)) with several interactive features, including the timeline function and the ability to overlay past outbreaks. 

## Shiny interface

Follow [this](https://vac-lshtm.shinyapps.io/ncov_tracker/) link for the interactive Shiny app. A screenshot of the interface is provided below.

![Shiny app interface](www/app_image.png)

## Analysis code

Key elements of the analysis code are as follows:
- *jhu_data_full.R* – an R script that extracts and reformats time-series from the [Johns Hopkins Center for Systems Science and Engineering github page](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series). The output files are saved in the *input_data* folder.
- *ny_data_us.R* – an R script that extracts and reformats time-series from the [New York Times github page](https://github.com/nytimes/covid-19-data). The output files are saved in the *input_data* folder.
- *app.R* - an R script used to render the Shiny app. This consists of several plotting functions as well as the ui (user interface) and server code required to render the Shiny app. The script has become more complex over time as a growing number of interactive features has been added.
- *input_data* - a folder containing dynamic input data relating to the evolving COVID-19 pandemic (updated by *jhu_data_full.R* and  *ny_data_us.R*) and static input data relating to past epidemics and country mapping coordinates.

## Updates

From 1st May 2020 onwards, this github page will no longer be updated daily. The [Shiny app](https://vac-lshtm.shinyapps.io/ncov_tracker/) automatically updates itself based on the code in *jhu_data_full.R* and *ny_data_us.R* and updated case data can be downloaded directly from the app via the 'Data' tab. To create up-to-date versions of the input data, users can clone this repository and run *jhu_data_full.R* and *ny_data_us.R* locally. Updated input data will also be uploaded to this github page at regular intervals.

From 3rd September onwards, the numbers of recovered cases are no longer plotted owing to large discrepancies in reporting rates among countries.

## Other resources

Several resources proved invaluable when building this app, including:
- A [tutorial by Florianne Verkroost](https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/) on building interactive maps;
- The [SuperZIP app](https://shiny.rstudio.com/gallery/superzip-example.html) and [associated code](https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example);
- The [RStudio Leaflet tutorials](https://rstudio.github.io/leaflet/).

## Authors
Dr Edward Parker, The Vaccine Centre, London School of Hygiene & Tropical Medicine

Quentin Leclerc, Department of Infectious Disease Epidemiology, London School of Hygiene & Tropical Medicine

## Contact
edward.parker@lshtm.ac.uk
