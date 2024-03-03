# Clean work space
rm(list=ls(all=T))
options(stringAsFactors = F)
options("scipen" = 100, "digits" = 4)

setwd("/Users/M276066/Documents/R projects/NationalVitalStatistics/")
nvs <- read.csv("NVSS__-_National_Cardiovascular_Disease_Surveillance_Data_20240301.csv", na.strings = c("", "NA"))
View(nvs)
summary(nvs)
str(nvs)

library(descr)
library(RColorBrewer)
library(plotly)
library(ggmap)
library(maps)
library(mapproj)
library(dplyr)
  # Description of columns
    freq(nvs$YearStart) # Goes from 2000 to 2020
    freq(nvs$Topic) # MI, CHD, Heart Dz, CHF, Major CV Dz, Stroke is 3/8
    freq(nvs$Question) # Same as topic, but has stroke, ischemic stroke, hemorrhagic stroke
    freq(nvs$Data_Value_Type) # age-standardized is 35%, crude is 65%
    freq(nvs$Break_Out) # Age ranges, sex, race
    freq(nvs$Break_Out_Category) # categories of age, gender, overall, race

# Data frames to grab
  ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
  stroke <- subset(nvs, subset = nvs$Topic == "Stroke")

# Starting point
# year 2020, interactable chloropleth map for stroke
stroke2020 <- subset(stroke, subset = stroke$YearStart == 2020 & 
                       stroke$Break_Out == "Overall" & 
                       stroke$Data_Value_Type == "Age-Standardized" &
                       stroke$Question == "Ischemic stroke mortality rate among US adults (18+); NVSS" &
                       stroke$LocationDesc != "Washington, DC" &
                       stroke$LocationDesc != "United States")
View(stroke2020)

# Interactable state map plot with values with plotly
# Here, we will make an interactable plot using data on Stroke mortality from 2020
# https://plotly.com/r/choropleth-maps/
  # Locationmode ='USA-states' and provide locations as two letter abbreviations
  stroke2020$hover <- with(stroke2020, paste(LocationDesc, '<br>', "AGM", Data_Value))
  View(stroke2020)
  # l - give state boundaries a white border
  # g specify some map projection/options
  l <- list(color = toRGB("white"), width = 2)
  g <- list(scope = 'usa', projection = list(type = 'albers usa'),
    showlakes = TRUE, lakecolor = toRGB('white')
  )
  fig <- plot_geo(stroke2020, locationmode = 'USA-states') %>% add_trace(
    z = ~Data_Value, text = ~hover, locations = ~LocationAbbr,
    color = ~Data_Value, colors = 'Purples'
  ) %>% colorbar(title = "Age-adjusted Mortality") %>% layout(
    title = 'Ischemic Stroke Age-adjusted Mortality by State<br>(Hover for breakdown)',
    geo = g
  )
  fig

    # *** Changing colors? ****  
  
# Approach #2 - plotting heatmap in ggplot
# Here, we will use data on AMI average from 2010-2020
# https://r-graphics.org/recipe-miscgraph-choropleth
  amiavg <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                     ami$Break_Out == "Overall" &
                     ami$LocationDesc != "Washington, DC" &
                     ami$LocationDesc != "United States"
                   )
  View(amiavg)
  freq(amiavg$YearStart)
  # Find average for the years for all state
    # New matrix, fill with state names
    avgmatrix <- matrix(0,50,2)
    avgmatrix[,1] <- tolower(amiavg$LocationDesc[1:50])
    # Fill with numbers
      # Make a vector to loop through all 21 years. 
      # Could also filter by subset(amiavg$LocationAbbr == "AL") - would take 50 lines of code to build but would always work
      countvector <- seq(0,1000,50)
      tempnum = 0
      # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
      for (i in 1:50) {
        for (j in 1:21) {
          tempnum <-  tempnum + amiavg[(i + countvector[j]), 15]
        }
        avgmatrix[i,2] <- tempnum/21
        tempnum = 0 # reset tempnum for the next iteration of i
      }
  # Need to merge my data to states, because you map the "states" from library(maps)
      # Library(maps)
      str(states)
      str(avgmatrix)
    states_map <- map_data("state")
    amiavgmap <- merge(states_map, avgmatrix, by.x = "region", by.y = "V1")
      str(amiavgmap)
      View(amiavgmap)
    # After the merge, the data isn't sorted correctly
     # library(dplyr) # For arrange() function
     # Sort by group, then order
    amiavgmap <- arrange(amiavgmap, group, order)
    str(amiavgmap)
    freq(as.numeric(amiavgmap$V2))
  # Final plot
  x11()
  ggplot(amiavgmap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      scale_fill_gradient2(name = "Avg AMI Mortality",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red", 
                           midpoint = median(as.numeric(amiavgmap$V2)),
                           breaks = c(0, 30, 60, 90, 120),
                           limits = c(0, 125)
                           ) +
      theme_bw() + 
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12, hjust = 0,vjust = 0.5))
      
          # working graph template
          states <- map_data("state")
          ggplot(states, aes(long, lat, group = group)) +
            geom_polygon(fill = "white", colour = "black") +
            coord_map()
  
# Approach #3 Urbnmapr
#  https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2


