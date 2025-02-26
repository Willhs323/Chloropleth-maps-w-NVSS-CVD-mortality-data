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
library(ggpubr)
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

#####
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

          
# AMI data as pct increase in last 5 years via graph option #2
amipctchange <- ami
amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                   ami$Break_Out == "Overall" &
                   ami$LocationDesc != "Washington, DC" &
                   ami$LocationDesc != "United States"
)   
View(amipctchange)
str(amipctchange)

pctchangematrix <- matrix(0,50,2)
pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
View(pctchangematrix)

# Make a vector to loop through all 21 years. 
# Loop from years 2011 (n + 500) through 2020 (n + 950)
countvector <- seq(0,1000,50)

tempnum = 0
tempnuminitial = 0
tempnumfinal = 0
# Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
for (i in 1:50) {
  tempnuminitial <- amipctchange[(i + 550),15] # add 500 to start at year 2011
  tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
  pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
  
  tempnum = 0 # reset tempnum for the next iteration of i
  tempnuminitial = 0
  tempnumfinal = 0
}

# Need to merge my data to states, because you map the "states" from library(maps)
# Library(maps)
str(states)
str(avgmatrix)
states_map <- map_data("state")
amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")

# After the merge, the data isn't sorted correctly
# library(dplyr) # For arrange() function
# Sort by group, then order
amichangemap <- arrange(amichangemap, group, order)
str(amichangemap)

# Final plot
x11()
ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
  geom_polygon(colour = "black") +
  coord_map() +
  scale_fill_gradient2(name = "AMI Mortality % change",
                       low = "darkgreen", 
                       mid = "white", 
                       high = "red", 
                       midpoint = 0,
                       breaks = c(-50, -25, 0, 25, 50),
                       limits = c(-50, 50)
  ) +
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.text = element_text(size = 12, hjust = 0,vjust = 0.5))
          
          
# Approach #3 Urbnmapr
#  https://urban-institute.medium.com/how-to-create-state-and-county-maps-easily-in-r-577d29300bb2


#####
# Looking at AMI deaths from 2016-2020, averaging
# Find overall trends and see if explained by race, sex, age

finalrates <- matrix(0,51,37)
finalrates[1,1] <- c("Age calculation")
finalrates[2:51,1] <- amipctchange[,1] <- tolower(amipctchange$LocationDesc[1:50])
colnames(finalrates) <- c("Header", "Overall", "F", "M", "25-44", "45-64", "65+", "White",
                          "Black", "Hispanice", "Other", "25-44 F", "25-44 M", "45-64 F", "45-64 M",
                          "65+F", "65+M", "F, White", "M, White", "F, Black", "M, Black", "F, Hispanic", "Male, Hispanic",
                          "F, Other", "M, Other", "White, 25-44", "Black, 25-44", "Hispanic, 25-44", "Other, 25-44",
                          "White, 45-64", "Black, 45-64", "Hispanic, 45-64", "Other, 45-64",
                          "White, 65+", "Black, 65+", "Hispanic, 65+", "Other, 65+")
finalrates

# Start with a baseline that is age standardized
ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                         ami$Break_Out == "Overall" &
                         ami$LocationDesc != "Washington, DC" &
                         ami$LocationDesc != "United States"
)
  # Step one - looking at overall change from 5 years
  pctchangematrix <- matrix(0,50,2)
  pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
  countvector <- seq(0,1000,50)
  tempnum = 0
  tempnuminitial = 0
  tempnumfinal = 0
  # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
  for (i in 1:50) {
    tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
    tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
    pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
    tempnum = 0 # reset tempnum for the next iteration of i
    tempnuminitial = 0
    tempnumfinal = 0
  }

  states_map <- map_data("state")
  amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
  amichangemap <- arrange(amichangemap, group, order)
  finalrates[1,2]  <- "Age Stdz"
  finalrates[2:51,2] <- pctchangematrix[,2]
  # Final plot
  x11()
  p1 <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
    geom_polygon(colour = "black") +
    coord_map() +
    labs(title = "AMI mortality % change 2016-2020, Age standardized") +
    scale_fill_gradient2(name = "% change",
                         low = "darkgreen", 
                         mid = "white", 
                         high = "red", 
                         midpoint = 0,
                         breaks = c(-50, -25, 0, 25, 50),
                         limits = c(-50, 50)
    ) +
    theme_bw() + 
    theme(title = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.text = element_text(size = 12, hjust = 0,vjust = 0.5))
  p1
    # Steptwo - look at that range for females
  
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- ami
    amipctchange <- subset(amipctchange, subset = ami$Data_Value_Type == "Age-Standardized" &
                             ami$Break_Out == "Female" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,3]  <- "Age Stdz"
    finalrates[2:51,3] <- pctchangematrix[,2]
    x11()
    p2  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Women") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red", 
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p2
    
    # stepthree - look at that range for males
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                             ami$Break_Out == "Male" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,4]  <- "Age Stdz"
    finalrates[2:51,4] <- pctchangematrix[,2]
    x11()
    p3  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Men") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red", 
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p3
    
    # ggarrange - look at males vs females comparison last 5 years
    plot <- ggarrange(p2, p3, ncol = 1,common.legend = TRUE, legend = "right", labels = "auto",
                      legend.grob = )
    x11()
    annotate_figure(plot, 
                    top = text_grob("Percent change in age-adjusted AMI Mortality from 2016-2020", 
                                    face = "bold", size = 20))
  
    
    
  # Looking at it divided by age
    # Step one - look at that range for 25-44
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Crude" &
                             ami$Break_Out == "25-44" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,5]  <- "Crude"
    finalrates[2:51,5] <- pctchangematrix[,2]
    x11()
    p1  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Ages 25-44") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red", 
                           midpoint = 0,
                           na.value = "gray80",
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p1

    # step two look at range for 45-64
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Crude" &
                             ami$Break_Out == "45-64" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,6]  <- "Crude"
    finalrates[2:51,6] <- pctchangematrix[,2]
    x11()
    p2  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Ages 45-64") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red",
                           na.value = "gray80",
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p2
    
    # step three look at range for 65+
    
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Crude" &
                             ami$Break_Out == "65+" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,7]  <- "Crude"
    finalrates[2:51,7] <- pctchangematrix[,2]
    x11()
    p3  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Ages 65+") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red",
                           na.value = "gray80",
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p3
    
    # ggarrange - look at Age breakdown (crude) last 5 years % change
    plot <- ggarrange(p1, p2, p3, ncol = 1, common.legend = TRUE, legend = "right", labels = "auto")
    
    x11()
    annotate_figure(plot, 
                    top = text_grob("Percent change in AMI Mortality from 2016-2020", 
                                    face = "bold", size = 20))
    
    
    
  # Look at it divided by race - other, hispanic, non hispanic black, non hispanic white
    # stratify by White, Black, Hispanic, Other
    # Step 1 - start with White
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                             ami$Break_Out == "Non-Hispanic White" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,8]  <- "Age Stdz"
    finalrates[2:51,8] <- pctchangematrix[,2]
    x11()
    p1  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "White") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red", 
                           midpoint = 0,
                           na.value = "gray80",
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p1
    
    # step two - Black
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                             ami$Break_Out == "Non-Hispanic Black" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,9]  <- "Age Stdz"
    finalrates[2:51,9] <- pctchangematrix[,2]
    x11()
    p2  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Black") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red",
                           na.value = "gray80",
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p2
    
    # step three - Hispanic
    
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                             ami$Break_Out == "Hispanic" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,10]  <- "Age Stdz"
    finalrates[2:51,10] <- pctchangematrix[,2]
    x11()
    p3  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Hispanic") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red",
                           na.value = "gray80",
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p3
    
    # Step 4 - other
    ami <- subset(nvs, subset = nvs$Topic == "Acute Myocardial Infarction (Heart Attack)")
    amipctchange <- subset(ami, subset = ami$Data_Value_Type == "Age-Standardized" &
                             ami$Break_Out == "Other" &
                             ami$LocationDesc != "Washington, DC" &
                             ami$LocationDesc != "United States"
    )
    pctchangematrix <- matrix(0,50,2)
    pctchangematrix[,1] <- tolower(amipctchange$LocationDesc[1:50])
    countvector <- seq(0,1000,50)
    tempnum = 0
    tempnuminitial = 0
    tempnumfinal = 0
    # Loop - averaging each row + 50 + 100, + 150... 1000 (50 states, alphabetical order)
    for (i in 1:50) {
      tempnuminitial <- amipctchange[(i + 800),15] # add 500 to start at year 2011
      tempnumfinal <- amipctchange[(i + 1000),15] # Add 1000 to start at year 2020
      pctchangematrix[i,2] <- (tempnumfinal - tempnuminitial) / tempnuminitial *100
      tempnum = 0 # reset tempnum for the next iteration of i
      tempnuminitial = 0
      tempnumfinal = 0
    }
    states_map <- map_data("state")
    amichangemap <- merge(states_map, pctchangematrix, by.x = "region", by.y = "V1")
    amichangemap <- arrange(amichangemap, group, order)
    finalrates[1,11]  <- "Age Stdz"
    finalrates[2:51,11] <- pctchangematrix[,2]
    x11()
    p4  <- ggplot(amichangemap, aes(long, lat, group = group, fill = as.numeric(V2))) +
      geom_polygon(colour = "black") +
      coord_map() +
      labs(title = "Other") +
      scale_fill_gradient2(name = "% change",
                           low = "darkgreen", 
                           mid = "white", 
                           high = "red",
                           na.value = "gray80",
                           midpoint = 0,
                           breaks = c(-50, -25, 0, 25, 50),
                           limits = c(-50, 50)
      ) +
      theme_bw() + 
      theme(plot.title = element_text(size = 14),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 12, vjust = 1),
            legend.key.size = unit(1.5, "cm"))
    p4
    
    # ggarrange - look at males vs females comparison last 5 years
    plot <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
              common.legend = TRUE, legend = "right", labels = "auto")
    x11()
    annotate_figure(plot, 
                    top = text_grob("Percent change in AMI Mortality from 2016-2020", 
                                    face = "bold", size = 20))

