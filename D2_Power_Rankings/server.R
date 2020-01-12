# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(stats)
library(knitr)
library(DT)
library(dplyr)
library(rvest)

# Read in TFRRS page
url = 'https://www.tfrrs.org/lists/2771/2019_2020_NCAA_Div._II_Indoor_Qualifying/2020/i?gender=f'

# Get HTML
webpage = read_html(url)

# Read in tables
tables = html_table(webpage)

# Extract first df
results = as.data.frame(tables[1])
names(results) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE")

# Create gender column
results$Gender = "M"

# Table for relays data
relays = data.frame()

# Extract all the tables into dataframes
for( i in 2:length(tables) )
{
    # Temporary df
    temp = as.data.frame(tables[i])
    # Make sure table has 7 columns for binding - some tables using conversions for field events have 8 columns
    if ( ncol(temp) == 7 )
    { 
        # Rename and bind
        names(temp) = c("PLACE", "ATHLETE", "YEAR", "TEAM", "TIME", "MEET", "MEET.DATE")
        
        # Check to assign gender - womens tables should be "even" numbers
        if ( i%%2 == 0 ) 
        { 
            temp$Gender = "F" 
        }
        else { temp$Gender = "M" }
        
        results = rbind(results, temp) 
    }
    else if( ncol(temp) == 6)
    {
        relays = rbind(relays, temp)
    }
}

# Events are not tied in to data - need to add them based on times
results = results %>%
    mutate(EVENT = case_when(
        grepl('6\\.|7\\.', TIME) & (substr(TIME,1,1) %in% c('6','7')) ~ '60m', # Have to add the substr clause to get only those that start with a 6
        grepl('21\\.|22\\.|23\\.|24\\.|25\\.|26\\.', TIME) & (substr(TIME,1,2) %in% c('21','22','23','24','25','26')) ~ '200m',
        grepl('44\\.|45\\.|46\\.|47\\.|48\\.|49\\.|50\\.|51\\.|52\\.|53\\.|54\\.|55\\.|56\\.|57\\.|58\\.|59\\.', TIME) & 
            (substr(TIME,1,2) %in% c('44','45','46','47','48','49','50','51','52','53','54','55','56','57','58','59')) ~ '400m',
        grepl('1:|2:', TIME) & (substr(TIME,1,2) %in% c('1:','2:')) ~ '800m',
        grepl('3:|4:|5:', TIME) & (substr(TIME,1,2) %in% c('3:','4:','5:')) ~ 'Mile',
        grepl('7:|8:|9:|10:|11:', TIME) & (substr(TIME,1,2) %in% c('7:', '8:', '9:', '10', '11')) ~ '3000m',
        grepl('13:|14:|15:|16:|17:|18:', TIME) & (substr(TIME,1,3) %in% c('13:','14:','15:','16:','17:','18:')) ~ '5000m',
        T ~ 'OTHER'
    ))

# Relays data
relays = relays %>%
    mutate(EVENT = case_when(
        grepl('3:|4:', TIME) & (substr(TIME,1,2) %in% c('3:','4:')) ~ '4x400m',
        grepl('9:|10:|11:|12:|13:|14:|15:|16:', TIME) & (substr(TIME,1,2) %in% c('9:','10','11','12','13','14','15','16')) ~ 'DMR',
        T ~ 'OTHER'
    ))

# Get only the top 40 per event and assign points
top40 = results %>%
    filter(PLACE <= 40) %>%
    filter(EVENT %in% c('800m', 'Mile', '3000m', '5000m')) %>%
    mutate(
        PTS = 41 - PLACE
    )

# Group athletes to single line and get point totals
# Order by PTS_PER_EVENT
pts_grp = top40 %>%
    group_by(ATHLETE) %>%
    summarise(TEAM = max(TEAM), YEAR = max(YEAR), GENDER = max(Gender), POINTS = sum(PTS, na.rm = T), EVENTS = n_distinct(EVENT)) %>%
    mutate(PTS_PER_EVENT = POINTS / EVENTS) %>%
    arrange(-PTS_PER_EVENT)

# Separate by gender
top_men = pts_grp %>%
    filter(GENDER == 'M')
top_women = pts_grp %>%
    filter(GENDER == 'F')

# Only display top 50 for each gender
top_men = top_men[1:50, ]
top_women = top_women[1:50, ]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$menRank = renderTable(top_men)
    
    output$womenRank = renderTable(top_women)

})
