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

# Function to read in webpage and produce a dataframe of scored athletes
readPerfList = function(url){
    # Read in webpage HTML
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
    
    # Subset to Top 40 per event
    top40 = results %>%
        filter(PLACE <= 40) %>%
        filter(EVENT %in% c('800m', 'Mile', '3000m', '5000m')) %>%
        mutate(
            PTS = 41 - PLACE
        )
    
    # Group to one line per runner
    pts_grp = top40 %>%
        group_by(ATHLETE) %>%
        summarise(TEAM = max(TEAM), YEAR = max(YEAR), GENDER = max(Gender), POINTS = sum(PTS, na.rm = T), EVENTS = n_distinct(EVENT)) %>%
        mutate(PTS_PER_EVENT = POINTS / EVENTS) %>%
        arrange(-POINTS)
    
    # Return dataframe
    return(pts_grp)
}

# Current performance lists
rank2020 = readPerfList('https://www.tfrrs.org/lists/2770/2019_2020_NCAA_Div._I_Indoor_Qualifying/2020/i')
rank2019 = readPerfList('https://www.tfrrs.org/lists/2324/2018_2019_NCAA_Div._I_Indoor_Qualifying_(FINAL)/2019/i')

# Rename columns in 2019
names(rank2019) = c("ATHLETE", "TEAM", "YEAR", "GENDER", "POINTS_19", "EVENTS_19", "PTS_PER_EVENT_19")

# Select only required columns
rank2019 = rank2019 %>%
    select(ATHLETE, POINTS_19, EVENTS_19, PTS_PER_EVENT_19)

# Join tables
rank2020 = rank2020 %>%
    left_join(rank2019, by = c("ATHLETE"))

# Change NAs produced by join to 0
rank2020 = rank2020 %>%
    mutate(
        POINTS_19 = ifelse(is.na(POINTS_19), 0, POINTS_19),
        EVENTS_19 = ifelse(is.na(EVENTS_19), 0, EVENTS_19),
        PTS_PER_EVENT_19 = ifelse(is.na(PTS_PER_EVENT_19), 0, PTS_PER_EVENT_19)
    ) %>%
    mutate(
        PTS_PER_EVENT = round(PTS_PER_EVENT, 2),
        PTS_PER_EVENT_19 = round(PTS_PER_EVENT_19, 2)
    )

# Split by gender
men2020 = rank2020 %>% filter(GENDER == 'M')
women2020 = rank2020 %>% filter(GENDER == 'F')

# Function to get the rank
getRank = function(df){
    # Extract maximum total of any runner in each year
    maxPts20 = max(df$POINTS)
    maxPts19 = max(df$POINTS_19)
    # Extract maximum points per event for each year
    maxPtsPerEvent20 = max(df$PTS_PER_EVENT)
    maxPtsPerEvent19 = max(df$PTS_PER_EVENT_19)
    
    # Create RANK value
    df = df %>%
        mutate(
            RANK = case_when(
                    # Athlete has results from prior year
                    maxPts19 != 0 ~ (((POINTS / maxPts20) * 35) + ((POINTS_19 / maxPts19) * 15) + 
                                         ((PTS_PER_EVENT / maxPtsPerEvent20) * 35) + ((PTS_PER_EVENT_19 / maxPtsPerEvent19) * 15)),
                    # Athlete has no results from prior year
                    T ~ ((POINTS / maxPts20) * 50) + ((PTS_PER_EVENT / maxPtsPerEvent20) * 50))
        ) %>%
        arrange(-RANK) %>%
        mutate(
            RANK = round(RANK, 2)
        ) %>%
        select(ATHLETE, TEAM, YEAR, GENDER, RANK)
    
    # Subset to top 100
    df = df[1:100, ]
    
    # Give a numeric rank
    df$OVERALL_RANK = 1:100
    
    # Return df
    return(df)
}

# Run ranking function
men2020 = getRank(men2020)
women2020 = getRank(women2020)

# Create data set for overall
overall2020 = rbind(men2020, women2020)

# Get information on athlete
searchAthlete = function(runner){
    
    # Split name into first and last
    runner = strsplit(runner, " ")[[1]]
    
    # Define first & last names
    first_name = tolower(runner[1])
    last_name = tolower(runner[2])
    
    # Search for the athlete
    searchedRunner = rank2020[which(grepl(first_name, tolower(rank2020$ATHLETE)) & grepl(last_name, tolower(rank2020$ATHLETE))), ]
    
    return(searchedRunner)
}

# Get rank for each athlete
getRank = function(runner){
    # Split name into first and last
    runner = strsplit(runner, " ")[[1]]
    
    # Define first & last names
    first_name = tolower(runner[1])
    last_name = tolower(runner[2])
    
    # Search for the athlete
    searchedRunner = overall2020[which(grepl(first_name, tolower(overall2020$ATHLETE)) & grepl(last_name, tolower(overall2020$ATHLETE))), ]
    
    # Return rank
    return(searchedRunner$OVERALL_RANK)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$menRank = renderTable(men2020)
    
    output$womenRank = renderTable(women2020)
    
    # Functions to pull out runner info
    # Get name of first runner
    name1 = reactive({
        input$compareButton
        # Get the runner
        runner1 = searchAthlete(input$runnerName1)
        
        # Pull out the name
        runnerName1 = runner1$ATHLETE
    })
    
    # Get name of first runner
    name2 = reactive({
        input$compareButton
        # Get the runner
        runner2 = searchAthlete(input$runnerName2)
        
        # Pull out the name
        runnerName2 = runner2$ATHLETE
    })
    
    # Get the overall rank
    rank1 = reactive({
        input$compareButton
        # Pull out the name
        runnerRank1 = getRank(input$runnerName1)
    })
    
    # Get the overall rank
    rank2 = reactive({
        input$compareButton
        # Pull out the name
        runnerRank2 = getRank(input$runnerName2)
    })
    
    # Get the 2020 points
    pts1 = reactive({
        input$compareButton
        # Get the runner
        runner1 = searchAthlete(input$runnerName1)
        
        # Pull out the name
        runnerPts1 = runner1$POINTS
    })
    
    # Get the overall rank
    pts2 = reactive({
        input$compareButton
        # Get the runner
        runner2 = searchAthlete(input$runnerName2)
        
        # Pull out the name
        runnerPts2 = runner2$POINTS
    })
    
    # Get the points per event in 2020
    ptsPerEvent1 = reactive({
        input$compareButton
        # Get the runner
        runner1 = searchAthlete(input$runnerName1)
        
        # Pull out the name
        runnerPtsPerEvent1 = runner1$PTS_PER_EVENT
    })
    
    # Get the overall rank
    ptsPerEvent2 = reactive({
        input$compareButton
        # Get the runner
        runner2 = searchAthlete(input$runnerName2)
        
        # Pull out the name
        runnerPtsPerEvent2 = runner2$PTS_PER_EVENT
    })
    
    # Functions to output the value boxes
    # Runner 1 Name
    output$runnerName1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(name1())),
            subtitle = "Name"
        )
    })
    
    # Runner 2 Name
    output$runnerName2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(name2())),
            subtitle = "Name"
        )
    })
    
    # Runner 1 Rank
    output$runnerRank1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(rank1())),
            subtitle = "Rank"
        )
    })
    
    # Runner 2 Rank
    output$runnerRank2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(rank2())),
            subtitle = "Rank"
        )
    })
    
    # Runner 1 Points
    output$runnerPts1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(pts1())),
            subtitle = "2020 Points"
        )
    })
    
    # Runner 2 Points
    output$runnerPts2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(pts2())),
            subtitle = "2020 Points"
        )
    })
    
    # Runner 1 Pts Per Event
    output$runnerPtsPerEvent1 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(ptsPerEvent1())),
            subtitle = "2020 Points Per Event"
        )
    })
    
    # Runner 2 Points Per Event
    output$runnerPtsPerEvent2 <- renderValueBox({
        # Run on button press
        input$compareButton
        # Generate box
        valueBox(
            value = paste0(isolate(ptsPerEvent2())),
            subtitle = "2020 Points Per Event"
        )
    })
    
})

