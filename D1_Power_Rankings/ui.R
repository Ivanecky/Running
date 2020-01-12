# Load libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(stats)
library(knitr)
library(dplyr)
library(png)

# Define UI for application
ui = dashboardPage(
    dashboardHeader(title = "D1 Power Rankings"),
    dashboardSidebar(
        # Search boxes for athletes to compare
        textInput("runnerName1", "Runner 1", "Weini Kelati"),
        textInput("runnerName2", "Runner 2", "Edwin Kurgat"),
        # Search button
        actionButton("compareButton", "Compare Runners", style = "color: #000000; background-color: #42f44e; border-color: #2e6da4"),
        # Sidebar tab menu
        sidebarMenu(
            menuItem("Power Rankings", tabName = "powerRankings"),
            menuItem("Runner Comparison", tabName = "runnerCompare")
        )
    ),
    # Define body of application
    dashboardBody(
        # Create tabs
        tabItems(
            # Main page with rankings
            tabItem("powerRankings", 
                fluidRow(
                    column(12,
                           tableOutput('menRank')
                    )
                ),
                fluidRow(
                    column(12,
                           tableOutput('womenRank')
                    )
                )
            ),
            # Second page for runner compare tool
            tabItem("runnerCompare",
                # Athlete 1
                fluidRow(
                    valueBoxOutput("runnerName1", width = 3),
                    valueBoxOutput("runnerRank1", width = 3),
                    valueBoxOutput("runnerPts1", width = 3),
                    valueBoxOutput("runnerPtsPerEvent1", width = 3)
                ),
                # Athlete 2
                fluidRow(
                    valueBoxOutput("runnerName2", width = 3),
                    valueBoxOutput("runnerRank2", width = 3),
                    valueBoxOutput("runnerPts2", width = 3),
                    valueBoxOutput("runnerPtsPerEvent2", width = 3)
                )
            )
        )
    )
)


