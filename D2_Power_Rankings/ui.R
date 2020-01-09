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

# Define UI for application
ui = dashboardPage(
    dashboardHeader(title = "D2 Power Rankings"),
    dashboardSidebar(),
    # Define body of application
    dashboardBody(
        # Distribution of salaries
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
    )
)

