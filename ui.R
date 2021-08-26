#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 04/16/2021
# Purpose: interact with results in UI
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------
# clear memory
rm(list = ls(all = TRUE))

# load packages, install if missing
packages <- c("data.table","dplyr", "magrittr","shiny", "ggplot2", "ggpubr", "scales", "rstudioapi") 

for (p in packages) {
  if (p %in% rownames(installed.packages()) == FALSE) install.packages(p)
  suppressMessages(library(p, character.only = T, warn.conflicts = FALSE, quietly = TRUE))
}
options(scipen = 10000)

# create data directory, setwd
h_root <- dirname(rstudioapi::getSourceEditorContext()$path)

#load calculated data
summaryDir <- file.path(h_root, "data_summary")
attrBurden <- rbind(fread(file.path(summaryDir, "attr_burd.csv")), 
                    fread(file.path(summaryDir, "attr_burd_prop.csv")))
all_burden <- fread(file.path(summaryDir, "all_burd.csv"))
pm_summ <- fread(file.path(summaryDir, "pm_summary.csv"))
pop_summary <- fread(file.path(summaryDir, "pop_summary.csv"))

# Define UI for dataset viewer app ----
#ui <- fluidPage(
ui = shinyUI(bootstrapPage(
  # App title ----
  titlePanel("Explore data"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Selector for choosing dataset ----
      selectInput(
        inputId = "raceOrEduc",
        label = "Aggregate by race or education?",
        choices = c("race", "education","nothing")
      ),
      selectInput(
        inputId = "Gender.Code",
        label = "Gender",
        choices = unique(all_burden$Gender.Code)
      ),
      selectInput(
        inputId = "rural_urban_class",
        label = "Rural Urban class",
        choices = c("All",setdiff(unique(all_burden$rural_urban_class),"All"))
      ),
      selectInput(
        inputId = "Region",
        label = "Region",
        choices = unique(all_burden$Region)
      ),
      selectInput(
        inputId = "measure1",
        label = "YLL or Deaths",
        choices = unique(all_burden$measure1)
      ),
      selectInput(
        inputId = "measure2",
        label = "rate",
        choices = unique(all_burden$measure2)
      ),
      selectInput(
        inputId = "source",
        label = "source burden data",
        choices = unique(all_burden$source)
      ),
      checkboxInput(
        inputId = "conf",
        label = "Display confidence interval",
        value = FALSE, width = NULL
      ),
      selectInput(
        inputId = "method",
        label = "which method used to calculate attributable burden",
        choices = c("burnett",setdiff(unique(attrBurden$method),"burnett"))
      ),
      selectInput(
        inputId = "pm_metric",
        label = "median or mean PM2.5 exposure",
        choices = unique(pm_summ$pm_metric)
      ),
      selectInput(
        inputId = "source2",
        label = "source population data",
        choices = unique(pop_summary$source2)
      ),
      selectInput(
        inputId = "scenario",
        label = "scenario",
        choices = unique(attrBurden$scenario)
      ),
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output:  ----
      plotOutput(outputId = "plot1", height = "1200px")
    )
  )
))