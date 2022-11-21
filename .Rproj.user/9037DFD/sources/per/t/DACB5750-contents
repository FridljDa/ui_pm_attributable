#-------------------Header------------------------------------------------
# Author: Daniel Fridljand
# Date: 04/16/2021
# Purpose: interact with results in UI
#
#***************************************************************************

#------------------SET-UP--------------------------------------------------

library(data.table)
library(dplyr)
library(magrittr)
library(shiny)
library(ggplot2)
library(ggpubr)
library(scales)
library(here)

options(scipen = 10000)

#load calculated data
file_list <- list.files(here("data_summary"))
file_list <- here("data_summary", file_list[grepl("attr_bur", file_list)])
attrBurden <- lapply(file_list, fread) %>% rbindlist(use.names = TRUE)
rm(file_list)

all_burden <- fread(here("data_summary", "all_burd.csv"))
pm_summ <- fread(here("data_summary", "pm_summary.csv"))
pop_summary <- fread(here("data_summary", "pop_summary.csv"))

all_burden <- all_burden %>% filter(rural_urban_class != "Unknown")
attrBurden <- attrBurden %>% filter(rural_urban_class != "Unknown")
pm_summ <- pm_summ %>% filter(rural_urban_class != "Unknown")
pop_summary <- pop_summary %>% filter(rural_urban_class != "Unknown")

##---shiny app---
shinyApp(
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
          label = "Aggregate by which factor?",
          choices = c("race", "education","nothing","rural urban class")
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
          choices = c("United States",setdiff(unique(all_burden$Region),"United States"))
        ),
        selectInput(
          inputId = "measure1",
          label = "YLL or Deaths",
          choices = unique(all_burden$measure1)
        ),
        selectInput(
          inputId = "measure2",
          label = "rate",
          choices = c("age-adjusted rate per 100,000",setdiff(unique(all_burden$measure2),"age-adjusted rate per 100,000"))
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
          label = "concentration-response function",
          choices = c("di_gee",setdiff(unique(attrBurden$method),"di_gee"))
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
          choices = c("real",setdiff(unique(attrBurden$scenario),"real")) 
        ),
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        
        # Output:  ----
        plotOutput(outputId = "plot1", height = "1200px")
      )
    )
  )),
  # Server.R
  server = function(input, output) {
    output$plot1 <- renderPlot({
      # get Input
      Gender.CodeI <- input$Gender.Code
      rural_urban_classI <- input$rural_urban_class
      RegionI <- input$Region
      measure1I <- input$measure1
      measure2I <- input$measure2
      sourceI <- input$source
      methodI <- input$method
      pm_metricI <- input$pm_metric
      source2I <- input$source2
      scenarioI <- input$scenario
      
      # filter data accordingly
      allBurden1 <- all_burden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & attr == "overall")
      allBurden2 <- all_burden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & attr == "total")
      attrBurden1 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method & measure3 == "value" & scenarioI == scenario)
      attrBurden2 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method & measure3 == "prop. of overall burden" & scenarioI == scenario)
      attrBurden3 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method 
                                           & !(Ethnicity == "All, All Origins" & Education == 666)  & scenarioI == scenario)
      attrBurden4 <- attrBurden %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & measure1 == measure1I & measure2 == measure2I & source == sourceI & methodI == method & measure3 == "prop. of total burden" & scenarioI == scenario)
      
      pm_summ1 <- pm_summ %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & pm_metric == pm_metricI & scenarioI == scenario)
      pop_summary1 <- pop_summary %>% filter(Gender.Code == Gender.CodeI & Region == RegionI & source2 == source2I)
      
      if(rural_urban_classI != "All"){
        allBurden1 <- allBurden1 %>% filter(Year >= 2000)
        allBurden2 <- allBurden2 %>% filter(Year >= 2000)
        attrBurden1 <- attrBurden1 %>% filter(Year >= 2000)
        attrBurden2 <- attrBurden2 %>% filter(Year >= 2000)
        attrBurden3 <- attrBurden3 %>% filter(Year >= 2000)
        attrBurden4 <- attrBurden4 %>% filter(Year >= 2000)
        pm_summ1 <- pm_summ1 %>% filter(Year >= 2000)
        pop_summary1 <- pop_summary1 %>% filter(Year >= 2000)
      }
      
      group.colors <- c(RColorBrewer::brewer.pal(n = 12, name = "Paired")[c(1:6,8:10, 12)], 
                        RColorBrewer::brewer.pal(n = 6, name = "Spectral")[1:2])
      group.colors[c(12,2)] <- group.colors[c(2,12)] 
      names(group.colors) <- c("NH White",
                               "Hispanic or Latino White",
                               "Black American",
                               "White",
                               "Asian or Pacific Islander",
                               "American Indian or Alaska Native",
                               
                               "High school graduate or lower",
                               "Some college education but no 4-year college degree",
                               "4-year college graduate or higher",
                               
                               "Non metro",
                               "Large metro",
                               "Small-medium metro"
      )
      
      if (input$raceOrEduc == "race") {
        allBurden1 <- allBurden1 %>% filter(Education == 666 & Ethnicity != "All, All Origins"  & rural_urban_classI == rural_urban_class) 
        allBurden2 <- allBurden2 %>% filter(Education == 666 & Ethnicity != "All, All Origins"  & rural_urban_classI == rural_urban_class)
        attrBurden1 <- attrBurden1 %>% filter(Education == 666 & Ethnicity != "All, All Origins"  & rural_urban_classI == rural_urban_class)
        attrBurden2 <- attrBurden2 %>% filter(Education == 666 & Ethnicity != "All, All Origins"  & rural_urban_classI == rural_urban_class)
        attrBurden3 <- attrBurden3 %>% filter(Education == 666 & Ethnicity != "All, All Origins"  & rural_urban_classI == rural_urban_class 
                                              & measure3 =="proportion of disparity to Black or African American attributable"
                                              & Year >= 2000
                                              #& measure3 =="proportion of disparity to average"
                                              )
        attrBurden4 <- attrBurden4 %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_classI == rural_urban_class)
        pm_summ1 <- pm_summ1 %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_classI == rural_urban_class)
        pop_summary1 <- pop_summary1 %>% filter(Education == 666 & Ethnicity != "All, All Origins" & rural_urban_classI == rural_urban_class)
        
        g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = Ethnicity))
        g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = Ethnicity))
        g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Ethnicity))
        g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Ethnicity))
        g5 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Ethnicity))
        g6 <- ggplot(pop_summary1, aes(x = Year, y = Population, color = Ethnicity))
        g7 <- ggplot(attrBurden3, aes(x = Year, y = mean, color = Ethnicity))
        g8 <- ggplot(attrBurden4, aes(x = Year, y = mean, color = Ethnicity))
        
        group.colors <- group.colors[1:6]
      } else if(input$raceOrEduc == "education"){
        allBurden1 <- allBurden1 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        allBurden2 <- allBurden2 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        attrBurden1 <- attrBurden1 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        attrBurden2 <- attrBurden2 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        attrBurden3 <- attrBurden3 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class & measure3 =="proportion of disparity to lower educational attainment")
        attrBurden4 <- attrBurden4 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        pm_summ1 <- pm_summ1 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        pop_summary1 <- pop_summary1 %>% filter(Education != 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        
        g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = Education))
        g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = Education))
        g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = Education))
        g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = Education))
        g5 <- ggplot(pm_summ1, aes(x = Year, y = value, color = Education))
        g6 <- ggplot(pop_summary1, aes(x = Year, y = Population, color = Education))
        g7 <- ggplot(attrBurden3, aes(x = Year, y = mean, color = Education))
        g8 <- ggplot(attrBurden4, aes(x = Year, y = mean, color = Education))
        
        group.colors <- group.colors[7:9]
      }else if(input$raceOrEduc == "nothing"){
        allBurden1 <- allBurden1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        allBurden2 <- allBurden2 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        attrBurden1 <- attrBurden1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        attrBurden2 <- attrBurden2 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        attrBurden3 <- attrBurden3 %>% filter(FALSE)
        attrBurden4 <- attrBurden4 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        pm_summ1 <- pm_summ1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        pop_summary1 <- pop_summary1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_classI == rural_urban_class)
        
        g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value))
        g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value))
        g3 <- ggplot(attrBurden1, aes(x = Year, y = mean))
        g4 <- ggplot(attrBurden2, aes(x = Year, y = mean))
        g5 <- ggplot(pm_summ1, aes(x = Year, y = value))
        g6 <- ggplot(pop_summary1, aes(x = Year, y = Population))
        g7 <- ggplot(attrBurden3, aes(x = Year, y = mean))
        g8 <- ggplot(attrBurden4, aes(x = Year, y = mean))
        
      }else if(input$raceOrEduc == "rural urban class"){
        allBurden1 <- allBurden1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
        allBurden2 <- allBurden2 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
        attrBurden1 <- attrBurden1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
        attrBurden2 <- attrBurden2 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
        attrBurden3 <- attrBurden3 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & measure3 =="proportion of disparity to non-metro" & Year >= 2000)
        attrBurden4 <- attrBurden4 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
        pm_summ1 <- pm_summ1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All"& Year >= 2000)
        pop_summary1 <- pop_summary1 %>% filter(Education == 666 & Ethnicity == "All, All Origins" & rural_urban_class != "All" & Year >= 2000)
        
        g1 <- ggplot(allBurden1, aes(x = Year, y = overall_value, color = rural_urban_class))
        g2 <- ggplot(allBurden2, aes(x = Year, y = overall_value, color = rural_urban_class))
        g3 <- ggplot(attrBurden1, aes(x = Year, y = mean, color = rural_urban_class))
        g4 <- ggplot(attrBurden2, aes(x = Year, y = mean, color = rural_urban_class))
        g5 <- ggplot(pm_summ1, aes(x = Year, y = value, color = rural_urban_class))
        g6 <- ggplot(pop_summary1, aes(x = Year, y = Population, color = rural_urban_class))
        g7 <- ggplot(attrBurden3, aes(x = Year, y = mean, color = rural_urban_class))
        g8 <- ggplot(attrBurden4, aes(x = Year, y = mean, color = rural_urban_class))
        
        group.colors <- group.colors[10:12]
      }
      
      g1 <- g1 + geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) +scale_x_continuous(breaks= pretty_breaks())+ ylim(0, NA)   + ggtitle("all-cause burden")
      g2 <- g2 + geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) +scale_x_continuous(breaks= pretty_breaks())+ ylim(0, NA)  + ggtitle("total burden from causes associated with PM2.5 exposure ",
                                                                                                                                                                    subtitle = "(resp_copd, lri, neo_lung, t2_dm, cvd_ihd, cvd_stroke)"
      )
      g3 <- g3 + geom_line(size = 1) + xlab("Year") + ylab(paste0(measure1I, ", ", measure2I)) +scale_x_continuous(breaks= pretty_breaks())+ ylim(0, NA)  + ggtitle("directly attributable to PM2.5 exposure")
      g4 <- g4 + geom_line(size = 1) + xlab("Year") + ylab("%") +scale_x_continuous(breaks= pretty_breaks()) + ggtitle("proportion of all-cause burden directly attributable to PM2.5 exposure")
      
      g5 <- g5 + geom_line(size = 1) + xlab("Year") + ylab("μg/m3") +scale_x_continuous(breaks= pretty_breaks()) +ggtitle(paste("population-weighted", pm_metricI, "of PM2.5  exposure"))
      g6 <- g6 + geom_line(size = 1) + xlab("Year") + ylab("Population")+scale_x_continuous(breaks= pretty_breaks()) + expand_limits(y = c(0, NA)) + ggtitle("Population size") + scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))
      g7 <- g7 + geom_line(size = 1) + xlab("Year") + ylab("%")  +scale_x_continuous(breaks= pretty_breaks())+ ggtitle(paste(unique(attrBurden3$measure3)))
      g8 <- g8 + geom_line(size = 1) + xlab("Year") + ylab("%") +scale_x_continuous(breaks= pretty_breaks())+ ylim(0, NA)  + ggtitle("proportion of total burden directly attributable to PM2.5 exposure")
      
      if (input$conf) {
        g3 <- g3 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
        g4 <- g4 + geom_ribbon(aes(ymin = lower, ymax = upper), linetype = 0, alpha = 0.1)
      }
      
      if(input$raceOrEduc != "nothing"){ #TODO
        g1 <- g1+  scale_colour_manual(values=group.colors)
        g2 <- g2+  scale_colour_manual(values=group.colors)
        g3 <- g3+  scale_colour_manual(values=group.colors)
        g4 <- g4+  scale_colour_manual(values=group.colors)
        g5 <- g5+  scale_colour_manual(values=group.colors)
        g6 <- g6+  scale_colour_manual(values=group.colors)
        g7 <- g7+  scale_colour_manual(values=group.colors)
        g8 <- g8+  scale_colour_manual(values=group.colors)
      }
      
      g_comb <- ggarrange(g1, #g2, 
                          g3, g4, g5, g6, g7,#g8,
                          ncol = 2, nrow = 4,
                          common.legend = TRUE, legend = "top", 
                          labels = "AUTO") 
      g_comb
    })
  }
)