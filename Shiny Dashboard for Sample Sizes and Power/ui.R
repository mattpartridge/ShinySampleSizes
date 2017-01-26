# Matt Partridge
# MS Project
# University of Minnesota
# Advisor: Julian Wolfson

# Target/Sampled Population
# Reference Population


############################## Libraries ##############################
library(shiny); library(shinydashboard); library(pwr); library(gsDesign); library(knitr)
############################## Libraries ##############################

ui = dashboardPage(
  dashboardHeader(title = "Sample Size Calculations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("One Sample Mean", tabName = "OM"),
      menuItem("One Sample Proportion", tabName = "OP"),
      menuItem("Two Sample Means", tabName = "TM"),
      menuItem("Two Sample Proportions", tabName = "TP"),
      menuItem("Time to Event", tabName = "TTE")
    )
  ),
  dashboardBody(
    tabItems(
      ############################## One Mean ##############################
      {
        tabItem(tabName = "OM",
                fluidRow(
                  box(width = 12,
                      selectizeInput(inputId = "solvefor_OM",
                                     label = p("Solve For: ", em("the desired output")),
                                     choices = c("Sample Size", "Power", "Precision"),
                                     selected = "Sample Size",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = "100%")),
                  uiOutput("OM")))
      },
      ############################## One Mean ##############################
      
      ############################## One Proportion ##############################
      {
        tabItem(tabName = "OP",
                fluidRow(
                  box(width = 12,
                      selectizeInput(inputId = "solvefor_OP",
                                     label = p("Solve For: ", em("the desired output")),
                                     choices = c("Sample Size", "Power", "Precision"),
                                     selected = "Sample Size",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = "100%")),
                  uiOutput("OP")))
      },
      ############################## One Proportion ##############################
      
      ############################## Two Means ##############################
      {
        tabItem(tabName = "TM",
                fluidRow(
                  box(width = 12,
                      selectizeInput(inputId = "solvefor_TM",
                                     label = p("Solve For: ", em("the desired output")),
                                     choices = c("Sample Size", "Power"),
                                     selected = "Sample Size",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = "100%")),
                  uiOutput("TM")))
      },
      ############################## Two Means ##############################
      
      ############################## Two Proportions ##############################
      {
        tabItem(tabName = "TP",
                fluidRow(
                  box(width = 12,
                      selectizeInput(inputId = "solvefor_TP",
                                     label = p("Solve For: ", em("the desired output")),
                                     choices = c("Sample Size", "Power"),
                                     selected = "Sample Size",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = "100%")),
                  uiOutput("TP")))
      },
      ############################## Two Proportions ##############################
      
      ############################## Time to Event ##############################
      {
        tabItem(tabName = "TTE",
                fluidRow(
                  box(width = 12,
                      selectizeInput(inputId = "solvefor_TTE",
                                     label = p("Solve For: ", em("the desired output")),
                                     choices = c("Sample Size", "Power"),
                                     selected = "Sample Size",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = "100%")),
                  # Study Information
                  box(width = 4,
                      selectizeInput(inputId = "enrollment_SE_TTE",
                                     label = p("Enrollment Schedule: ", em("At what point(s) during the study are participants recruited")),
                                     choices = c("All at Once", "Over a Period", "Continuous Throughout"),
                                     selected = "All at Once",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = NULL),
                      conditionalPanel("input.enrollment_SE_TTE != 'All at Once'",
                                       selectizeInput(inputId = "enrollmentdist_SE_TTE",
                                                      label = p("Distribution of Enrollment: ", em("How participants entered the study")),
                                                      # Should go through and change "Exponential Decay" to just "Exponential"
                                                      choices = c("Uniform", "Exponential"),
                                                      selected = "Uniform",
                                                      multiple = FALSE,
                                                      options = NULL,
                                                      width = NULL),
                                       conditionalPanel("input.enrollmentdist_SE_TTE == 'Exponential'",
                                                        numericInput(inputId = "gamma_N_TTE",
                                                                     label = p("Rate: ", em("This must be greater than 0 and can be greater than 1")),
                                                                     value = 0.5,
                                                                     min = 0,
                                                                     max = NA,
                                                                     step = 0.01,
                                                                     width = NULL),
                                                        sliderInput(inputId = "gamma_S_TTE",
                                                                    label = "",
                                                                    min = 0,
                                                                    max = 1,
                                                                    value = 0.5,
                                                                    step = 0.01,
                                                                    round = FALSE,
                                                                    width = NULL))),
                      numericInput(inputId = "studyduration_N_TTE",
                                   label = p("Study Duration: ", em("Length of study in units of time")),
                                   value = 5,
                                   min = 0,
                                   max = NA,
                                   step = 0.25,
                                   width = NULL),
                      sliderInput(inputId = "studyduration_S_TTE",
                                  label = "",
                                  min = 0,
                                  max = 10,
                                  value = 5,
                                  step = 0.25,
                                  round = FALSE,
                                  width = NULL),
                      conditionalPanel("input.enrollment_SE_TTE == 'Over a Period'",
                                       numericInput(inputId = "enrollmentduration_N_TTE",
                                                    label = p("Enrollment Duration: ", em("Length of enrollment in units of time")),
                                                    value = 2.5,
                                                    min = 0,
                                                    max = NA,
                                                    step = 0.25,
                                                    width = NULL),
                                       sliderInput(inputId = "enrollmentduration_S_TTE",
                                                   label = "",
                                                   min = 0,
                                                   max = 10,
                                                   value = 2.5,
                                                   step = 0.25,
                                                   round = FALSE,
                                                   width = NULL)),
                      numericInput(inputId = "ratio_N_TTE",
                                   # Should confirm this is correct and not one to two
                                   label = p("Sample Allocation Ratio: ", em("Ratio of the Targeted population to the Reference population")),
                                   value = 1,
                                   min = 0,
                                   max = NA,
                                   step = 0.1,
                                   width = NULL),
                      sliderInput(inputId = "ratio_S_TTE",
                                  label = "",
                                  min = 0,
                                  max = 5,
                                  value = 1,
                                  step = 0.1,
                                  round = FALSE,
                                  width = NULL)),
                  # Calculation Information
                  uiOutput("TTE")))
      }
      ############################## Time to Event ##############################
    ) # tabItems
  ) # dashboardBody
) # dashboardPage