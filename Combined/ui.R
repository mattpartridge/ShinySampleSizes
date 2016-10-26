# Matt Partridge
# MS Project
# University of Minnesota
# Advisor: Julian Wolfson


############################## Libraries ##############################
library(shiny); library(shinydashboard); library(pwr); library(gsDesign)
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
                                  label = "Solve For",
                                  choices = c("Sample Size", "Power", "Precision"),
                                  selected = "Sample Size",
                                  multiple = FALSE,
                                  # selectize = TRUE,
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
                                  label = "Solve For",
                                  choices = c("Sample Size", "Power", "Precision"),
                                  selected = "Sample Size",
                                  multiple = FALSE,
                                  # selectize = TRUE,
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
                                  label = "Solve For",
                                  choices = c("Sample Size", "Power"),
                                  selected = "Sample Size",
                                  multiple = FALSE,
                                  # selectize = TRUE,
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
                                  label = "Solve For",
                                  choices = c("Sample Size", "Power"),
                                  selected = "Sample Size",
                                  multiple = FALSE,
                                  # selectize = TRUE,
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
                                  label = "Solve For",
                                  choices = c("Sample Size", "Power"),
                                  selected = "Sample Size",
                                  multiple = FALSE,
                                  # selectize = TRUE,
                                  options = NULL,
                                  width = "100%")),
                  # Study Information
                  conditionalPanel("input.solvefor_TTE == 'Sample Size'",
                                   box(width = 4,
                                       selectizeInput(inputId = "enrollment_SE_TTE",
                                                   label = "Enrollment Schedule",
                                                   choices = c("All at Once", "Over a Period", "Continuous Throughout"),
                                                   selected = "All at Once",
                                                   multiple = FALSE,
                                                   # selectize = TRUE,
                                                   options = NULL,
                                                   width = NULL),
                                       conditionalPanel("input.enrollment_SE_TTE != 'All at Once'", # Would this work with "placeholder" option
                                       # conditionalPanel("input.enrollment_SE_TTE == 'Over a Period' | input.enrollment_SE_TTE == 'Continuous Throughout'",
                                                        selectizeInput(inputId = "enrollmentdist_SE_TTE",
                                                                    label = "Distribution of Enrollment",
                                                                    choices = c("Uniform", "Exponential Decay"),
                                                                    selected = "Uniform",
                                                                    multiple = FALSE,
                                                                    # selectize = TRUE,
                                                                    options = NULL,
                                                                    width = NULL),
                                                        conditionalPanel("input.enrollmentdist_SE_TTE == 'Exponential Decay'",
                                                                         numericInput(inputId = "gamma_N_TTE",
                                                                                      label = "Decay Rate",
                                                                                      value = 0.5,
                                                                                      min = 0,
                                                                                      max = 1,
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
                                                    label = "Study Duration (units of time)",
                                                    value = 5,
                                                    min = 0,
                                                    max = 10,
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
                                       conditionalPanel("input.enrollment_SE_TTE != 'All at Once'", # Will this work with "placeholder" option
                                       # conditionalPanel("input.enrollment_SE_TTE == 'Over a Period' | input.enrollment_SE_TTE == 'Continuous Throughout'",
                                                        numericInput(inputId = "enrollmentduration_N_TTE",
                                                                     label = "Enrollment Duration",
                                                                     value = 2.5,
                                                                     min = 0,
                                                                     max = 10,
                                                                     step = 0.25,
                                                                     width = NULL),
                                                        sliderInput(inputId = "enrollmentduration_S_TTE",
                                                                    label = "",
                                                                    min = 0,
                                                                    max = 10,
                                                                    value = 2.5,
                                                                    step = 0.25,
                                                                    round = FALSE,
                                                                    width = NULL)))),
                  # Calculation Information
                  uiOutput("CalculationInfo"),
                  uiOutput("Outputs")))
      }
      ############################## Time to Event ##############################
    ) # tabItems
  ) # dashboardBody
) # dashboardPage