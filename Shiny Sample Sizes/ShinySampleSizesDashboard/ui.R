# Matt Partridge
# MS Project
# University of Minnesota
# Advisor: Julian Wolfson



############################## Libraries ##############################
library(shiny); library(shinydashboard); library(pwr); library(gsDesign); library(knitr)
############################## Libraries ##############################

ui = dashboardPage(skin = "black",
  dashboardHeader(
    title = "Sample Size and Power",
    titleWidth = 245),
  dashboardSidebar(
    width = 245,
    sidebarMenu(
      menuItem("Introduction", tabName = "Intro"),
      menuItem("One Sample Mean", tabName = "OM"),
      menuItem("One Sample Proportion", tabName = "OP"),
      menuItem("Two Sample Means", tabName = "TM"),
      menuItem("Two Sample Proportions", tabName = "TP"),
      menuItem("Time to Event", tabName = "TTE")
    )
  ),
  dashboardBody(
    tabItems(
      ############################## Inroduction ##############################
      {
        tabItem(tabName = "Intro",
                fluidRow(
                  uiOutput("Intro")))
      },
      ############################## Introduction ##############################
      
      
      ############################## One Mean ##############################
      {
        tabItem(tabName = "OM",
                fluidRow(
                  box(width = 12, color = "black",
                      selectizeInput(inputId = "calculate_OM",
                                     label = p("Calculate"),
                                     choices = c("Sample Size", "Power", "Margin of Error"),
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
                      selectizeInput(inputId = "calculate_OP",
                                     label = p("Calculate"),
                                     choices = c("Sample Size", "Power", "Margin of Error"),
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
                      selectizeInput(inputId = "calculate_TM",
                                     label = p("Calculate"),
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
                      selectizeInput(inputId = "calculate_TP",
                                     label = p("Calculate"),
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
                      selectizeInput(inputId = "calculate_TTE",
                                     label = p("Calculate"),
                                     choices = c("Sample Size", "Power"),
                                     selected = "Sample Size",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = "100%")),
                  box(width = 12,
                        HTML(paste("The null (H", tags$sub(0), ") and alternative (H", tags$sub(1), ") hypotheses are:", tags$br(),
                                   tags$strong("H", tags$sub(0)), ":  (target event rate / reference event rate) &leq; 1", tags$br(),
                                   tags$strong("H", tags$sub(1)), ":  (target event rate / reference event rate) > 1"))),
                  # Study Information
                  box(width = 4,
                      selectizeInput(inputId = "enrollment_SE_TTE",
                                     label = p("Enrollment Schedule: ", em("when do participants enroll")),
                                     choices = c("All at Once", "Over a Period", "Throughout"),
                                     selected = "Throughout",
                                     multiple = FALSE,
                                     options = NULL,
                                     width = NULL),
                      conditionalPanel("input.enrollment_SE_TTE != 'All at Once'",
                                       selectizeInput(inputId = "enrollmentdist_SE_TTE",
                                                      label = p("Distribution of Enrollment: ", em("shape of participants entering over time")),
                                                      choices = c("Uniform", "Exponential"),
                                                      selected = "Uniform",
                                                      multiple = FALSE,
                                                      options = NULL,
                                                      width = NULL),
                                       conditionalPanel("input.enrollmentdist_SE_TTE == 'Exponential'",
                                                        numericInput(inputId = "gamma_N_TTE",
                                                                     label = p("Exponential Rate: ", em("of growth or decay(<0 for concave entry, >0 for convex entry)")),
                                                                     value = 0.5,
                                                                     min = NA,
                                                                     max = NA,
                                                                     step = 0.01,
                                                                     width = NULL),
                                                        sliderInput(inputId = "gamma_S_TTE",
                                                                    label = "",
                                                                    min = -3.74,
                                                                    max = 4.74,
                                                                    value = 0.5,
                                                                    step = 0.01,
                                                                    round = FALSE,
                                                                    width = NULL))),
                      numericInput(inputId = "studyduration_N_TTE",
                                   label = p("Study Duration: ", em("length of study in units of time")),
                                   value = 15,
                                   min = 0,
                                   max = NA,
                                   step = 0.25,
                                   width = NULL),
                      sliderInput(inputId = "studyduration_S_TTE",
                                  label = "",
                                  min = 0,
                                  max = 38,
                                  value = 15,
                                  step = 0.25,
                                  round = FALSE,
                                  width = NULL),
                      conditionalPanel("input.enrollment_SE_TTE == 'Over a Period'",
                                       numericInput(inputId = "enrollmentduration_N_TTE",
                                                    label = p("Enrollment Duration: ", em("length of enrollment in units of time")),
                                                    value = 5,
                                                    min = 0,
                                                    max = NA,
                                                    step = 0.25,
                                                    width = NULL),
                                       sliderInput(inputId = "enrollmentduration_S_TTE",
                                                   label = "",
                                                   min = 0,
                                                   max = 15,
                                                   value = 5,
                                                   step = 0.25,
                                                   round = FALSE,
                                                   width = NULL)),
                      numericInput(inputId = "ratio_N_TTE",
                                   label = p("Sample Allocation Ratio: ", em("of the Targeted sample to the Reference sample")),
                                   value = 1.09,
                                   min = 0,
                                   max = NA,
                                   step = 0.01,
                                   width = NULL),
                      sliderInput(inputId = "ratio_S_TTE",
                                  label = "",
                                  min = 0,
                                  max = 7.35,
                                  value = 1.09,
                                  step = 0.01,
                                  round = FALSE,
                                  width = NULL)),
                  # Calculation Information
                  uiOutput("TTE")))
      }
      ############################## Time to Event ##############################
    ) # tabItems
  ) # dashboardBody
) # dashboardPage