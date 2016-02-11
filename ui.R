# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

# Total number of groups for two props when not on "N"
# Slider for N or add by increments
# Adjusting ranges for slider as you drag
# Unequal groups
# reactive UI renderUI****
# --> Still need to connect power and n

library(shiny); library(shinydashboard)
ui = dashboardPage(
  dashboardHeader(title = "Sample Size Calculations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("One Sample Proportion", tabName = "oneproportion"),
      menuItem("Two Sample Proportions", tabName = "twoproportions"),
      menuItem("Two Means", tabName = "twomeans"),
      menuItem("TEST", tabName = "TEST"))),
  dashboardBody(
    tabItems(
      
      ############################################
      ############## One Proportion ##############
      ############################################
      tabItem(tabName = "oneproportion",
              fluidRow(
                box(
                  numericInput(inputId = "Noneprop", label = "N", min = 0, max = NA, value = 100),
                  sliderInput(inputId = "pioneprop", label = "Pi", min = 0, max = 1, value = 0.5),
                  numericInput(inputId = "confoneprop", label = "Confidence", min = 0, max = 1, value = 0.95, step = 0.05),
                  numericInput(inputId = "MOEoneprop", label = "Margin of Error", value = 0.098),
                  sliderInput(inputId = "CIoneprop", label = "Confidence Interval", min = 0, max = 1, value = c(0.402, 0.598))))),
      
      #############################################
      ############## Two Proportions ##############
      #############################################
      tabItem(tabName = "twoproportions",
              fluidRow(
                box(
                  selectInput(inputId = "SolveFortwoprops", label = "Solve For",
                              choices = c("N", "Power")),
                  numericInput(inputId = "alphatwoprops", label = "Alpha Level:",
                               min = 0, max = 1, value = 0.05),
                  sliderInput(inputId = "betatwoprops", label = "Power:",
                              min = 0, max = 100, value = 80.0),
                  uiOutput(outputId = "PowerTwoProps")),
                box(
                  wellPanel(
                    sliderInput(inputId = "p1", label = "Proportion Affected by Treatment 1:",
                                min = 0, max = 1, value = 0.5),
                    sliderInput(inputId = "p2", label = "Proportion Affected by Treatment 2:",
                                min = 0, max = 1, value = 0.4)),
                  uiOutput(outputId = "NTwoProps"))),
              fluidRow(
                box(
                  sliderInput(inputId = "n.per.group.twoprops", label = "Number of People per Group", min = 0, max = 10000, value = 385),
                  sliderInput(inputId = "TotalN.twoprops", label = "Total Number of People", min = 0, max = 10000, value = 770)))),
      
      #######################################
      ############## Two Means ##############
      #######################################
      tabItem(tabName = "twomeans",
              fluidRow(
                box(
                  numericInput(inputId = "alphatwomeans", label = "Alpha Level:",
                               min = 0, max = 1, value = 0.05),
                  sliderInput(inputId = "betatwomeans", label = "Power:",
                              min = 0, max = 100, value = 80.0)),
                box(
                  numericInput(inputId = "deltamu", label = "Difference In Means", value = ""),
                  numericInput(inputId = "sd", label = "Standard Deviation", value = ""))),
              fluidRow(
                box(
                  numericInput(inputId = "n.per.group.twomeans", label = "Number of People per Group", value = ""),
                  numericInput(inputId = "TotalN.twomeans", label = "Total Number of People", value = "")))),
      
      ##################################
      ############## Test ##############
      ##################################
      tabItem(tabName = "TEST",
              fluidRow(
                box(
                  sliderInput(inputId = "Test1", label = "Test1", min = 0, max = 50, value = 25),
                  uiOutput("TEST2"))))
    ) # tabItems
  ) # dashboardBody
) # dashboardPage