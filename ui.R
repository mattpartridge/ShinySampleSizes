# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

###############################################
############## Things To Look At ##############
###############################################
# See server.R

library(shiny); library(shinydashboard); library(pwr)
ui = dashboardPage(
  dashboardHeader(title = "Sample Size Calculations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("One Sample Proportion", tabName = "oneproportion"),
      menuItem("Two Sample Proportions", tabName = "twoproportions"),
      menuItem("Two Means", tabName = "twomeans"))),
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
                box(width = 12,
                    selectInput(inputId = "solvefortwoprops", label = "Solve For", choices = c("Sample Size", "Power"), width = "100%"))),
              fluidRow(
                # Inputs
                box(width = 6,
                    # Alpha
                    numericInput(inputId = "alphatwoprops.n", label = "Alpha Level:", min = 0, max = 1, value = 0.05, step = 0.01),
                    sliderInput(inputId = "alphatwoprops.s", label = NULL, min = 0, max = 1, value = 0.05, step = 0.01),
                    # Beta
                    numericInput(inputId = "betatwoprops.n", label = "Power:", min = 0, max = 100, value = 80.0),
                    sliderInput(inputId = "betatwoprops.s", label = NULL, min = 0, max = 100, value = 80.0),
                    # Proportions
                    numericInput(inputId = "p1.n", label = "Proportion Affected by Treatment 1:", min = 0, max = 1, value = 0.5, step = 0.01),
                    sliderInput(inputId = "p1.s", label = NULL, min = 0, max = 1, value = 0.5, step = 0.01),
                    numericInput(inputId = "p2.n", label = "Proportion Affected by Treatment 2:", min = 0, max = 1, value = 0.4, step = 0.01),
                    sliderInput(inputId = "p2.s", label = NULL, min = 0, max = 1, value = 0.4, step = 0.01)),
                # Sample Size
                #                   conditionalPanel("input.solvefortwoprops == 'Sample Size'",
                #                                    # Alpha
                #                                    numericInput(inputId = "alphatwoprops.n", label = "Alpha Level:", min = 0, max = 1, value = 0.05, step = 0.01),
                #                                    sliderInput(inputId = "alphatwoprops.s", label = NULL, min = 0, max = 1, value = 0.05, step = 0.01),
                #                                    # Beta
                #                                    numericInput(inputId = "betatwoprops.n", label = "Power:", min = 0, max = 100, value = 80.0),
                #                                    sliderInput(inputId = "betatwoprops.s", label = NULL, min = 0, max = 100, value = 80.0),
                #                                    # Proportions
                #                                    numericInput(inputId = "p1.n", label = "Proportion Affected by Treatment 1:", min = 0, max = 1, value = 0.5, step = 0.01),
                #                                    sliderInput(inputId = "p1.s", label = NULL, min = 0, max = 1, value = 0.5, step = 0.01),
                #                                    numericInput(inputId = "p2.n", label = "Proportion Affected by Treatment 2:", min = 0, max = 1, value = 0.4, step = 0.01),
                #                                    sliderInput(inputId = "p2.s", label = NULL, min = 0, max = 1, value = 0.4, step = 0.01)),
                #                 # Power
                #                 conditionalPanel("input.solvefortwoprops == 'Power'",
                #                                  # Alpha
                #                                  numericInput(inputId = "alphatwoprops.n", label = "Alpha Level:", min = 0, max = 1, value = 0.05, step = 0.01),
                #                                  sliderInput(inputId = "alphatwoprops.s", label = NULL, min = 0, max = 1, value = 0.05, step = 0.01),
                #                                  # Group Sizes
                #                                  numericInput(inputId = "n.per.group.twoprops.n", label = "Number of People per Group", min = 0, max = 10000, value = 385),
                #                                  sliderInput(inputId = "n.per.group.twoprops.s", label = NULL, min = 0, max = 10000, value = 385),
                #                                  numericInput(inputId = "TotalN.twoprops.n", label = "Total Number of People", min = 0, max = 10000, value = 770, step = 2),
                #                                  sliderInput(inputId = "TotalN.twoprops.s", label = NULL, min = 0, max = 10000, value = 770, step = 2),
                #                                  # Proportions
                #                                  numericInput(inputId = "p1.n", label = "Proportion Affected by Treatment 1:", min = 0, max = 1, value = 0.5, step = 0.01),
                #                                  sliderInput(inputId = "p1.s", label = NULL, min = 0, max = 1, value = 0.5, step = 0.01),
                #                                  numericInput(inputId = "p2.n", label = "Proportion Affected by Treatment 2:", min = 0, max = 1, value = 0.4, step = 0.01),
                #                                  sliderInput(inputId = "p2.s", label = NULL, min = 0, max = 1, value = 0.4, step = 0.01))),
                # Outputs
                box(width = 6,
                    numericInput(inputId = "n.per.group.twoprops.n", label = "Number of People per Group", min = 0, max = 10000, value = 385),
                    sliderInput(inputId = "n.per.group.twoprops.s", label = NULL, min = 0, max = 10000, value = 385),
                    numericInput(inputId = "TotalN.twoprops.n", label = "Total Number of People", min = 0, max = 10000, value = 770, step = 2),
                    sliderInput(inputId = "TotalN.twoprops.s", label = NULL, min = 0, max = 10000, value = 770, step = 2)))),
                # Sample Size
                #                 conditionalPanel("input.solvefortwoprops == 'Sample Size'",
                #                                  # Group Sizes
                #                                  numericInput(inputId = "n.per.group.twoprops.n", label = "Number of People per Group", min = 0, max = 10000, value = 385),
                #                                  sliderInput(inputId = "n.per.group.twoprops.s", label = NULL, min = 0, max = 10000, value = 385),
                #                                  numericInput(inputId = "TotalN.twoprops.n", label = "Total Number of People", min = 0, max = 10000, value = 770, step = 2),
                #                                  sliderInput(inputId = "TotalN.twoprops.s", label = NULL, min = 0, max = 10000, value = 770, step = 2)),
                #                 # Power
                #                 conditionalPanel("input.solvefortwoprops == 'Power'",
                #                                  # Beta
                #                                  numericInput(inputId = "betatwoprops.n", label = "Power:", min = 0, max = 100, value = 80.0),
                #                                  sliderInput(inputId = "betatwoprops.s", label = NULL, min = 0, max = 100, value = 80.0))))),
                
                #######################################
                ############## Two Means ##############
                #######################################
                tabItem(tabName = "twomeans",
                        fluidRow(
                          box(
                            numericInput(inputId = "alphatwomeans", label = "Alpha Level:", min = 0, max = 1, value = 0.05),
                            sliderInput(inputId = "betatwomeans", label = "Power:", min = 0, max = 100, value = 80.0)),
                          box(
                            numericInput(inputId = "deltamu", label = "Difference In Means", value = ""),
                            numericInput(inputId = "sd", label = "Standard Deviation", value = ""))),
                        fluidRow(
                          box(
                            numericInput(inputId = "n.per.group.twomeans", label = "Number of People per Group", value = ""),
                            numericInput(inputId = "TotalN.twomeans", label = "Total Number of People", value = ""))))
              ) # tabItems
      ) # dashboardBody
    ) # dashboardPage