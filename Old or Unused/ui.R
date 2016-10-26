# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

###############################################
############## Things To Look At ##############
###############################################
# See server.R
library(shiny); library(shinydashboard); library(pwr); library(gsDesign)
ui = dashboardPage(
  dashboardHeader(title = "Sample Size Calculations"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("One Sample Mean", tabName = "onemean"),
      menuItem("One Sample Proportion", tabName = "oneproportion"),
      menuItem("Comparison of Two Means", tabName = "twomeans"),
      menuItem("Comparison of Two Proportions", tabName = "twoproportions"),
      menuItem("Time to Event", tabName = "timetoevent"))),
  dashboardBody(
    tabItems(
      
      ######################################
      ############## One Mean ##############
      ######################################
      tabItem(tabName = "onemean",
              fluidRow(
                box(width = 12,
                    selectInput(inputId = "solveforonemean", label = "Solve For", choices = c("Sample Size", "Power", "Precision"), selected = "Precision", width = "100%"))),
              conditionalPanel(
                condition = "input.solveforonemean == 'Sample Size'",
                fluidRow(
                  # Inputs
                  box(
                    numericInput(inputId = "muonemean.n", label = "Mu", min = 0, max = 2, value = 0.5, step = 0.1),
                    sliderInput(inputId = "muonemean.s", label = "", min = 0, max = 2, value = 0.5, step = 0.1),
                    numericInput(inputId = "sdonemean.n", label = "SD", min = 0, max = NA, value = 1, step = 0.1),
                    sliderInput(inputId = "sdonmean.s", label = "", min = 0, max = 5, value = 1, step = 0.1),
                    numericInput(inputId = "nullmuonemean.n", label = "Null Hypothesis Mean", min = 0, max = 2, value = 1, step = 0.1),
                    sliderInput(inputId = "nullmuonemean.s", label = "", min = 0, max = 2, value = 1, step = 0.1),
                    numericInput(inputId = "alphaonemean.n", label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01),
                    sliderInput(inputId = "alphaonemean.s", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
                    numericInput(inputId = "poweronemean.n", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
                    sliderInput(inputId = "poweronemean.s", label = "", min = 0, max = 1, value = 0.8, step = 0.05)),
                  # Outputs
                  box(
                    numericInput(inputId = "Nonemean.n", label = "N", min = 0, max = NA, value = 100),
                    sliderInput(inputId = "Nonemean.s", label = "", min = 0, max = 1000, value = 100)))),
              conditionalPanel(
                condition = "input.solveforonemean == 'Power'",
                fluidRow(
                  # Inputs
                  box(
                    numericInput(inputId = "muonemean.n", label = "Mu", min = 0, max = 2, value = 0.5, step = 0.1),
                    sliderInput(inputId = "muonemean.s", label = "", min = 0, max = 2, value = 0.5, step = 0.1),
                    numericInput(inputId = "sdonemean.n", label = "SD", min = 0, max = NA, value = 1, step = 0.1),
                    sliderInput(inputId = "sdonmean.s", label = "", min = 0, max = 5, value = 1, step = 0.1),
                    numericInput(inputId = "Nonemean.n", label = "N", min = 0, max = NA, value = 100),
                    sliderInput(inputId = "Nonemean.s", label = "", min = 0, max = 1000, value = 100),
                    numericInput(inputId = "nullmuonemean.n", label = "Null Hypothesis Mean", min = 0, max = 2, value = 1, step = 0.1),
                    sliderInput(inputId = "nullmuonemean.s", label = "", min = 0, max = 2, value = 1, step = 0.1),
                    numericInput(inputId = "alphaonemean.n", label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01),
                    sliderInput(inputId = "alphaonemean.s", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
                  # Outputs
                  box(
                    numericInput(inputId = "poweronemean.n", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
                    sliderInput(inputId = "poweronemean.s", label = "", min = 0, max = 1, value = 0.8, step = 0.05)))),
              conditionalPanel(
                condition = "input.solveforonemean == 'Precision'",
                fluidRow(
                  # Inputs
                  box(
                    numericInput(inputId = "muonemean.n", label = "Mu", min = 0, max = 2, value = 0.5, step = 0.1),
                    sliderInput(inputId = "muonemean.s", label = "", min = 0, max = 2, value = 0.5, step = 0.1),
                    numericInput(inputId = "sdonemean.n", label = "SD", min = 0, max = NA, value = 1, step = 0.1),
                    sliderInput(inputId = "sdonmean.s", label = "", min = 0, max = 5, value = 1, step = 0.1),
                    numericInput(inputId = "Nonemean.n", label = "N", min = 0, max = NA, value = 100),
                    sliderInput(inputId = "Nonemean.s", label = "", min = 0, max = 1000, value = 100),
                    numericInput(inputId = "alphaonemean.n", label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01),
                    sliderInput(inputId = "alphaonemean.s", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
                  # Outputs
                  box(
                    numericInput(inputId = "MOEonemean.n", label = "+/- Precision", min = 0, max = 3, value = 1),
                    sliderInput(inputId = "MOEonemean.s", label = "", min = 0, max = 3, value = 1),
                    sliderInput(inputId = "CIonemean", label = "Confidence Interval", min = 0, max = 5, value = c(0.402, 0.598)))))),

              
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
                  numericInput(inputId = "n.per.group.tte.n", label = "Number of People per Group", value = ""),
                  numericInput(inputId = "TotalN.twomeans", label = "Total Number of People", value = "")))),
      
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
                
      ###########################################
      ############## Time to Event ##############
      ###########################################
      tabItem(tabName = "timetoevent",
              fluidRow(
                box(width = 12,
                    selectInput(inputId = "solvefortte", label = "Solve For", choices = c("Sample Size", "Power"), selected = "Sample Size", width = "100%"))),
              fluidRow(
                box(width = 4,
                    selectInput(inputId = "enrollmenttte", label = "Enrollment Information:", c("All at Once", "Over a Period", "Continuous Throughout")),
                    uiOutput("enrollmentdist"),
                    uiOutput("decayrate"),

                    numericInput(inputId = "studyduration.n", label = "Length of Study in Unit of Time:", min = 0, max = 10, value = 5, step = 1),
                    sliderInput(inputId = "studyduration.s", label = "", min = 0, max = 10, value = 5, step = 1),
                    uiOutput("enrollmenttimes")),
                    # numericInput(inputId = "enrollmentduration.n", label = "Length of Enrollment in Unit of Time:", min = 0, max = 10, value = 2.5, step = 0.5),
                    # sliderInput(inputId = "enrollmentduration.s", label = "", min = 0, max = 10, value = 2.5, step = 0.5)),
                box(width = 4,
                    numericInput(inputId = "alphatte.n", label = "Alpha:", min = 0, max = 1, value = 0.05, step = 0.01),
                    sliderInput(inputId = "alphatte.s", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
                    numericInput(inputId = "betatte.n", label = "Percent Power:", min = 0, max = 100, value = 80.0, step = 1),
                    sliderInput(inputId = "betatte.s", label = "", min = 0, max = 100, value = 80.0, step = 1),
                    numericInput(inputId = "lambdac.n", label = "Control Group's Event Rate per Unit of Time:", min = 0, max = 2, value = 1, step = 0.01),
                    sliderInput(inputId = "lambdac.s", label = "", min = 0, max = 2, value = 1, step = 0.01),
                    numericInput(inputId = "lambdae.n", label = "Experimental Group's Event Rate per Unit of Time:", min = 0, max = 10, value = 1.5, step = 0.01),
                    sliderInput(inputId = "lambdae.s", label = "", min = 0, max = 10, value = 1.5, step = 0.01),
                    numericInput(inputId = "censorc.n", label = "Control Group's Censoring Rate:", min = 0, max = 1, value = 0, step = 0.01),
                    sliderInput(inputId = "censorc.s", label = "", min = 0, max = 1, value = 0, step = 0.01),
                    numericInput(inputId = "censore.n", label = "Experimental Group's Censoring Rate:", min = 0, max = 1, value = 0, step = 0.01),
                    sliderInput(inputId = "censore.s", label = "", min = 0, max = 1, value = 0, step = 0.01)),
                box(width = 4,
                  numericInput(inputId = "npergroup.tte.n", label = "Number of People per Group", min = 0, max = 1000, value = 500, step = 1),
                  sliderInput(inputId = "npergroup.tte.s", label = "", min = 0, max = 1000, value = 500, step = 1),
                  numericInput(inputId = "TotalN.tte.n", label = "Total Number of People", value = 1000, step = 2),
                  sliderInput(inputId = "TotalN.tte.s", label = "", min = 0, max = 2000, value = 1000, step = 2))))
    ) # tabItems
  ) # dashboardBody
) # dashboardPage