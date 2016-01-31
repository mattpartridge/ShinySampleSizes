# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes


#################
### Libraries ###
#################
# library(shiny); library(shinydashboard)
shinyApp(
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
                  box(
                    selectInput(inputId = "SolveFortwoprops", label = "Solve For",
                                choices = c("N", "Significance", "Power")))),
                fluidRow(
                  box(
                    sliderInput(inputId = "alphatwoprops", label = "Significance:",
                                min = 0, max = 100, value = 95.0),
                    sliderInput(inputId = "betatwoprops", label = "Power:",
                                min = 0, max = 100, value = 80.0)),
                  box(
                    sliderInput(inputId = "p1", label = "Proportion Affected by Treatment 1:",
                                min = 0, max = 1, value = 0.5),
                    sliderInput(inputId = "p2", label = "Proportion Affected by Treatment 2:",
                                min = 0, max = 1, value = 0.4))),
                fluidRow(
                  box(
                    numericInput(inputId = "n.per.group.twoprops", label = "Number of People per Group", value = 385),
                    numericInput(inputId = "TotalN.twoprops", label = "Total Number of People", value = 770)))),
        
        #######################################
        ############## Two Means ##############
        #######################################
        tabItem(tabName = "twomeans",
                fluidRow(
                  box(
                    sliderInput(inputId = "alphatwomeans", label = "Significance:",
                                min = 0, max = 100, value = 95.0),
                    sliderInput(inputId = "betatwomeans", label = "Power:",
                                min = 0, max = 100, value = 80.0)),
                  box(
                    numericInput(inputId = "mu1", label = "Mu_1", value = ""),
                    numericInput(inputId = "sd", label = "Standard Deviation", value = ""),
                    numericInput(inputId = "mu2", label = "Mu_2", value = ""))),
                fluidRow(
                  box(
                    numericInput(inputId = "n.per.group.twomeans", label = "Number of People per Group", value = ""),
                    numericInput(inputId = "TotalN.twomeans", label = "Total Number of People", value = ""))))
        ) # tabItems
      ) # dashboardBody
    ), # dashboardPage
  
  server = function(input, output, clientData, session){
    observe({
      ############################################
      ############## One Proportion ##############
      ############################################
      # Margin of Error
      pi = input$pioneprop
      N = input$Noneprop
      confidence = input$confoneprop
      alpha = 1 - confidence
      Z_alpha2 = qnorm(1 - (alpha/2))
      se = sqrt((pi*(1 - pi))/N)
      moe = round(Z_alpha2*se, digits = 3)
      lo = round(pi - moe, digits = 3)
      hi = round(pi + moe, digits = 3)
      updateNumericInput(session, inputId = "MOEoneprop", value = moe)
      updateSliderInput(session, inputId = "CIoneprop", value = c(lo, hi))
    
      #############################################
      ############## Two Proportions ##############
      #############################################
      if(input$SolveFortwoprops == "N"){
        alpha = (100 - input$alphatwoprops)/100
        beta = input$betatwoprops/100
        p1 = input$p1
        p2 = input$p2
        Z.alpha.2 = qnorm(1 - alpha/2)
        Z.beta = qnorm(beta)
        n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
        N = 2*n
        updateNumericInput(session, inputId = "n.per.group.twoprops", value = n)
        updateNumericInput(session, inputId = "TotalN.twoprops", value = N)}
      if(input$SolveFortwoprops == "Significance"){
        SFalpha = round(2*(1 - pnorm( sqrt(input$n.per.group.twoprops*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(input$betatwoprops/100) )), digits = 3)
        SFSignificance = 100*(1 - SFalpha)
        updateSliderInput(session, inputId = "alphatwoprops", value = SFSignificance)}
      if(input$SolveFortwoprops == "Power"){
        SFbeta = round((pnorm( sqrt(input$n.per.group.twoprops*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(1 - ((100 - input$alphatwoprops)/100)/2) )), digits = 3)
        SFPower = 100*(SFbeta)
        updateSliderInput(session, inputId = "betatwoprops", value = SFPower)}
      
      #######################################
      ############## Two Means ##############
      #######################################
      alpha = (100 - input$alphatwomeans)/100
      beta = input$betatwomeans/100
      mu1 = input$mu1
      mu2 = input$mu2
      sd = input$sd
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * 2*(sd^2))/((mu1 - mu2)^2))
      N = 2*n
      updateNumericInput(session, inputId = "n.per.group.twomeans", value = n)
      updateNumericInput(session, inputId = "TotalN.twomeans", value = N)
    }) # Observe
  } # Server Function
) # Shiny App


