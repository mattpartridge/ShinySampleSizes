# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

####################
### Things to Do ###
####################
# Update textOutput()s to numericInput()s and make the updatable


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
                    textOutput("MOEoneprop"),
                    textOutput("CIoneprop"))
                  # box(
                    # sliderInput(inputId = "moeoneprop", label = "Margin of Error", min = 0, max = 80, value = 0.5)
                  # )
                )),
        
        #############################################
        ############## Two Proportions ##############
        #############################################
        tabItem(tabName = "twoproportions",
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
                    textOutput("n.per.group.props"),
                    textOutput("TotalN.props"),
                    textOutput("TEST"),
                    selectInput(inputId = "SolveFortwoprops", label = "Solve For",
                                choices = c("Significance", "Power"))))),
        
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
                    textOutput("n.per.group.means"),
                    textOutput("TotalN.means")))),
        
        ##################################
        ############## Test ##############
        ##################################
        tabItem(tabName = "TEST",
                fluidRow(
                  box(
                    selectInput(inputId = "SolveFor", label = "Solver For: ", choices = c("Control", "Response")),
                    sliderInput(inputId = "Control", label = "Control: ",
                                min = 0, max = 100, value = 25),
                    sliderInput(inputId = "Response", label = "Response: ",
                                min = 0, max = 100, value = 50),
                    numericInput(inputId = "controlresponse", label = "Addition", value = 75))))
        
        ) # tabItems
      ) # dashboardBody
    ), # dashboardPage
  
  server = function(input, output, clientData, session){
    
    ############################################
    ############## One Proportion ##############
    ############################################
    output$MOEoneprop <- renderText({
      pi = input$pioneprop
      N = input$Noneprop
      confidence = input$confoneprop
      alpha = 1 - confidence
      Z_alpha2 = qnorm(1 - (alpha/2))
      se = sqrt((pi*(1 - pi))/N)
      moe = round(Z_alpha2*se, digits = 3)
      paste("The Margin of Error is: ", moe)})
    output$CIoneprop = renderText({
      pi = input$pioneprop
      N = input$Noneprop
      confidence = input$confoneprop
      alpha = 1 - confidence
      Z_alpha2 = qnorm(1 - (alpha/2))
      se = sqrt((pi*(1 - pi))/N)
      moe = Z_alpha2*se
      lo = round(pi - moe, digits = 3)
      hi = round(pi + moe, digits = 3)
      paste("The confidence interval is: ", lo, ", ", hi)})
    
    #############################################
    ############## Two Proportions ##############
    #############################################
    output$TEST = renderText({
      ifelse(input$solvefor == "alpha", paste("alpha"), ifelse(input$solvefor == "Power", paste("Power"), paste("Sample Size")))})
    output$n.per.group.props <- renderText({
      alpha = (100 - input$alphatwoprops)/100
      beta = input$betatwoprops/100
      p1 = input$p1
      p2 = input$p2
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
      paste("The number of people needed per group is: ", n)})
    output$TotalN.props <- renderText({
      alpha = (100 - input$alphatwoprops)/100
      beta = input$betatwoprops/100
      p1 = input$p1
      p2 = input$p2
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
      N = 2*n
      paste("The total number of people needed is: ", N)})
    
    #######################################
    ############## Two Means ##############
    #######################################
    output$n.per.group.means <- renderText({
      alpha = (100 - input$alpha)/100
      beta = input$beta/100
      mu1 = input$mu1
      mu2 = input$mu2
      sd = input$sd
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * 2*(sd^2))/((mu1 - mu2)^2))
      paste("The number of people needed per group is: ", n)})
    output$TotalN.means <- renderText({
      alpha = (100 - input$alpha)/100
      beta = input$beta/100
      mu1 = input$mu1
      mu2 = input$mu2
      sd = input$sd
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      N = 2*ceiling(((Z.alpha.2 + Z.beta)^2 * 2*(sd^2))/((mu1 - mu2)^2))
      paste("The total number of people needed is: ", N)})
    
    ##################################
    ############## Test ##############
    ##################################
    
    output$controlresponse = renderText({
      paste(input$Control + input$Response)
    })
    
    observe({
      if(input$SolveFor == "Control"){
        SFResponse = 0.5*input$Response
        updateSliderInput(session, inputId = "Control", value = SFResponse)}
      else{ updateSliderInput(session, inputId = "Response", value = 2*(input$Control)) }
      updateNumericInput(session, inputId = "controlresponse", value = (input$Control + input$Response))
      
#       if(input$SolveFortwoprops == "Significance"){
#                 SFalpha = round(2*(1 - pnorm( sqrt(output$n.per.group.props*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(input$betatwoprops/100) )), digits = 3)
#                 SFSignificance = 100*(1 - SFalpha)
#                 updateSliderInput(session, inputId = "alphatwoprops", value = SFSignificance)
#         
#       }
#       else{}
      })
    
  }
)

