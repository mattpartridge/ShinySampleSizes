# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

###############################################
############## Things To Look At ##############
###############################################
# Slider for N or add by increments
# Adjusting ranges for slider as you drag --> Kind of implemented (Could look better)
# Max n for slider would be n that gives 99% power --> Still need to implement
# Unequal groups


library(shiny); library(shinydashboard)
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Sample Size Calculations"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("One Sample Proportion", tabName = "oneproportion"),
        menuItem("Two Sample Proportions", tabName = "twoproportions"),
        menuItem("Two Means", tabName = "twomeans"),
        menuItem("reactiveValues() Test", tabName = "Test"))),
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
                    numericInput(inputId = "alphatwoprops", label = "Alpha Level:",
                                 min = 0, max = 1, value = 0.05, step = 0.01),
                    sliderInput(inputId = "betatwoprops", label = "Power:",
                                min = 0, max = 100, value = 80.0),
                    uiOutput(outputId = "PowerTwoProps")),
                  box(
                    sliderInput(inputId = "p1", label = "Proportion Affected by Treatment 1:",
                                min = 0, max = 1, value = 0.5),
                    sliderInput(inputId = "p2", label = "Proportion Affected by Treatment 2:",
                                min = 0, max = 1, value = 0.4))),
                fluidRow(
                  box(
                    sliderInput(inputId = "n.per.group.twoprops", label = "Number of People per Group", min = 0, max = 10000, value = 385),
                    sliderInput(inputId = "TotalN.twoprops", label = "Total Number of People", min = 0, max = 10000, value = 770, step = 2)))),
        
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
        tabItem(tabName = "Test",
                fluidRow(
                  box(
                    sliderInput(inputId = "Test1", label = "Test1", min = 0, max = 50, value = 50),
                    sliderInput(inputId = "Test2", label = "Test2", min = 0, max = 50, value = 25),
                    textOutput("LASTUPDATED"))))
      ) # tabItems
    ) # dashboardBody
  ), # dashboardPage
  
  server = function(input, output, clientData, session){
    
    ############################################
    ############## One Proportion ##############
    ############################################
    observe({
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
    })
    
    #######################################
    ############## Two Means ##############
    #######################################
    observe({
      alpha = input$alphatwomeans
      beta = input$betatwomeans/100
      deltamu = input$deltamu
      sd = input$sd
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * 2*(sd^2))/((deltamu)^2))
      N = 2*n
      updateNumericInput(session, inputId = "n.per.group.twomeans", value = n)
      updateNumericInput(session, inputId = "TotalN.twomeans", value = N)
    })
    
    #############################################
    ############## Two Proportions ##############
    #############################################
    Last_Widget_2prop = reactiveValues( last = NULL )
    observe({ input$alphatwoprops; Last_Widget_2prop$last = "alpha"})
    observe({ input$betatwoprops; Last_Widget_2prop$last = "beta" })
    observe({ input$p1; Last_Widget_2prop$last = "p1" })
    observe({ input$p2; Last_Widget_2prop$last = "p2" })
    observe({ input$n.per.group.twoprops; Last_Widget_2prop$last = "n" })
    observe({ input$TotalN.twoprops; Last_Widget_2prop$last = "N" })
    observe({
      if(Last_Widget_2prop$last %in% c("alpha", "p1", "p2")){
        alpha = input$alphatwoprops
        beta = input$betatwoprops/100
        p1 = input$p1
        p2 = input$p2
        Z.alpha.2 = qnorm(1 - alpha/2)
        Z.beta = qnorm(beta)
        n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
        N = 2*n
        updateSliderInput(session, inputId = "n.per.group.twoprops", min = 0, max = n + n/2, value = n)
        updateSliderInput(session, inputId = "TotalN.twoprops", min = 0, max = N + n, value = N)}
      if(Last_Widget_2prop$last == "beta"){
        alpha = input$alphatwoprops
        beta = input$betatwoprops/100
        p1 = input$p1
        p2 = input$p2
        Z.alpha.2 = qnorm(1 - alpha/2)
        Z.beta = qnorm(beta)
        n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
        N = 2*n
        updateSliderInput(session, inputId = "n.per.group.twoprops", min = 0, max = n + n/2, value = n)
        updateSliderInput(session, inputId = "TotalN.twoprops", min = 0, max = N + n, value = N)}
      if(Last_Widget_2prop$last == "n"){
        SFbeta = round(pnorm( sqrt(input$n.per.group.twoprops*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(1-input$alphatwoprops/2) ), digits = 1)
        SFPower = 100*(SFbeta)
        updateSliderInput(session, inputId = "betatwoprops", value = SFPower)
        updateSliderInput(session, inputId = "TotalN.twoprops", min = 0, max = 3*input$n.per.group.twoprops, value = 2*input$n.per.group.twoprops)}
      if(Last_Widget_2prop$last == "N"){
        SFbeta = round(pnorm( sqrt((0.5*input$TotalN.twoprops)*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(1-input$alphatwoprops/2) ), digits = 1)
        SFPower = 100*(SFbeta)
        updateSliderInput(session, inputId = "betatwoprops", value = SFPower)
        updateSliderInput(session, inputId = "n.per.group.twoprops", min = 0, max = input$TotalN.twoprops, value = 0.5*input$TotalN.twoprops)}
    })
    
    ##################################
    ############## Test ##############
    ##################################
    last_updated_widget <- reactiveValues( last = 0 )
    observe({ input$Test1; last_updated_widget$last = 1 })
    observe({ input$Test2; last_updated_widget$last = 2 })
    observe({
      if(last_updated_widget$last %in% c(0, 1)){ updateSliderInput(session, inputId = "Test2", value = .5*input$Test1) }
      if(last_updated_widget$last %in% c(0, 2)){ updateSliderInput(session, inputId = "Test1", value = 2*input$Test2) }
    })
    output$LASTUPDATED = renderText(last_updated_widget$last)
    
  } # Server Function
) # Shiny App