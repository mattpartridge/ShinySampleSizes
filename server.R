# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

# Total number of groups for two props when not on "N"
# Slider for N or add by increments
# Adjusting ranges for slider as you drag
# Unequal groups
# reactive UI renderUI****
# reactiveValues
# --> Still need to connect power and n

library(shiny); library(shinydashboard)
server = function(input, output, clientData, session){
  output$TEST2 = renderUI({
    x = input$Test1
    y = 2*x
    wellPanel(
      sliderInput(inputId = "Test4", label = "Test4", min = 0, max = y + 50, value = y),
      sliderInput(inputId = "TEST3", label = "TEST3", min = 0, max = input$Test1 + 50, value = input$Test1))
    })
  
  # Two Proportions
  beta = reactive({
    round((pnorm( sqrt(input$n.per.group.twoprops*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(1 - (input$alphatwoprops)/2))), digits = 3) })
  output$PowerTwoProps = renderUI({
    sliderInput(inputId = "Power", label = "PowerTwoProps", min = 0, max = 1, value = beta()) })
  n = reactive({
    alpha = input$alphatwoprops
    beta = beta()
    p1 = input$p1
    p2 = input$p2
    Z.alpha.2 = qnorm(1 - alpha/2)
    Z.beta = qnorm(beta)
    n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))})
  output$NTwoProps = renderUI({
    wellPanel(
      sliderInput(inputId = "npergroup", label = "Number of People per Group", min = 0, max = n() + (n()/2), value = n()),
      sliderInput(inputId = "TotalN", label = "Total Number of People", min = 0, max = 2*n() + n(), value = 2*n()))
  })
  
  observe({
    ############################################
    ############## One Proportion ##############
    ############################################
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
      alpha = input$alphatwoprops
      beta = input$betatwoprops/100
      p1 = input$p1
      p2 = input$p2
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
      N = 2*n
      updateSliderInput(session, inputId = "n.per.group.twoprops", value = n)
      updateSliderInput(session, inputId = "TotalN.twoprops", value = N)}
    if(input$SolveFortwoprops == "Power"){
      SFbeta = round(pnorm( sqrt(input$n.per.group.twoprops*((input$p1 - input$p2)^2)/(input$p1*(1-input$p1) + input$p2*(1-input$p2))) - qnorm(1-input$alphatwoprops/2) ), digits = 3)
      SFPower = 100*(SFbeta)
      updateSliderInput(session, inputId = "betatwoprops", value = SFPower)}
    #         updateSliderInput(session, inputId = "n.per.group.twoprops", value = 0.5*input$TotalN.twoprops)
    #         updateSliderInput(session, inputId = "TotalN.twoprops", value = 2*input$n.per.group.twoprops)}
    #       
    #######################################
    ############## Two Means ##############
    #######################################
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
    
    ##################################
    ############## Test ##############
    ##################################
    
  }) # Observe
} # Server Function