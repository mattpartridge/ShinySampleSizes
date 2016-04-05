# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

###############################################
############## Things To Look At ##############
###############################################
# Number of "#" signifiy priority:
##### Add solve for back --> Everything is connected to the sliders
##### Add numeric boxes for everything --> Need to connect to sliders
##### Add a final sentence compiling everything into one
##### Max n for slider would be n that gives 99% power --> Still need to implement
### Look into packages that do it for your; power.t.test/package(pwr)/package(pwerSurvEpi)
# Committee --> (Nicole Basta?, Sonja Brady, people who actually design studies)?/Jim Neaton(Biostat)/Julian Wolfson(Biostat)
# PASS
# One sample means
########## Tim to event (Potentially ask Jim what he uses)
########## Read up on everything first
########## nSurvival/nSurv/gsDesign (group sequential)
########## r time to event
########## GOAL: Have some thoughts about what the interface would look like (Even on paper)
# Representative CI
# Arrows()
# Exact CI vs Wald CI for small p
# Maybe something "Fancier"? --> Time to Event
# Look for plots that could help understanding
# Unequal groups


library(shiny); library(shinydashboard); library(pwr)
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
#   Last_Widget_2prop = reactiveValues( last = NULL )
#   observe({ input$alphatwoprops; Last_Widget_2prop$last = "alpha"})
#   observe({ input$betatwoprops; Last_Widget_2prop$last = "beta" })
#   observe({ input$p1; Last_Widget_2prop$last = "p1" })
#   observe({ input$p2; Last_Widget_2prop$last = "p2" })
#   observe({ input$n.per.group.twoprops; Last_Widget_2prop$last = "n" })
#   observe({ input$TotalN.twoprops; Last_Widget_2prop$last = "N" })
  observe({
    if(input$solvefortwoprops == "Sample Size"){
      alpha = input$alphatwoprops.s
      beta = input$betatwoprops.s/100
      p1 = input$p1.s
      p2 = input$p2.s
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
      N = 2*n
      updateNumericInput(session, inputId = "n.per.group.twoprops.n", min = 0, max = n + n/2, value = n)
      updateSliderInput(session, inputId = "n.per.group.twoprops.s", min = 0, max = n + n/2, value = n)
      updateNumericInput(session, inputId = "TotalN.twoprops.n", min = 0, max = N + n, value = N)
      updateSliderInput(session, inputId = "TotalN.twoprops.s", min = 0, max = N + n, value = N)
    }
    if(input$solvefortwoprops == "Power"){
      n = input$n.per.group.twoprops.s
      p1 = input$p1.s
      p2 = input$p2.s
      alpha = input$alphatwoprops.s
      SFbeta = round(pnorm( sqrt(n*((p1 - p2)^2)/(p1*(1-p1) + p2*(1-p2))) - qnorm(1-alpha/2) ), digits = 1)
      SFPower = 100*(SFbeta)
      updateNumericInput(session, inputId = "betatwoprops.n", value = SFPower)
      updateSliderInput(session, inputId = "betatwoprops.s", value = SFPower)
#       updateNumericInput(session, inputId = "TotalN.twoprops.n", min = 0, max = 3*input$n.per.group.twoprops, value = 2*input$n.per.group.twoprops)
#       updateSliderInput(session, inputId = "TotalN.twoprops.s", min = 0, max = 3*input$n.per.group.twoprops, value = 2*input$n.per.group.twoprops)
      }
  })
} # Server Function