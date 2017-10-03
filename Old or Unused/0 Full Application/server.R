# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

###############################################
############## Things To Look At ##############
###############################################
## Two Sample Props
# Add a final sentence compiling everything into one
# Max n for slider would be n that gives 99% power --> Still need to implement
# Committee: Sonja Brady(Epidemiology), Jim Neaton(Biostat), and Julian Wolfson(Biostat)
# PASS
# One sample means
# Representative CI
# Arrows()
# Exact CI vs Wald CI for small p
# Look for plots that could help understanding
# Unequal groups


library(shiny); library(shinydashboard); library(pwr); library(gsDesign)
server = function(input, output, clientData, session){
  
  ######################################
  ############## One Mean ##############
  ######################################
  {
    ##### Numeric or Slider
    numericorslider.onemean = reactiveValues( which = "" )
    observe({
      input$muonemean.n; input$sdonemean.n; input$nullmuonemean.n; input$alphaonemean.n; input$poweronemean.n; input$Nonemean.n; input$MOEonemean.n
      numericorslider.onemean$which = "Numeric"
    })
    observe({
      input$muonemean.s; input$sdonemean.s; input$nullmuonemean.s; input$alphaonemean.s; input$poweronemean.s; input$Nonemean.s; input$MOEonemean.s
      numericorslider.onemean$which = "Slider"
    })
    
    ##### Calculations
    observe({
      ##### Sample Size
      if(input$solveforonemean == "Sample Size"){
        #### Prep
        ### Numeric
        if(numericorslider.onemean$which == "Numeric"){
          # Update Slider Inputs
          updateSliderInput(muonemean.s, value = input$muonemean.n)
          updateSliderInput(sdonemean.s, value = input$sdonemean.n)
          updateSliderInput(nullmuonemean.s, value = input$nullmuonemean.n)
          updateSliderInput(alphaonemean.s, value = input$alphaonemean.n)
          updateSliderInput(poweronemean.s, value = input$poweronemean.n)
          # Sample Size Calculations
          mu = input$muonemean.n
          sd = input$sdonemean.n
          nullmu = input$nullmuonemean.n
          alpha = input$alphaonemean.n
          beta = 1 - (input$poweronemean.n/100)
          Z.alpha.2 = qnorm(1 - alpha/2)
          Z.beta = qnorm(1 - beta)
          n = ceiling((sd*((Z.alpha.2 + Z.beta)/(mu - nullmu)))^2)
          # Output
          updateNumericInput(session, inputId = "Nonemean.n", value = n)
          updateSliderInput(session, inputId = "Nonemean.s", value = n)
        }
        ### Slider
        else{if(numericorslider.onemean$which == "Slider"){
          # Update Numeric Inputs
          updateSliderInput(muonemean.n, value = input$muonemean.s)
          updateSliderInput(sdonemean.n, value = input$sdonemean.s)
          updateSliderInput(nullmuonemean.n, value = input$nullmuonemean.s)
          updateSliderInput(alphaonemean.n, value = input$alphaonemean.s)
          updateSliderInput(poweronemean.n, value = input$poweronemean.s)
          # Sample Size Calculations
          mu = input$muonemean.s
          sd = input$sdonemean.s
          nullmu = input$nullmuonemean.s
          alpha = input$alphaonemean.s
          beta = 1 - (input$poweronemean.s/100)
          Z.alpha.2 = qnorm(1 - alpha/2)
          Z.beta = qnorm(1 - beta)
          n = ceiling((sd*((Z.alpha.2 + Z.beta)/(mu - nullmu)))^2)
          # Output
          updateNumericInput(session, inputId = "Nonemean.n", value = n)
          updateSliderInput(session, inputId = "Nonemean.s", value = n)
          }}
      }
      
      ##### Power
      if(input$solveforonemean == "Power"){
        #### Prep
        ### Numeric
        if(numericorslider.onemean$which == "Numeric"){
          # Update Slider Inputs
          updateSliderInput(muonemean.s, value = input$muonemean.n)
          updateSliderInput(sdonemean.s, value = input$sdonemean.n)
          updateSliderInput(Nonemean.s, value = input$Nonemean.n)
          updateSliderInput(nullmuonemean.s, value = input$nullmuonemean.n)
          updateSliderInput(alphaonemean.s, value = input$alphaonemean.n)
          # Power Calculations
          mu = input$muonemean.n
          sd = input$sdonemean.n
          n = input$Nonemean.n
          nullmu = input$nullmuonemean.n
          alpha = input$alphaonemean.n
          Z.alpha.2 = qnorm(1 - alpha/2)
          Z = (mu - nullmu)/(sd/sqrt(n))
          power = round(100*pnorm(Z - Z.alpha.2) + pnorm(-Z - Z.alpha.2), digits = 2)
          # Output
          updateNumericInput(session, inputId = "poweronemean.n", value = power)
          updateSliderInput(session, inputId = "poweronemean.s", value = power)
        }
        ### Slider
        else{if(numericorslider.onemean$which == "Slider"){
          # Update Numeric Inputs
          updateSliderInput(muonemean.n, value = input$muonemean.s)
          updateSliderInput(sdonemean.n, value = input$sdonemean.s)
          updateSliderInput(Nonemean.n, value = input$Nonemean.s)
          updateSliderInput(nullmuonemean.n, value = input$nullmuonemean.s)
          updateSliderInput(alphaonemean.n, value = input$alphaonemean.s)
          # Power Calculations
          mu = input$muonemean.s
          sd = input$sdonemean.s
          n = input$Nonemean.s
          nullmu = input$nullmuonemean.s
          alpha = input$alphaonemean.s
          Z.alpha.2 = qnorm(1 - alpha/2)
          Z = (mu - nullmu)/(sd/sqrt(n))
          power = round(100*pnorm(Z - Z.alpha.2) + pnorm(-Z - Z.alpha.2), digits = 2)
          # Output
          updateNumericInput(session, inputId = "poweronemean.n", value = power)
          updateSliderInput(session, inputId = "poweronemean.s", value = power)
        }}
      }
      
      ##### Precision
      if(input$solveforonemean == "Precision"){
        #### Prep
        ### Numeric
        if(numericorslider.onemean$which == "Numeric"){
          # Update Slider Inputs
          updateSliderInput(muonemean.s, value = input$muonemean.n)
          updateSliderInput(sdonemean.s, value = input$sdonemean.n)
          updateSliderInput(Nonemean.s, value = input$Nonemean.n)
          updateSliderInput(alphaonemean.s, value = input$alphaonemean.n)
          # Precision Calculations
          mu = input$muonemean.n
          sd = input$sdonemean.n
          n = input$Nonemean.n
          alpha = input$alphaonemean.n
          Z.alpha.2 = qnorm(1 - alpha/2)
          se = sd/sqrt(n)
          moe = round(Z.alpha.2*se, digits = 2)
          lo = round(mu - moe, digits = 3)
          hi = round(mu + moe, digits = 3)
          # Outputs
          updateNumericInput(session, inputId = "MOEonemean.n", min = moe - 1, max = moe + 1, value = moe)
          updateSliderInput(session, inputId = "MOEonemean.s", min = moe - 1, max = moe + 1, value = moe)
          updateSliderInput(session, inputId = "CIonemean", min = lo - 1, max = hi + 1, value = c(lo, hi))
        }
        ### Slider
        else{if(numericorslider.onemean$which == "Numeric"){
          # Update Numeric Inputs
          updateSliderInput(muonemean.n, value = input$muonemean.s)
          updateSliderInput(sdonemean.n, value = input$sdonemean.s)
          updateSliderInput(Nonemean.n, value = input$Nonemean.s)
          updateSliderInput(alphaonemean.n, value = input$alphaonemean.s)
          # Precision Calculations
          mu = input$muonemean.s
          sd = input$sdonemean.s
          n = input$Nonemean.s
          alpha = input$alphaonemean.s
          Z.alpha.2 = qnorm(1 - alpha/2)
          se = sd/sqrt(n)
          moe = round(Z.alpha.2*se, digits = 2)
          lo = round(mu - moe, digits = 3)
          hi = round(mu + moe, digits = 3)
          # Outputs
          updateNumericInput(session, inputId = "MOEonemean.n", min = moe - 1, max = moe + 1, value = moe)
          updateSliderInput(session, inputId = "MOEonemean.s", min = moe - 1, max = moe + 1, value = moe)
          updateSliderInput(session, inputId = "CIonemean", min = lo - 1, max = hi + 1, value = c(lo, hi))
        }}
      }
    })
  }
  
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
  
  ###########################################
  ############## Time to Event ##############
  ###########################################
  output$enrollmentdist = renderUI({
    if(input$enrollmenttte %in% c("Over a Period", "Continuous Throughout")){
      selectInput(inputId = "enrollmentdist", label = "Distribution of Enrollment", choices = c("Uniform", "Exponential Decay"))
    }
    else{ NULL }
  })
  output$decayrate = renderUI({
    if(is.null(input$enrollmentdist)){ NULL }
    else{
      if(input$enrollmentdist == "Exponential Decay"){
        list(numericInput(inputId = "gammatte.n", label = "Exponential Decay Rate", min = 0, max = 1, value = 0.5, step = 0.01),
             sliderInput(inputId = "gammatte.s", label = "", min = 0, max = 1, value = 0.5, step = 0.01))
      }
      else{ NULL }
    }
  })
  output$enrollmenttimes = renderUI({
    if(input$enrollmenttte == "All at Once"){
#       list(numericInput(inputId = "studyduration.n", label = "Length of Study in Unit of Time:", min = 0, max = 10, value = 5, step = 1),
#            sliderInput(inputId = "studyduration.s", label = "", min = 0, max = 10, value = 5, step = 1))
    }
    else{
      list(
#         numericInput(inputId = "studyduration.n", label = "Length of Study in Unit of Time:", min = 0, max = 10, value = 5, step = 1),
#         sliderInput(inputId = "studyduration.s", label = "", min = 0, max = 10, value = 5, step = 1),
        numericInput(inputId = "enrollmentduration.n", label = "Length of Enrollment in Unit of Time:", min = 0, max = 10, value = 2.5, step = 0.5),
        sliderInput(inputId = "enrollmentduration.s", label = "", min = 0, max = 10, value = 2.5, step = 0.5))
    }
  })
  
  # To see if a slider or a numeric box was last updated
  numericorslider = reactiveValues( which = "" )
  observe({
    input$studyduration.n; input$enrollmentduration.n; input$alphatte.n; input$betatte.n; input$lambdac.n; input$lambdae.n; input$censorc.n; input$censore.n; input$npergroup.tte.n; input$TotalN.tte.n; input$gamma.tte.n
    numericorslider$which = "Numeric"
  })
  observe({
    input$studyduration.s; input$enrollmentduration.s; input$alphatte.s; input$betatte.s; input$lambdac.s; input$lambdae.s; input$censorc.s; input$censore.s; input$npergroup.tte.s; input$TotalN.tte.s; input$gamma.tte.s
    numericorslider$which = "Slider"
  })
  
  # All at once
  observe({
    if(input$enrollmenttte == "All at Once"){
      updateNumericInput(session, inputId = "enrollmentduration.n", min = 0, max = 0, value = 0, step = 0)
      updateSliderInput(session, inputId = "enrollmentduration.s", min = 0, max = 0, value = 0, step = 0)
    }
    else{
      updateNumericInput(session, inputId = "enrollmentduration.n", min = 0, max = 10, value = 2.5, step = 0.5)
      updateSliderInput(session, inputId = "enrollmentduration.s", min = 0, max = 10, value = 2.5, step = 0.5)
    }
  })
  
  # Calculations
  observe({
    if(input$solvefortte == "Sample Size"){
      # If a numericInput was updated
      if(numericorslider$which == "Numeric"){
        # Updating the input types that didnt get updated
        updateSliderInput(session, inputId = "studyduration.s", value = input$studyduration.n)
        updateSliderInput(session, inputId = "enrollmentduration.s", value = input$enrollmentduration.n)
        updateSliderInput(session, inputId = "gammatte.s", value = input$gamma.tte.n)
        updateSliderInput(session, inputId = "alphatte.s", value = input$alphatte.n)
        updateSliderInput(session, inputId = "betatte.s", value = input$betatte.n)
        updateSliderInput(session, inputId = "lambdac.s", value = input$lambdac.n)
        updateSliderInput(session, inputId = "lambdae.s", value = input$lambdae.n)
        updateSliderInput(session, inputId = "censorc.s", value = input$censorc.n)
        updateSliderInput(session, inputId = "censore.s", value = input$censore.n)
        # Calculating the sample size
        lambdac = input$lambdac.n; lambda1 = lambdac*(lambdac/(lambdac + input$censorc.n))
        lambdec = input$lambdae.n; lambda2 = lambdec*(lambdec/(lambdec + input$censore.n))
        Ts = input$studyduration.n
        # Tr = 0
        # Tr = input$enrollmentduration.n
        Tr = ifelse(is.null(input$enrollmentduration.n), 0, input$enrollmentduration.n)
        eta = 0; ratio = 1
        alpha = input$alphatte.n; beta = (100 - input$betatte.n)/100
        sided = 2; approx = F; type = "rr" # Im not sure
        entry = ifelse(is.null(input$enrollmentdist), "unif", ifelse(input$enrollmentdist == "Uniform", "unif", "expo"))
        # entry = "unif"
        gamma = ifelse(entry == "unif", NA, 0.5)
        # gamma = ifelse(is.null(input$gammatte.n), NA, input$gammette.n)
        # N = Tr
        N = ceiling(nSurvival(lambda1, lambda2, Ts, Tr, eta, ratio, alpha, beta, sided, approx, type, entry, gamma)$n)
        n = N/2
        # Updating the sample size inputs
        updateNumericInput(session, inputId = "npergroup.tte.n", min = n - .2*n, value = n, max = n + .2*n)
        updateSliderInput(session, inputId = "npergroup.tte.s", min = n - .2*n, value = n, max = n + .2*n)
        updateNumericInput(session, inputId = "TotalN.tte.n", min = N - .2*N, value = N, max = N + .2*N)
        updateSliderInput(session, inputId = "TotalN.tte.s", min = N - .2*N, value = N, max = N + .2*N)
      }
      # If a sliderInput was updated
      if(numericorslider$which == "Slider"){
        # Updating the input types that didnt get updated
        updateNumericInput(session, inputId = "studyduration.n", value = input$studyduration.s)
        updateNumericInput(session, inputId = "enrollmentduration.n", value = input$enrollmentduration.s)
        updateNumericInput(session, inputId = "gammatte.n", value = input$gamma.tte.s)
        updateNumericInput(session, inputId = "alphatte.n", value = input$alphatte.s)
        updateNumericInput(session, inputId = "betatte.n", value = input$betatte.s)
        updateNumericInput(session, inputId = "lambdac.n", value = input$lambdac.s)
        updateNumericInput(session, inputId = "lambdae.n", value = input$lambdae.s)
        updateNumericInput(session, inputId = "censorc.n", value = input$censorc.s)
        updateNumericInput(session, inputId = "censore.n", value = input$censore.s)
        # Calculating the sample size
        lambdac = input$lambdac.s; lambda1 = lambdac*(lambdac/(lambdac + input$censorc.s))
        lambdec = input$lambdae.s; lambda2 = lambdec*(lambdec/(lambdec + input$censore.s))
        Ts = input$studyduration.s
        # Tr = 0
        # Tr = input$enrollmentduration.s
        Tr = ifelse(is.null(input$enrollmentduration.n), 0, input$enrollmentduration.s)
        eta = 0; ratio = 1
        alpha = input$alphatte.s; beta = (100 - input$betatte.s)/100
        sided = 2; approx = F; type = "rr" # Im not sure
        entry = ifelse(is.null(input$enrollmentdist), "unif", ifelse(input$enrollmentdist == "Uniform", "unif", "expo"))
        # entry = "unif"
        gamma = ifelse(entry == "unif", NA, 0.5)
        # gamma = ifelse(is.null(input$gammatte.s), NA, input$gammette.s)
        # N = 
        N = ceiling(nSurvival(lambda1, lambda2, Ts, Tr, eta, ratio, alpha, beta, sided, approx, type, entry, gamma)$n)
        n = N/2
        # Updating the sample size inputs
        updateNumericInput(session, inputId = "npergroup.tte.n", min = n - .2*n, value = n, max = n + .2*n)
        updateSliderInput(session, inputId = "npergroup.tte.s", min = n - .2*n, value = n, max = n + .2*n)
        updateNumericInput(session, inputId = "TotalN.tte.n", min = N - .2*N, value = N, max = N + .2*N)
        updateSliderInput(session, inputId = "TotalN.tte.s", min = N - .2*N, value = N, max = N + .2*N)
      }
    }
    #     if(input$solvefortte == "Power"){
    #       # If a numericInput was updated
    #       if(numericorslider$which == "Numeric"){
    #         # Updating the input types that didnt get updated
    #         updateSliderInput(session, inputId = "studyduration.s", value = input$studyduration.n)
    #         updateSliderInput(session, inputId = "enrollmentduration.s", value = input$enrollmentduration.n)
    #         updateSliderInput(session, inputId = "alphatte.s", value = input$alphatte.n)
    #         updateSliderInput(session, inputId = "lambdac.s", value = input$lambdac.n)
    #         updateSliderInput(session, inputId = "lambdae.s", value = input$lambdae.n)
    #         updateSliderInput(session, inputId = "npergrouptte.s", value = input$npergroup.tte.n)
    #         updateSliderInput(session, inputId = "TotalN.tte.s", value = input$TotalN.tte.n)
    #         # Calculating the sample size
    #         lambda1 = input$lambdac.n; lambda2 = input$lambdae.n
    #         Ts = input$studyduration.n; Tr = input$enrollmentduration.n
    #         eta = 0; ratio = 1
    #         alpha = input$alphatte.n; beta = (100 - input$betatte.n)/100
    #         sided = 2; approx = F; type = "rr" # Im not sure
    #         entry = ifelse(input$enrollmenttte == "Exponential Decay", "expo", "unif") # Not sure about this
    #         rate = 0.5; gamma = ifelse(input$enrollmenttte == "Exponential Decay", rate, NA)
    #         N = ceiling(nSurvival(lambda1, lambda2, Ts, Tr, eta, ratio, alpha, beta, sided, approx, type, entry, gamma)$n)
    #         n = N/2
    #         # Updating the sample size inputs
    #         updateNumericInput(session, inputId = "npergroup.tte.n", min = n - .2*n, value = n, max = n + .2*n)
    #         updateSliderInput(session, inputId = "npergroup.tte.s", min = n - .2*n, value = n, max = n + .2*n)
    #         updateNumericInput(session, inputId = "TotalN.tte.n", min = N - .2*N, value = N, max = N + .2*N)
    #         updateSliderInput(session, inputId = "TotalN.tte.s", min = N - .2*N, value = N, max = N + .2*N)
    #       }
    #       # If a sliderInput was updated
    #       if(numericorslider$which == "Slider"){
    #         # Updating the input types that didnt get updated
    #         updateSliderInput(session, inputId = "studyduration.n", value = input$studyduration.s)
    #         updateSliderInput(session, inputId = "enrollmentduration.n", value = input$enrollmentduration.s)
    #         updateSliderInput(session, inputId = "alphatte.n", value = input$alphatte.s)
    #         updateSliderInput(session, inputId = "lambdac.n", value = input$lambdac.s)
    #         updateSliderInput(session, inputId = "lambdae.n", value = input$lambdae.s)
    #         updateSliderInput(session, inputId = "npergrouptte.n", value = input$npergroup.tte.s)
    #         updateSliderInput(session, inputId = "TotalN.tte.n", value = input$TotalN.tte.s)
    #         # Calculating the sample size
    #         lambda1 = input$lambdac.s; lambda2 = input$lambdae.s
    #         Ts = input$studyduration.s; Tr = input$enrollmentduration.s
    #         eta = 0; ratio = 1
    #         alpha = input$alphatte.s; beta = (100 - input$betatte.s)/100
    #         sided = 2; approx = F; type = "rr" # Im not sure
    #         entry = ifelse(input$enrollmenttte == "Exponential Decay", "expo", "unif") # Not sure about this
    #         rate = 0.5; gamma = ifelse(input$enrollmenttte == "Exponential Decay", rate, NA)
    #         N = ceiling(nSurvival(lambda1, lambda2, Ts, Tr, eta, ratio, alpha, beta, sided, approx, type, entry, gamma)$n)
    #         n = N/2
    #         # Updating the sample size inputs
    #         updateNumericInput(session, inputId = "npergroup.tte.n", min = n - .2*n, value = n, max = n + .2*n)
    #         updateSliderInput(session, inputId = "npergroup.tte.s", min = n - .2*n, value = n, max = n + .2*n)
    #         updateNumericInput(session, inputId = "TotalN.tte.n", min = N - .2*N, value = N, max = N + .2*N)
    #         updateSliderInput(session, inputId = "TotalN.tte.s", min = N - .2*N, value = N, max = N + .2*N)
    #       }
    #     }
  })
  
} # Server Function