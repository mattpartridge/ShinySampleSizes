# Matt Partridge
# MS Project
# University of Minnesota
# Advisor: Julian Wolfson


############################## Libraries ##############################
library(shiny); library(shinydashboard); library(pwr); library(gsDesign)
############################## Libraries ##############################

server = function(input, output, clientData, session){
  ############################## One Mean ##############################
  ### UI
  output$OM = renderUI({
    list(
      # Inputs
      box(
        numericInput(inputId = "mu_N_OM", label = "Mean", value = 0.77, min = 0, max = NA, step = 0.01, width = NULL),
        sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 2, value = 0.77, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_OM %in% c("Sample Size", "Power")){
          list(
            numericInput(inputId = "nullmu_N_OM", label = "Hypothesis Mean", value = 1, min = 0, max = NA, step = 0.01, width = NULL),
            sliderInput(inputId = "nullmu_S_OM", label = "", min = 0, max = 2, value = 1, step = 0.01, round = FALSE, width = NULL))},
        if(input$solvefor_OM == "Precision"){
          list(
            numericInput(inputId = "sd_N_OM", label = "SD", value = 1, min = 0, max = NA, step = 0.1, width = NULL),
            sliderInput(inputId = "sd_S_OM", label = "", min = 0, max = 5, value = 1, step = 0.1, round = FALSE, width = NULL))},
        if(input$solvefor_OM %in% c("Power", "Precision")){
          list(
            numericInput(inputId = "N_N_OM", label = "Sample Size", value = 150, min = 0, max = NA, step = 1, width = NULL),
            sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 150, step = 1, round = FALSE, width = NULL))},
        numericInput(inputId = "alpha_N_OM", label = "Significance Level", value = 0.05, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_OM == "Sample Size"){
          list(
            numericInput(inputId = "power_N_OM", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
            sliderInput(inputId = "power_S_OM", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))}),
      # Outputs
      box(
        if(input$solvefor_OM == "Sample Size"){
          list(
            numericInput(inputId = "N_N_OM", label = "Sample Size", value = 150, min = 0, max = NA, step = 1, width = NULL),
            sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 150, step = 1, round = FALSE, width = NULL))},
        if(input$solvefor_OM == "Power"){
          list(
            numericInput(inputId = "power_N_OM", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
            sliderInput(inputId = "power_S_OM", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))},
        if(input$solvefor_OM == "Precision"){
          list(
            numericInput(inputId = "moe_N_OM", label = "+/- Precision", value = 0.2, min = 0, max = NA, step = 0.1, width = NULL),
            sliderInput(inputId = "moe_S_OM", label = "", min = 0, max = 3, value = 0.2, step = 0.1, round = FALSE, width = NULL),
            sliderInput(inputId = "ci_S_OM", label = "Confidence Interval", min = 0, max = 3, value = c(0.57, 0.97), step = 0.1, round = FALSE, width = NULL))}))
  })
  
  ### Numeric or Slider
  NorS_OM = reactiveValues( NorS = "" )
  observe({
    input$mu_N_OM; input$nullmu_N_OM; input$alpha_N_OM; input$power_N_OM; input$N_N_OM; input$sd_N_OM; input$moe_N_OM
    NorS_OM$NorS = "N"
  })
  observe({
    input$mu_S_OM; input$nullmu_S_OM; input$alpha_S_OM; input$power_S_OM; input$N_S_OM; input$sd_S_OM; input$moe_S_OM
    NorS_OM$NorS = "S"
  })
  
  ### Calculations
  observe({
    ##### Sample Size
    if(input$solvefor_OM == "Sample Size"){
      #### numericInput was updated
      if(NorS_OM$NorS == "N"){
        ### Update sliderInputs
        updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM, max = ifelse(input$mu_N_OM < 10, 10, round(input$mu_N_OM + sqrt(input$mu_N_OM), digits = 0)))
        updateSliderInput(session, "nullmu_S_OM", value = input$nullmu_N_OM, max = ifelse(input$nullmu_N_OM < 10, 10, round(input$nullmu_N_OM + sqrt(input$nullmu_N_OM), digits = 0)))
        updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
        updateSliderInput(session, "power_S_OM", value = input$power_N_OM)
      }
      #### sliderInput was updated
      if(NorS_OM$NorS == "S"){
        ### Update numericInputs
        updateNumericInput(session, "mu_N_OM", value = input$mu_S_OM)
        updateNumericInput(session, "nullmu_N_OM", value = input$nullmu_S_OM)
        updateNumericInput(session, "alpha_N_OM", value = input$alpha_S_OM)
        updateNumericInput(session, "power_N_OM", value = input$power_S_OM)
      }
      # Sample Size Calculations
      n = NULL
      mu = ifelse(is.null(input$mu_N_OM), 0.77, input$mu_N_OM)
      nullmu = ifelse(is.null(input$nullmu_N_OM), 1, input$nullmu_N_OM)
      d = mu - nullmu
      sig.level = ifelse(is.null(input$alpha_N_OM), 0.05, input$alpha_N_OM)
      power = ifelse(is.null(input$power_N_OM), 0.8, input$power_N_OM)
      type = "one.sample"
      alternative = "two.sided"
      n = round(pwr.t.test(n = n, d = d, sig.level = sig.level, power = power, type = type, alternative = alternative)$n, digits = 0)
      # Output
      updateNumericInput(session, inputId = "N_N_OM", value = n)
      updateSliderInput(session, inputId = "N_S_OM", value = n, max = round(n + sqrt(n), digits = 0))
    }
    
    ##### Power
    if(input$solvefor_OM == "Power"){
      #### Prep
      ### Numeric
      if(NorS_OM$NorS == "N"){
        # Update Slider Inputs
        updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM, max = ifelse(input$mu_N_OM < 10, 10, round(input$mu_N_OM + sqrt(input$mu_N_OM), digits = 0)))
        updateSliderInput(session, "nullmu_S_OM", value = input$nullmu_N_OM, max = ifelse(input$nullmu_N_OM < 10, 10, round(input$nullmu_N_OM + sqrt(input$nullmu_N_OM), digits = 0)))
        updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
        updateSliderInput(session, "N_S_OM", value = input$N_N_OM, max = ifelse(input$N_N_OM < 10, 10, round(input$N_N_OM + sqrt(input$N_N_OM), digits = 0)))
      }
      ### Slider
      if(NorS_OM$NorS == "S"){
        # Update Numeric Inputs
        updateNumericInput(session, "mu_N_OM", value = input$mu_S_OM)
        updateNumericInput(session, "nullmu_N_OM", value = input$nullmu_S_OM)
        updateSliderInput(session, "alpha_N_OM", value = input$alpha_S_OM)
        updateSliderInput(session, "N_N_OM", value = input$N_S_OM)
      }
      # Power Calculations
      n = ifelse(is.null(input$N_N_OM), 150, input$N_N_OM)
      mu = ifelse(is.null(input$mu_N_OM), 0.77, input$mu_N_OM)
      nullmu = ifelse(is.null(input$nullmu_N_OM), 1, input$nullmu_N_OM)
      d = mu - nullmu
      sig.level = ifelse(is.null(input$alpha_N_OM), 0.05, input$alpha_N_OM)
      power = NULL
      type = "one.sample"
      alternative = "two.sided"
      power = round(pwr.t.test(n = n, d = d, sig.level = sig.level, power = power, type = type, alternative = alternative)$power, digits = 2)
      # Output
      updateNumericInput(session, inputId = "power_N_OM", value = power)
      updateSliderInput(session, inputId = "power_S_OM", value = power)
    }
    
    ##### Precision
    if(input$solvefor_OM == "Precision"){
      #### Prep
      ### Numeric
      if(NorS_OM$NorS == "N"){
        # Update Slider Inputs
        updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM, max = ifelse(input$mu_N_OM < 10, 10, round(input$mu_N_OM + sqrt(input$mu_N_OM), digits = 0)))
        updateSliderInput(session, "sd_S_OM", value = input$sd_N_OM, max = ifelse(input$sd_N_OM < 10, 10, round(input$sd_N_OM + sqrt(input$sd_N_OM), digits = 0)))
        updateSliderInput(session, "N_S_OM", value = input$N_N_OM, max = ifelse(input$N_N_OM < 10, 10, round(input$N_N_OM + sqrt(input$N_N_OM), digits = 0)))
        updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
      }
      ### Slider
      if(NorS_OM$NorS == "S"){
        # Update Numeric Inputs
        updateNumericInput(session, "mu_N_OM", value = input$mu_S_OM)
        updateNumericInput(session, "sd_N_OM", value = input$sd_S_OM)
        updateNumericInput(session, "N_N_OM", value = input$N_S_OM)
        updateNumericInput(session, "alpha_N_OM", value = input$alpha_S_OM)
      }
      # Precision Calculations
      mu = ifelse(is.null(input$mu_N_OM), 0.77, input$mu_N_OM)
      sd = ifelse(is.null(input$sd_N_OM), 1, input$sd_N_OM)
      n = ifelse(is.null(input$N_N_OM), 150, input$N_N_OM)
      alpha = ifelse(is.null(input$alpha_N_OM), 0.05, input$alpha_N_OM)
      Z.alpha.2 = qnorm(1 - alpha/2)
      se = sd/sqrt(n)
      moe = round(Z.alpha.2*se, digits = 2)
      lo = round(mu - moe, digits = 2)
      hi = round(mu + moe, digits = 2)
      # Output
      updateNumericInput(session, "moe_N_OM", value = moe)
      updateSliderInput(session, "moe_S_OM", value = moe, max = ifelse(moe < 1, 1, round(moe + sqrt(moe), digits = 0)))
      updateSliderInput(session, "ci_S_OM", value = c(lo, hi), max = ifelse(hi < 1, 1, round(hi + sqrt(hi), digits = 0)))
    }
  })
  ############################## One Mean ##############################
  
  ############################## One Proportion ##############################
  ### UI
  output$OP = renderUI({
    ### Sample Size
    list(
      # Inputs
      box(
        numericInput(inputId = "p_N_OP", label = "Proportion", value = 0.5, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "p_S_OP", label = "", min = 0, max = 1, value = 0.5, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_OP %in% c("Sample Size", "Power")){
          list(
            numericInput(inputId = "nullp_N_OP", label = "Hypothesis Proportion", value = 0.52, min = 0, max = 1, step = 0.01, width = NULL),
            sliderInput(inputId = "nullp_S_OP", label = "", min = 0, max = 1, value = 0.52, step = 0.01, round = FALSE, width = NULL))},
        if(input$solvefor_OP %in% c("Power", "Precision")){
          list(
            numericInput(inputId = "N_N_OP", label = "Sample Size", value = 4903, min = 0, max = NA, step = 1, width = NULL),
            sliderInput(inputId = "N_S_OP", label = "", min = 0, max = 5000, value = 4903, step = 1, round = FALSE, width = NULL))},
        numericInput(inputId = "alpha_N_OP", label = "Significance Level", value = 0.05, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "alpha_S_OP", label = "", min = 0, max = 1, value = 0.05, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_OP == "Sample Size"){
          list(
            numericInput(inputId = "power_N_OP", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
            sliderInput(inputId = "power_S_OP", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))}),
      # Output
      box(
        if(input$solvefor_OP == "Sample Size"){
          list(
            numericInput(inputId = "N_N_OP", label = "Sample Size", value = 4903, min = 0, max = NA, step = 1, width = NULL),
            sliderInput(inputId = "N_S_OP", label = "", min = 0, max = 5000, value = 4903, step = 1, round = FALSE, width = NULL))},
        if(input$solvefor_OP == "Power"){
          list(
            numericInput(inputId = "power_N_OP", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
            sliderInput(inputId = "power_S_OP", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))},
        if(input$solvefor_OP == "Precision"){
          list(
            numericInput(inputId = "moe_N_OP", label = "+/- Precision", value = 0.01, min = 0, max = NA, step = 0.01, width = NULL),
            sliderInput(inputId = "moe_S_OP", label = "", min = 0, max = 1, value = 0.01, step = 0.01, round = FALSE, width = NULL),
            sliderInput(inputId = "ci_S_OP", label = "Confidence Interval", min = 0, max = 1, value = c(0.49, 0.51), step = 0.1, round = FALSE, width = NULL))}))
  })
  
  ### Numeric or Slider
  NorS_OP = reactiveValues( NorS = "" )
  observe({
    input$p_N_OP; input$nullp_N_OP; input$alpha_N_OP; input$power_N_OP; input$N_N_OP; input$moe_N_OP
    NorS_OP$NorS = "N"
  })
  observe({
    input$p_S_OP; input$nullp_S_OP; input$alpha_S_OP; input$power_S_OP; input$N_S_OP; input$moe_S_OP
    NorS_OP$NorS = "S"
  })
  
  ### Calculations
  observe({
    ##### Sample Size
    if(input$solvefor_OP == "Sample Size"){
      #### numericInput was updated
      if(NorS_OP$NorS == "N"){
        ### Update sliderInputs
        updateSliderInput(session, "p_S_OP", value = input$p_N_OP)
        updateSliderInput(session, "nullp_S_OP", value = input$nullp_N_OP)
        updateSliderInput(session, "alpha_S_OP", value = input$alpha_N_OP)
        updateSliderInput(session, "power_S_OP", value = input$power_N_OP)
      }
      #### sliderInput was updated
      if(NorS_OP$NorS == "S"){
        ### Update numericInputs
        updateNumericInput(session, "p_N_OP", value = input$p_S_OP)
        updateNumericInput(session, "nullp_N_OP", value = input$nullp_S_OP)
        updateNumericInput(session, "alpha_N_OP", value = input$alpha_S_OP)
        updateNumericInput(session, "power_N_OP", value = input$power_S_OP)
      }
      # Sample Size Calculations
      p1 = ifelse(is.null(input$p_N_OP), 0.5, input$p_N_OP)
      p2 = ifelse(is.null(input$nullp_N_OP), 0.52, input$nullp_N_OP)
      h = ES.h(p1, p2)
      n = NULL
      sig.level = ifelse(is.null(input$alpha_N_OP), 0.05, input$alpha_N_OP)
      power = ifelse(is.null(input$power_N_OP), 0.8, input$power_N_OP)
      alternative = "two.sided"
      n = round(pwr.p.test(h, n, sig.level, power, alternative)$n, digits = 0)
      # Output
      updateNumericInput(session, inputId = "N_N_OP", value = n)
      updateSliderInput(session, inputId = "N_S_OP", value = n, max = ifelse(n < 10, 10, round(n + sqrt(n), digits = 0)))
    }
    
    ##### Power
    if(input$solvefor_OP == "Power"){
      #### Prep
      ### Numeric
      if(NorS_OP$NorS == "N"){
        # Update Slider Inputs
        updateSliderInput(session, "p_S_OP", value = input$p_N_OP)
        updateSliderInput(session, "nullp_S_OP", value = input$nullp_N_OP)
        updateSliderInput(session, "alpha_S_OP", value = input$alpha_N_OP)
        updateSliderInput(session, "N_S_OP", value = input$N_N_OP, max = ifelse(input$N_N_OM < 10, 10, round(input$N_N_OM + sqrt(input$N_N_OM), digits = 0)))
      }
      ### Slider
      if(NorS_OP$NorS == "S"){
        # Update Numeric Inputs
        updateNumericInput(session, "p_N_OP", value = input$p_S_OP)
        updateNumericInput(session, "nullp_N_OP", value = input$nullp_S_OP)
        updateNumericInput(session, "alpha_N_OP", value = input$alpha_S_OP)
        updateNumericInput(session, "N_N_OP", value = input$N_S_OP)
      }
      # Power Calculations
      p1 = ifelse(is.null(input$p_N_OP), 0.5, input$p_N_OP)
      p2 = ifelse(is.null(input$nullp_N_OP), 0.52, input$nullp_N_OP)
      h = ES.h(p1, p2)
      n = ifelse(is.null(input$N_N_OP), 4903, input$N_N_OP)
      sig.level = ifelse(is.null(input$alpha_N_OP), 0.05, input$alpha_N_OP)
      power = NULL
      alternative = "two.sided"
      power = round(pwr.p.test(h, n, sig.level, power, alternative)$power, digits = 2)
      # Output
      updateNumericInput(session, inputId = "power_N_OP", value = power)
      updateSliderInput(session, inputId = "power_S_OP", value = power)
    }
    
    ##### Precision
    if(input$solvefor_OP == "Precision"){
      #### Prep
      ### Numeric
      if(NorS_OP$NorS == "N"){
        # Update Slider Inputs
        updateSliderInput(session, "p_S_OP", value = input$p_N_OP)
        updateSliderInput(session, "N_S_OP", value = input$N_N_OP, max = ifelse(input$N_N_OM < 10, 10, round(input$N_N_OM + sqrt(input$N_N_OM), digits = 0)))
        updateSliderInput(session, "alpha_S_OP", value = input$alpha_N_OP)
      }
      ### Slider
      if(NorS_OP$NorS == "S"){
        # Update Numeric Inputs
        updateNumericInput(session, "p_N_OP", value = input$p_S_OP)
        updateNumericInput(session, "N_N_OP", value = input$N_S_OP)
        updateNumericInput(session, "alpha_N_OP", value = input$alpha_S_OP)
      }
      # Precision Calculations
      p = ifelse(is.null(input$p_N_OP), 0.5, input$p_N_OP)
      n = ifelse(is.null(input$N_N_OP), 4903, input$N_N_OP)
      alpha = ifelse(is.null(input$alpha_N_OP), 0.05, input$alpha_N_OP)
      Z.alpha.2 = qnorm(1 - alpha/2)
      moe = round(sqrt(((Z.alpha.2^2)*(p*(1-p)))/n), digits = 2)
      lo = ifelse(p - moe < 0, 0, round(p - moe, digits = 2))
      hi = ifelse(p + moe > 1, 1, round(p + moe, digits = 2))
      # Output
      updateNumericInput(session, "moe_N_OP", value = moe)
      updateSliderInput(session, "moe_S_OP", value = moe, max = ifelse(moe < 1, 1, round(moe + sqrt(moe), digits = 0)))
      updateSliderInput(session, "ci_S_OP", value = c(lo, hi), max = ifelse(hi < 1, 1, round(hi + sqrt(hi), digits = 0)))
    }
  })
  ############################## One Proportion ##############################
  
  ############################## Two Means ##############################
  ### UI
  output$TM = renderUI({
    ### Sample Size
    list(
      # Inputs
      box(
        numericInput(inputId = "mu1_N_TM", label = "Mean One", value = 1.0, min = 0, max = NA, step = 0.01, width = NULL),
        sliderInput(inputId = "mu1_S_TM", label = "", min = 0, max = 10, value = 1.0, step = 0.01, round = FALSE, width = NULL),
        numericInput(inputId = "N1_N_TM", label = "Sample Size One", value = 100, min = 0, max = NA, step = 1, width = NULL),
        sliderInput(inputId = "N1_S_TM", label = "", min = 0, max = 10000, value = 100, step = 1, round = FALSE, width = NULL),
        numericInput(inputId = "mu2_N_TM", label = "Mean Two", value = 1.3, min = 0, max = NA, step = 0.01, width = NULL),
        sliderInput(inputId = "mu2_S_TM", label = "", min = 0, max = 10, value = 1.3, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_TM == "Power"){
          list(
            numericInput(inputId = "N2_N_TM", label = "Sample Size Two", value = 695, min = 0, max = NA, step = 1, width = NULL),
            sliderInput(inputId = "N2_S_TM", label = "", min = 0, max = 10000, value = 695, step = 1, round = FALSE, width = NULL))},
        numericInput(inputId = "alpha_N_TM", label = "Significance Level", value = 0.05, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "alpha_S_TM", label = "", min = 0, max = 1, value = 0.05, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_TM == "Sample Size"){
          list(
            numericInput(inputId = "power_N_TM", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
            sliderInput(inputId = "power_S_TM", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))}),
      # Outputs
      box(
        if(input$solvefor_TM == "Sample Size"){
          list(
            numericInput(inputId = "N2_N_TM", label = "Sample Size Two", value = 695, min = 0, max = NA, step = 1, width = NULL),
            sliderInput(inputId = "N2_S_TM", label = "", min = 0, max = 10000, value = 695, step = 1, round = FALSE, width = NULL))},
        if(input$solvefor_TM == "Power"){
          list(
            numericInput(inputId = "power_N_TM", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
            sliderInput(inputId = "power_S_TM", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))}))
  })
  
  ### Numeric or Slider
  NorS_TM = reactiveValues( NorS = "" )
  observe({
    input$mu1_N_TM; input$N1_N_TM; input$mu2_N_TM; input$N2_N_TM; input$alpha_N_TM; input$power_N_TM
    NorS_TM$NorS = "N"
  })
  observe({
    input$mu1_S_TM; input$N1_S_TM; input$mu2_S_TM; input$N2_S_TM; input$alpha_S_TM; input$power_S_TM
    NorS_TM$NorS = "S"
  })
  
  ### Calculations
  observe({
    ##### Sample Size
    if(input$solvefor_TM == "Sample Size"){
      #### numericInput was updated
      if(NorS_TM$NorS == "N"){
        ### Update sliderInputs
        updateSliderInput(session, "mu1_S_TM", value = input$mu1_N_TM, max = ifelse(input$mu1_N_TM < 10, 10, round(input$mu1_N_TM + sqrt(input$mu1_N_TM), digits = 0)))
        updateSliderInput(session, "N1_S_TM", value = input$N1_N_TM, max = ifelse(input$N1_N_TM < 10, 10, round(input$N1_N_TM + sqrt(input$N1_N_TM), digits = 0)))
        updateSliderInput(session, "mu2_S_TM", value = input$mu2_N_TM, max = ifelse(input$mu2_N_TM < 10, 10, round(input$mu2_N_TM + sqrt(input$mu2_N_TM), digits = 0)))
        updateSliderInput(session, "alpha_S_TM", value = input$alpha_N_TM)
        updateSliderInput(session, "power_S_TM", value = input$power_N_TM)
      }
      #### sliderInput was updated
      if(NorS_TM$NorS == "S"){
        ### Update numericInputs
        updateNumericInput(session, "mu1_N_TM", value = input$mu1_S_TM)
        updateNumericInput(session, "N1_N_TM", value = input$N1_S_TM)
        updateNumericInput(session, "mu2_N_TM", value = input$mu2_S_TM)
        updateNumericInput(session, "alpha_N_TM", value = input$alpha_S_TM)
        updateNumericInput(session, "power_N_TM", value = input$power_S_TM)
      }
      # Sample Size Calculations
      mu1 = ifelse(is.null(input$mu1_N_TM), 1, input$mu1_N_TM)
      n1 = ifelse(is.null(input$N1_N_TM), 100, input$N1_N_TM)
      mu2 = ifelse(is.null(input$mu2_N_TM), 1.3, input$mu2_N_TM)
      n2 = NULL
      d = mu1 - mu2
      sig.level = ifelse(is.null(input$alpha_N_TM), 0.05, input$alpha_N_TM)
      power = ifelse(is.null(input$power_N_TM), 0.8, input$power_N_TM)
      alternative = "two.sided"
      n2 = try(round(pwr.t2n.test(n1, n2, d, sig.level, power, alternative)$n2, digits = 0), silent = T)
      n2 = ifelse(class(n2) == "try-error", 0, n2)
      # Output
      updateNumericInput(session, inputId = "N2_N_TM", value = n2)
      updateSliderInput(session, inputId = "N2_S_TM", value = n2, max = ifelse(n2 < 10, 10, round(n2 + sqrt(n2), digits = 0)))
    }
    
    ##### Power
    if(input$solvefor_TM == "Power"){
      #### Prep
      ### Numeric
      if(NorS_TM$NorS == "N"){
        # Update Slider Inputs
        updateSliderInput(session, "mu1_S_TM", value = input$mu1_N_TM, max = ifelse(input$mu1_N_TM < 10, 10, round(input$mu1_N_TM + sqrt(input$mu1_N_TM), digits = 0)))
        updateSliderInput(session, "N1_S_TM", value = input$N1_N_TM, max = ifelse(input$N1_N_TM < 10, 10, round(input$N1_N_TM + sqrt(input$N1_N_TM), digits = 0)))
        updateSliderInput(session, "mu2_S_TM", value = input$mu2_N_TM, max = ifelse(input$mu2_N_TM < 10, 10, round(input$mu2_N_TM + sqrt(input$mu2_N_TM), digits = 0)))
        updateSliderInput(session, "N2_S_TM", value = input$N2_N_TM, max = ifelse(input$N2_N_TM < 10, 10, round(input$N2_N_TM + sqrt(input$N2_N_TM), digits = 0)))
        updateSliderInput(session, "alpha_S_TM", value = input$alpha_N_TM)
      }
      ### Slider
      if(NorS_TM$NorS == "S"){
        # Update Numeric Inputs
        updateNumericInput(session, "mu1_N_TM", value = input$mu1_S_TM)
        updateNumericInput(session, "N1_N_TM", value = input$N1_S_TM)
        updateNumericInput(session, "mu2_N_TM", value = input$mu2_S_TM)
        updateNumericInput(session, "N2_N_TM", value = input$N2_S_TM)
        updateNumericInput(session, "alpha_N_TM", value = input$alpha_S_TM)
      }
      # Power Calculations
      mu1 = ifelse(is.null(input$mu1_N_TM), 1, input$mu1_N_TM)
      n1 = ifelse(is.null(input$N1_N_TM), 100, input$N1_N_TM)
      mu2 = ifelse(is.null(input$mu2_N_TM), 1.3, input$mu2_N_TM)
      n2 = ifelse(is.null(input$N2_N_TM), 695, input$N2_N_TM)
      d = mu1 - mu2
      sig.level = ifelse(is.null(input$alpha_N_TM), 0.05, input$alpha_N_TM)
      power = NULL
      alternative = "two.sided"
      power = try(round(pwr.t2n.test(n1, n2, d, sig.level, power, alternative)$power, digits = 2), silent = T)
      power = ifelse(class(power) == "try-error", 0, power)
      # Output
      updateNumericInput(session, inputId = "power_N_TM", value = power)
      updateSliderInput(session, inputId = "power_S_TM", value = power)
    }
  })
  ############################## Two Means ##############################
  
  ############################## Two Proportions ##############################
  ### UI
  output$TP = renderUI({
    list(
      # Inputs
      box(
        numericInput(inputId = "p1_N_TP", label = "Proportion One", value = 0.5, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "p1_S_TP", label = "", min = 0, max = 1, value = 0.5, step = 0.01, round = FALSE, width = NULL),
        numericInput(inputId = "N1_N_TP", label = "Sample Size One", value = 100, min = 0, max = NA, step = 1, width = NULL),
        sliderInput(inputId = "N1_S_TP", label = "", min = 0, max = 5000, value = 100, step = 1, round = FALSE, width = NULL),
        numericInput(inputId = "p2_N_TP", label = "Proportion Two", value = 0.7, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "p2_S_TP", label = "", min = 0, max = 1, value = 0.7, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_TP == "Power"){
          list(numericInput(inputId = "N2_N_TP", label = "Sample Size Two", value = 695, min = 0, max = NA, step = 1, width = NULL),
               sliderInput(inputId = "N2_S_TP", label = "", min = 0, max = 5000, value = 695, step = 1, round = FALSE, width = NULL))},
        numericInput(inputId = "alpha_N_TP", label = "Significance Level", value = 0.05, min = 0, max = 1, step = 0.01, width = NULL),
        sliderInput(inputId = "alpha_S_TP", label = "", min = 0, max = 1, value = 0.05, step = 0.01, round = FALSE, width = NULL),
        if(input$solvefor_TP == "Sample Size"){
          list(numericInput(inputId = "power_N_TP", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
               sliderInput(inputId = "power_S_TP", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))}
      ),
      # Outputs
      box(
        if(input$solvefor_TP == "Sample Size"){
          list(numericInput(inputId = "N2_N_TP", label = "Sample Size Two", value = 695, min = 0, max = NA, step = 1, width = NULL),
               sliderInput(inputId = "N2_S_TP", label = "", min = 0, max = 5000, value = 695, step = 1, round = FALSE, width = NULL))},
        if(input$solvefor_TP == "Power"){
          list(numericInput(inputId = "power_N_TP", label = "Power", value = 0.8, min = 0, max = 1, step = 0.05, width = NULL),
               sliderInput(inputId = "power_S_TP", label = "", min = 0, max = 1, value = 0.8, step = 0.05, round = FALSE, width = NULL))}
      )
    )
  })
  
  ### Numeric or Slider
  NorS_TP = reactiveValues( NorS = "" )
  observe({
    input$p1_N_TP; input$N1_N_TP; input$p2_N_TP; input$N2_N_TP; input$alpha_N_TP; input$power_N_TP
    NorS_TP$NorS = "N"
  })
  observe({
    input$p1_S_TP; input$N1_S_TP; input$p2_S_TP; input$N2_S_TP; input$alpha_S_TP; input$power_S_TP
    NorS_TP$NorS = "S"
  })
  
  ### Calculations
  observe({
    ##### Sample Size
    if(input$solvefor_TP == "Sample Size"){
      #### numericInput was updated
      if(NorS_TP$NorS == "N"){
        ### Update sliderInputs
        updateSliderInput(session, "p1_S_TP", value = input$p1_N_TP)
        updateSliderInput(session, "N1_S_TP", value = input$N1_N_TP, max = ifelse(input$N1_N_TP < 10, 10, round(input$N1_N_TP + sqrt(input$N1_N_TP), digits = 0)))
        updateSliderInput(session, "p2_S_TP", value = input$p2_N_TP)
        updateSliderInput(session, "N2_S_TP", value = input$N2_N_TP, max = ifelse(input$N2_N_TP < 10, 10, round(input$N2_N_TP + sqrt(input$N2_N_TP), digits = 0)))
        updateSliderInput(session, "alpha_S_TP", value = input$alpha_N_TP)
        updateSliderInput(session, "power_S_TP", value = input$power_N_TP)
      }
      #### sliderInput was updated
      if(NorS_TP$NorS == "S"){
        ### Update numericInputs
        updateNumericInput(session, "p1_N_TP", value = input$p1_S_TP)
        updateNumericInput(session, "N1_N_TP", value = input$N1_S_TP)
        updateNumericInput(session, "p2_N_TP", value = input$p2_S_TP)
        updateNumericInput(session, "N2_N_TP", value = input$N2_S_TP)
        updateNumericInput(session, "alpha_N_TP", value = input$alpha_S_TP)
        updateNumericInput(session, "power_N_TP", value = input$power_S_TP)
      }
      # Sample Size Calculations
      p1 = ifelse(is.null(input$p1_N_TP), 0.5, input$p1_N_TP)
      n1 = ifelse(is.null(input$N1_N_TP), 100, input$N1_N_TP)
      p2 = ifelse(is.null(input$p2_N_TP), 0.7, input$p2_N_TP)
      n2 = NULL
      h = ES.h(p1, p2)
      sig.level = ifelse(is.null(input$alpha_N_TP), 0.05, input$alpha_N_TP)
      power = ifelse(is.null(input$power_N_TP), 0.8, input$power_N_TP)
      alternative = "two.sided"
      n2 = try(round(pwr.2p2n.test(h, n1, n2, sig.level, power, alternative)$n2, digits = 0), silent = T)
      n2 = ifelse(class(n2) == "try-error", 0, n2)
      # Output
      updateNumericInput(session, inputId = "N2_N_TP", value = n2)
      updateSliderInput(session, inputId = "N2_S_TP", value = n2, max = ifelse(n2 < 10, 10, round(n2 + sqrt(n2), digits = 0)))
    }
    
    ##### Power
    if(input$solvefor_TP == "Power"){
      #### Prep
      ### Numeric
      if(NorS_TP$NorS == "N"){
        # Update Slider Inputs
        updateSliderInput(session, "p1_S_TP", value = input$p1_N_TP)
        updateSliderInput(session, "N1_S_TP", value = input$N1_N_TP, max = ifelse(input$N1_N_TP < 10, 10, round(input$N1_N_TP + sqrt(input$N1_N_TP), digits = 0)))
        updateSliderInput(session, "p2_S_TP", value = input$p2_N_TP)
        updateSliderInput(session, "N2_S_TP", value = input$N2_N_TP, max = ifelse(input$N2_N_TP < 10, 10, round(input$N2_N_TP + sqrt(input$N2_N_TP), digits = 0)))
        updateSliderInput(session, "alpha_S_TP", value = input$alpha_N_TP)
      }
      ### Slider
      if(NorS_TP$NorS == "S"){
        # Update Numeric Inputs
        updateNumericInput(session, "p1_N_TP", value = input$p1_S_TP)
        updateNumericInput(session, "N1_N_TP", value = input$N1_S_TP)
        updateNumericInput(session, "p2_N_TP", value = input$p2_S_TP)
        updateNumericInput(session, "N2_N_TP", value = input$N2_S_TP)
        updateNumericInput(session, "alpha_N_TP", value = input$alpha_S_TP)
      }
      # Power Calculations
      p1 = ifelse(is.null(input$p1_N_TP), 1, input$p1_N_TP)
      n1 = ifelse(is.null(input$N1_N_TP), 100, input$N1_N_TP)
      p2 = ifelse(is.null(input$p2_N_TP), 1.3, input$p2_N_TP)
      n2 = ifelse(is.null(input$N2_N_TP), 695, input$N2_N_TP)
      h = ES.h(p1, p2)
      sig.level = ifelse(is.null(input$alpha_N_TP), 0.05, input$alpha_N_TP)
      power = NULL
      alternative = "two.sided"
      power = try(round(pwr.2p2n.test(h, n1, n2, sig.level, power, alternative)$power, digits = 2), silent = T)
      power = ifelse(class(power) == "try-error", 0, power)
      # Output
      updateNumericInput(session, inputId = "power_N_TP", value = power)
      updateSliderInput(session, inputId = "power_S_TP", value = power)
    }
  })
  ############################## Two Proportions ##############################
  
  ############################## Time to Event ##############################
  ### UI
  output$TTE = renderUI({
    list(
      box(width = 4,
          numericInput(inputId = "alpha_N_TTE", label = "Significance Level", value = 0.05, min = 0, max = 1, step = 0.01, width = NULL),
          sliderInput(inputId = "alpha_S_TTE", label = "", min = 0, max = 1, value = 0.05, step = 0.01, round = FALSE, width = NULL),
          if(input$solvefor_TTE == "Sample Size"){
            list(
              numericInput(inputId = "power_N_TTE", label = "Power", value = 0.8, min = 0, max = 1, step = 0.01, width = NULL),
              sliderInput(inputId = "power_S_TTE", label = "", min = 0, max = 1, value = 0.8, step = 0.01, round = FALSE, width = NULL))},
          if(input$solvefor_TTE == "Power"){
            list(
              numericInput(inputId = "N_N_TTE", label = "Total Sample Size", value = 194, min = 0, max = NA, step = 1, width = NULL),
              sliderInput(inputId = "N_S_TTE", label = "", min = 0, max = 250, value = 194, step = 1, round = FALSE, width = NULL)
            )
          },
          numericInput(inputId = "ratio_N_TTE", label = "Sample Allocation Ratio", value = 1, min = 0, max = NA, step = 0.1, width = NULL),
          sliderInput(inputId = "ratio_S_TTE", label = "", min = 0, max = 5, value = 1, step = 0.1, round = FALSE, width = NULL),
          numericInput(inputId = "ctrllambda_N_TTE", label = "Control Group's Event Rate per Unit of Time", value = 1, min = 0, max = NA, step = 0.01, width = NULL),
          sliderInput(inputId = "ctrllambda_S_TTE", label = "", min = 0, max = 10, value = 1, step = 0.01, round = FALSE, width = NULL),
          numericInput(inputId = "explambda_N_TTE", label = "Experimental Group's Event Rate per Unit of Time", value = 1.5, min = 0, max = NA, step = 0.01, width = NULL),
          sliderInput(inputId = "explambda_S_TTE", label = "", min = 0, max = 10, value = 1.5, step = 0.01, round = FALSE, width = NULL),
          if(input$solvefor_TTE == "Sample Size"){
            list(
              numericInput(inputId = "ctrlcensorrate_N_TTE", label = "Control Group's Censoring Rate", value = 0, min = 0, max = 1, step = 0.01, width = NULL),
              sliderInput(inputId = "ctrlcensorrate_S_TTE", label = "", min = 0, max = 1, value = 0, step = 0.01, round = FALSE, width = NULL),
              numericInput(inputId = "expcensorrate_N_TTE", label = "Experimental Group's Censoring Rate", value = 0, min = 0, max = 1, step = 0.01, width = NULL),
              sliderInput(inputId = "expcensorrate_S_TTE", label = "", min = 0,  max = 1, value = 0, step = 0.01, round = FALSE, width = NULL))}),
      box(width = 4,
          if(input$solvefor_TTE == "Sample Size"){
            list(
              numericInput(inputId = "N_N_TTE",  label = "Total Sample Size", value = 194, min = 0, max = NA, step = 0.01, width = NULL),
              sliderInput(inputId = "N_S_TTE", label = "", min = 0, max = 250, value = 194, step = 0.01, round = FALSE, width = NULL))},
          if(input$solvefor_TTE == "Power"){
            list(
              numericInput(inputId = "power_N_TTE", label = "Power", value = 0.8, min = 0, max = 1, step = 0.01, width = NULL),
              sliderInput(inputId = "power_S_TTE", label = "", min = 0, max = 1, value = 0.8, step = 0.01, round = FALSE, width = NULL))}))
  })
  
  ### Numeric or Slider
  NorS_TTE = reactiveValues( NorS = "" )
  observe({
    input$studyduration_N_TTE; input$enrollmentduration_N_TTE; input$gamma_N_TTE; input$alpha_N_TTE; input$power_N_TTE; input$ratio_N_TTE; input$ctrllambda_N_TTE; input$explambda_N_TTE; input$ctrlcensorrate_N_TTE; input$expcensorrate_N_TTE; input$N_N_TTE
    NorS_TTE$NorS = "N"
  })
  observe({
    input$studyduration_S_TTE; input$enrollmentduration_S_TTE; input$gamma_S_TTE; input$alpha_S_TTE; input$power_S_TTE; input$ratio_S_TTE; input$ctrllambda_S_TTE; input$explambda_S_TTE; input$ctrlcensorrate_S_TTE; input$expcensorrate_S_TTE; input$N_S_TTE
    NorS_TTE$NorS = "S"
  })
  
  ### Calculations
  observe({
    ##### Sample Size
    if(input$solvefor_TTE == "Sample Size"){
      #### numericInput was updated
      if(NorS_TTE$NorS == "N"){
        updateSliderInput(session, "studyduration_S_TTE", value = input$studyduration_N_TTE, max = ifelse(input$studyduration_N_TTE < 10, 10, round(input$studyduration_N_TTE + sqrt(input$studyduration_N_TTE), digits = 0)))
        updateSliderInput(session, "enrollmentduration_S_TTE", value = input$enrollmentduration_N_TTE, max = ifelse(input$enrollmentduration_N_TTE < 10, 10, round(input$enrollmentduration_N_TTE + sqrt(input$enrollmentduration_N_TTE), digits = 0)))
        updateSliderInput(session, "gamma_S_TTE", value = input$gamma_N_TTE, max = ifelse(input$gamma_N_TTE < 10, 10, round(input$gamma_N_TTE + sqrt(input$gamma_N_TTE), digits = 0)))
        updateSliderInput(session, "alpha_S_TTE", value = input$alpha_N_TTE)
        updateSliderInput(session, "power_S_TTE", value = input$power_N_TTE)
        updateSliderInput(session, "ratio_S_TTE", value = input$ratio_N_TTE, max = ifelse(input$ratio_N_TTE < 10, 10, round(input$ratio_N_TTE + sqrt(input$ratio_N_TTE), digits = 0)))
        updateSliderInput(session, "ctrllambda_S_TTE", value = input$ctrllambda_N_TTE, max = ifelse(input$ctrllambda_N_TTE < 10, 10, round(input$ctrllambda_N_TTE + sqrt(input$ctrllambda_N_TTE), digits = 0)))
        updateSliderInput(session, "explambda_S_TTE", value = input$explambda_N_TTE, max = ifelse(input$explambda_N_TTE < 10, 10, round(input$explambda_N_TTE + sqrt(input$explambda_N_TTE), digits = 0)))
        updateSliderInput(session, "ctrlcensorrate_S_TTE", value = input$ctrlcensorrate_N_TTE)
        updateSliderInput(session, "expcensorrate_S_TTE", value = input$expcensorrate_N_TTE)
      }
      #### sliderInput was updated
      if(NorS_TTE$NorS == "S"){
        updateNumericInput(session, "studyduration_N_TTE", value = input$studyduration_S_TTE)
        updateNumericInput(session, "enrollmentduration_N_TTE", value = input$enrollmentduration_S_TTE)
        updateNumericInput(session, "gamma_N_TTE", value = input$gamma_S_TTE)
        updateNumericInput(session, "alpha_N_TTE", value = input$alpha_S_TTE)
        updateNumericInput(session, "power_N_TTE", value = input$power_S_TTE)
        updateNumericInput(session, "ratio_N_TTE", value = input$ratio_S_TTE)
        updateNumericInput(session, "ctrllambda_N_TTE", value = input$ctrllambda_S_TTE)
        updateNumericInput(session, "explambda_N_TTE", value = input$explambda_S_TTE)
        updateNumericInput(session, "ctrlcensorrate_N_TTE", value = input$ctrlcensorrate_S_TTE)
        updateNumericInput(session, "expcensorrate_N_TTE", value = input$expcensorrate_S_TTE)
      }
      # Sample Size Calculations
      ctrllambda = ifelse(is.null(input$ctrllambda_N_TTE), 1, input$ctrllambda_N_TTE)
      ctrlcensorrate = ifelse(is.null(input$ctrlcensorrate_N_TTE), 0, input$ctrlcensorrate_N_TTE)
      lambda1 = ctrllambda*(ctrllambda/(ctrllambda + ctrlcensorrate))
      explambda = ifelse(is.null(input$explambda_N_TTE), 1.5, input$explambda_N_TTE)
      expcensorrate = ifelse(is.null(input$expcensorrate_N_TTE), 0, input$expcensorrate_N_TTE)
      lambda2 = explambda*(explambda/(explambda + expcensorrate))
      Tstudy = ifelse(is.null(input$studyduration_N_TTE), 5, input$studyduration_N_TTE)
      Tenrollment = ifelse(is.null(input$enrollmentduration_N_TTE) | input$enrollmentduration_N_TTE == 0, 0.1^10, input$enrollmentduration_N_TTE)
      eta = 0
      ratio = ifelse(is.null(input$ratio_N_TTE), 1, input$ratio_N_TTE)
      alpha = ifelse(is.null(input$alpha_N_TTE), 0.05, input$alpha_N_TTE)
      beta =  1 - ifelse(is.null(input$power_N_TTE), 0.8, input$power_N_TTE)
      sided = 2
      approx = FALSE
      type = "rr" # Im not sure
      entry = ifelse(is.null(input$enrollmentdist_SE_TTE), "unif", ifelse(input$enrollmentdist_SE_TTE == "Uniform", "unif", "expo"))
      gamma = ifelse(is.null(input$gamma_N_TTE), NA, input$gamma_N_TTE) # Is this how its supposed to be?
      N = ceiling(nSurvival(lambda1 = lambda1, lambda2 = lambda2, Ts = Tstudy, Tr = Tenrollment, eta = eta, ratio = ratio, alpha = alpha, beta = beta, sided = sided, approx = approx, type = type, entry = entry, gamma = gamma)$n)
      # Output
      updateNumericInput(session, inputId = "N_N_TTE", value = N, max = N + sqrt(N))
      updateSliderInput(session, inputId = "N_S_TTE", value = N, max = N + sqrt(N))
    }
    
    ##### Power
    if(input$solvefor_TTE == "Power"){
      #### Prep
      #### numericInput was updated
      if(NorS_TTE$NorS == "N"){
        updateSliderInput(session, "studyduration_S_TTE", value = input$studyduration_N_TTE, max = ifelse(input$studyduration_N_TTE < 10, 10, round(input$studyduration_N_TTE + sqrt(input$studyduration_N_TTE), digits = 0)))
        updateSliderInput(session, "enrollmentduration_S_TTE", value = input$enrollmentduration_N_TTE, max = ifelse(input$enrollmentduration_N_TTE < 10, 10, round(input$enrollmentduration_N_TTE + sqrt(input$enrollmentduration_N_TTE), digits = 0)))
        updateSliderInput(session, "gamma_S_TTE", value = input$gamma_N_TTE, max = ifelse(input$gamma_N_TTE < 10, 10, round(input$gamma_N_TTE + sqrt(input$gamma_N_TTE), digits = 0)))
        updateSliderInput(session, "alpha_S_TTE", value = input$alpha_N_TTE)
        updateSliderInput(session, "N_S_TTE", value = input$N_N_TTE, max = ifelse(input$N_N_TTE < 10, 10, round(input$N_N_TTE + sqrt(input$N_N_TTE), digits = 0)))
        updateSliderInput(session, "ratio_S_TTE", value = input$ratio_N_TTE, max = ifelse(input$ratio_N_TTE < 10, 10, round(input$ratio_N_TTE + sqrt(input$ratio_N_TTE), digits = 0)))
        updateSliderInput(session, "ctrllambda_S_TTE", value = input$ctrllambda_N_TTE, max = ifelse(input$ctrllambda_N_TTE < 10, 10, round(input$ctrllambda_N_TTE + sqrt(input$ctrllambda_N_TTE), digits = 0)))
        updateSliderInput(session, "explambda_S_TTE", value = input$explambda_N_TTE, max = ifelse(input$explambda_N_TTE < 10, 10, round(input$explambda_N_TTE + sqrt(input$explambda_N_TTE), digits = 0)))
        updateSliderInput(session, "ctrlcensorrate_S_TTE", value = input$ctrlcensorrate_N_TTE)
        updateSliderInput(session, "expcensorrate_S_TTE", value = input$expcensorrate_N_TTE)
      }
      #### sliderInput was updated
      if(NorS_TTE$NorS == "S"){
        updateNumericInput(session, "studyduration_N_TTE", value = input$studyduration_S_TTE)
        updateNumericInput(session, "enrollmentduration_N_TTE", value = input$enrollmentduration_S_TTE)
        updateNumericInput(session, "gamma_N_TTE", value = input$gamma_S_TTE)
        updateNumericInput(session, "alpha_N_TTE", value = input$alpha_S_TTE)
        updateNumericInput(session, "N_N_TTE", value = input$N_S_TTE)
        updateNumericInput(session, "ratio_N_TTE", value = input$ratio_S_TTE)
        updateNumericInput(session, "ctrllambda_N_TTE", value = input$ctrllambda_S_TTE)
        updateNumericInput(session, "explambda_N_TTE", value = input$explambda_S_TTE)
        updateNumericInput(session, "ctrlcensorrate_N_TTE", value = input$ctrlcensorrate_S_TTE)
        updateNumericInput(session, "expcensorrate_N_TTE", value = input$expcensorrate_S_TTE)
      }
      # Power Calculations
      ctrllambda = ifelse(is.null(input$ctrllambda_N_TTE), 1, input$ctrllambda_N_TTE)
      ctrlcensorrate = ifelse(is.null(input$ctrlcensorrate_N_TTE), 0, input$ctrlcensorrate_N_TTE)
      lambda1 = ctrllambda*(ctrllambda/(ctrllambda + ctrlcensorrate))
      explambda = ifelse(is.null(input$explambda_N_TTE), 1.5, input$explambda_N_TTE)
      expcensorrate = ifelse(is.null(input$expcensorrate_N_TTE), 0, input$expcensorrate_N_TTE)
      lambda2 = explambda*(explambda/(explambda + expcensorrate))
      Tstudy = ifelse(is.null(input$studyduration_N_TTE), 5, input$studyduration_N_TTE)
      Tenrollment = ifelse(is.null(input$enrollmentduration_N_TTE) | input$enrollmentduration_N_TTE == 0, 0.1^10, input$enrollmentduration_N_TTE)
      eta = 0
      ratio = ifelse(is.null(input$ratio_N_TTE), 1, input$ratio_N_TTE)
      alpha = ifelse(is.null(input$alpha_N_TTE), 0.05, input$alpha_N_TTE)
      beta =  0.2
      sided = 2
      approx = FALSE
      type = "rr" # Im not sure
      entry = ifelse(is.null(input$enrollmentdist_SE_TTE) | input$enrollmentdist_SE_TTE == "Uniform", "unif", "expo")
      gamma = ifelse(is.null(input$gamma_N_TTE), NA, input$gamma_N_TTE) # This NA could be a problem
      # Use nSurvival with input$ for all except use power = 0.8
      survival = nSurvival(lambda1 = lambda1, lambda2 = lambda2, Ts = Tstudy, Tr = Tenrollment, eta = eta, ratio = ratio, alpha = alpha, beta = beta, sided = sided, approx = approx, type = type, entry = entry, gamma = gamma)
      # Calculate ratio of events per person ($nEvents/$n)
      eventsperperson = survival$nEvents/survival$n
      # Use input#N_N_TTE and ratio to get # of events
      N = ifelse(is.null(input$N_N_TTE), 192, input$N_N_TTE)
      nevents = ceiling(N*eventsperperson) # Does this have to be an integer or can it be left un-rounded
      # Use nEvents() with # of events to get the power
      hr = lambda1/lambda2
      hr0 = 1
      tbl = FALSE
      power = round(nEvents(hr = hr, alpha = alpha, beta = NULL, ratio = ratio, sided = sided, hr0 = hr0, n = nevents, tbl = tbl), digits = 2)
      # Output
      updateSliderInput(session, "power_S_TTE", value = power)
      updateNumericInput(session, "power_N_TTE", value = power)
    }
  })
  ############################## Time to Event ##############################
  
  
} # Server Function