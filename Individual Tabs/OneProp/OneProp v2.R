###############################
###############################
###      Matt Partridge     ###
### University of Minnesota ###
###    MS Plan B Project    ###
###    Shiny Sample Sizes   ###
###      One Proportion     ###
###############################
###############################

library(shiny); library(shinydashboard); library(pwr)


### Needs
# Need to find reputable references about the calculations. This whole tab needs to validate the equations.
# Bounds on sliders (and numerics) still needs to be able to change fluidly.

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Sample Size Calculations"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("One Sample Proportion", tabName = "OP"))),
    dashboardBody(
      tabItems(
        
        ############################################
        ############## One Proportion ##############
        ############################################
        tabItem(tabName = "OP",
                fluidRow(
                  box(width = 12,
                      selectInput(inputId = "solvefor_OP", label = "Solve For", choices = c("Sample Size", "Power", "Precision"), selected = "Sample Size", width = "100%")),
                  uiOutput("OP")))))
  ), # dashboardPage
  
  
  server = function(input, output, clientData, session){
    
    ##### User Interface
    output$OP = renderUI({
      ### Sample Size
        list(
          # Inputs
          box(
            numericInput(inputId = "p_N_OP", label = "Proportion", min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput(inputId = "p_S_OP", label = "", min = 0, max = 1, value = 0.5, step = 0.01),
            if(input$solvefor_OP %in% c("Sample Size", "Power")){
              list(
                numericInput(inputId = "nullp_N_OP", label = "Hypothesis Proportion", min = 0, max = 1, value = 0.52, step = 0.01),
                sliderInput(inputId = "nullp_S_OP", label = "", min = 0, max = 1, value = 0.52, step = 0.01))},
            if(input$solvefor_OP %in% c("Power", "Precision")){
              list(
                numericInput(inputId = "N_N_OP", label = "Sample Size", min = 0, max = 5000, value = 4903, step = 1),
                sliderInput(inputId = "N_S_OP", label = "", min = 0, max = 5000, value = 4903, step = 1))},
            numericInput(inputId = "alpha_N_OP", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_OP", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
            if(input$solvefor_OP == "Sample Size"){
              list(
                numericInput(inputId = "power_N_OP", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
                sliderInput(inputId = "power_S_OP", label = "", min = 0, max = 1, value = 0.8, step = 0.05))}),
          # Output
          box(
            if(input$solvefor_OP == "Sample Size"){
              list(
                numericInput(inputId = "N_N_OP", label = "Sample Size", min = 0, max = NA, value = 4903, step = 1),
                sliderInput(inputId = "N_S_OP", label = "", min = 0, max = 10000, value = 4903, step = 1))},
            if(input$solvefor_OP == "Power"){
              list(
                numericInput(inputId = "power_N_OP", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
                sliderInput(inputId = "power_S_OP", label = "", min = 0, max = 1, value = 0.8, step = 0.05))},
            if(input$solvefor_OP == "Precision"){
              list(
                numericInput(inputId = "moe_N_OP", label = "+/- Precision", min = 0, max = 1, step = 0.01, value = 0.01),
                sliderInput(inputId = "moe_S_OP", label = "", min = 0, max = 1, step = 0.01, value = 0.01),
                sliderInput(inputId = "ci_S_OP", label = "Confidence Interval", min = 0, max = 1, step = 0.1, value = c(0.49, 0.51)))}))
    })
    
    ##### Numeric or Slider
    NorS_OP = reactiveValues( NorS = "" )
    observe({
      input$p_N_OP; input$nullp_N_OP; input$alpha_N_OP; input$power_N_OP; input$N_N_OP; input$moe_N_OP
      NorS_OP$NorS = "N"
    })
    observe({
      input$p_S_OP; input$nullp_S_OP; input$alpha_S_OP; input$power_S_OP; input$N_S_OP; input$moe_S_OP
      NorS_OP$NorS = "S"
    })
    
    # Calculations
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
        updateSliderInput(session, inputId = "N_S_OP", value = n)
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
          updateSliderInput(session, "N_S_OP", value = input$N_N_OP)
        }
        ### Slider
        if(NorS_OP$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "p_N_OP", value = input$p_S_OP)
          updateSliderInput(session, "nullp_N_OP", value = input$nullp_S_OP)
          updateSliderInput(session, "alpha_N_OP", value = input$alpha_S_OP)
          updateSliderInput(session, "N_N_OP", value = input$N_S_OP)
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
          updateSliderInput(session, "N_S_OP", value = input$N_N_OP)
          updateSliderInput(session, "alpha_S_OP", value = input$alpha_N_OP)
        }
        ### Slider
        if(NorS_OP$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "p_N_OP", value = input$p_S_OP)
          updateSliderInput(session, "N_N_OP", value = input$N_S_OP)
          updateSliderInput(session, "alpha_N_OP", value = input$alpha_S_OP)
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
        updateSliderInput(session, "moe_S_OP", value = moe)
        updateSliderInput(session, "ci_S_OP", value = c(lo, hi))
      }
    }) # Observe
  } # Server
) # shinyApp