###############################
###############################
###      Matt Partridge     ###
### University of Minnesota ###
###    MS Plan B Project    ###
###    Shiny Sample Sizes   ###
###         One Mean        ###
###############################
###############################

library(shiny); library(shinydashboard); library(pwr)


### Needs
# Need to find reputable references about the calculations. This whole tab needs to validate the equations.

# Bounds on sliders (and numerics) still needs to be able to change fluidly.
# The "power" variable is starting out at NULL. Need to fix what is going on. There is currently a temporary fix.

shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Sample Size Calculations"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("One Sample Mean", tabName = "OM"))),
    dashboardBody(
      tabItems(
        
        ######################################
        ############## One Mean ##############
        ######################################
        tabItem(tabName = "OM",
                fluidRow(
                  box(width = 12,
                      selectInput(inputId = "solvefor_OM", label = "Solve For", choices = c("Sample Size", "Power", "Precision"), selected = "Sample Size", width = "100%")),
                  uiOutput("OM")))))
  ), # dashboardPage
  
  
  server = function(input, output, clientData, session){
    
    ##### User Interface
    output$OM = renderUI({
      ### Sample Size
      if(input$solvefor_OM == "Sample Size"){
        list(
          # Inputs
          box(
            numericInput(inputId = "mu_N_OM", label = "Mean", min = 0, max = 2, value = 0.77, step = 0.01),
            sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 2, value = 0.77, step = 0.01),
            numericInput(inputId = "nullmu_N_OM", label = "Hypothesis Mean", min = 0, max = 2, value = 1, step = 0.01),
            sliderInput(inputId = "nullmu_S_OM", label = "", min = 0, max = 2, value = 1, step = 0.01),
            numericInput(inputId = "alpha_N_OM", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
            numericInput(inputId = "power_N_OM", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
            sliderInput(inputId = "power_S_OM", label = "", min = 0, max = 1, value = 0.8, step = 0.05)),
          # Outputs
          box(
            numericInput(inputId = "N_N_OM", label = "Sample Size", min = 0, max = NA, value = 150, step = 1),
            sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 150, step = 1)))
      }
      ### Power
      else{if(input$solvefor_OM == "Power"){
        list(
          # Inputs
          box(
            numericInput(inputId = "mu_N_OM", label = "Mean", min = 0, max = 2, value = 0.77, step = 0.01),
            sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 2, value = 0.77, step = 0.01),
            numericInput(inputId = "nullmu_N_OM", label = "Hypothesis Mean", min = 0, max = 2, value = 1, step = 0.01),
            sliderInput(inputId = "nullmu_S_OM", label = "", min = 0, max = 2, value = 1, step = 0.01),
            numericInput(inputId = "N_N_OM", label = "Sample Size", min = 0, max = NA, value = 150, step = 1),
            sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 150, step = 1),
            numericInput(inputId = "alpha_N_OM", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
          # Outputs
          box(
            numericInput(inputId = "power_N_OM", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
            sliderInput(inputId = "power_S_OM", label = "", min = 0, max = 1, value = 0.8, step = 0.05)))
      }
        ### Precision
        else{if(input$solvefor_OM == "Precision"){
          list(
            # Inputs
            box(
              numericInput(inputId = "mu_N_OM", label = "Mean", min = 0, max = 10, value = 0.77, step = 0.01),
              sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 10, value = 0.77, step = 0.01),
              numericInput(inputId = "sd_N_OM", label = "SD", min = 0, max = 5, value = 1, step = 0.1),
              sliderInput(inputId = "sd_S_OM", label = "", min = 0, max = 5, value = 1, step = 0.1),
              numericInput(inputId = "N_N_OM", label = "Sample Size", min = 0, max = 1000, value = 150),
              sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 150),
              numericInput(inputId = "alpha_N_OM", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
              sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
            # Outputs
            box(
              numericInput(inputId = "moe_N_OM", label = "+/- Precision", min = 0, max = 3, step = 0.1, value = 0.2),
              sliderInput(inputId = "moe_S_OM", label = "", min = 0, max = 3, step = 0.1, value = 0.2),
              sliderInput(inputId = "ci_S_OM", label = "Confidence Interval", min = 0, max = 3, step = 0.1, value = c(0.3, 0.7))))
        }}}
    })
    
    ##### Numeric or Slider
    NorS_OM = reactiveValues( NorS = "" )
    observe({
      input$mu_N_OM; input$nullmu_N_OM; input$alpha_N_OM; input$power_N_OM; input$N_N_OM; input$moe_N_OM
      NorS_OM$NorS = "N"
    })
    observe({
      input$mu_S_OM; input$nullmu_S_OM; input$alpha_S_OM; input$power_S_OM; input$N_S_OM; input$moe_S_OM
      NorS_OM$NorS = "S"
    })
    
    # Calculations
    observe({
      ##### Sample Size
      if(input$solvefor_OM == "Sample Size"){
        #### numericInput was updated
        if(NorS_OM$NorS == "N"){
          ### Update sliderInputs
          updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM)
          updateSliderInput(session, "nullmu_S_OM", value = input$nullmu_N_OM)
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
        d = input$mu_N_OM - input$nullmu_N_OM
        sig.level = input$alpha_N_OM
        power = input$power_N_OM
        type = "one.sample"
        alternative = "two.sided"
        if(is.null(power)){ n = 100 }
        else{ n = round(pwr.t.test(n, d, sig.level, power, type, alternative)$n, digits = 0) }
        # Output
        updateNumericInput(session, inputId = "N_N_OM", value = n)
        updateSliderInput(session, inputId = "N_S_OM", value = n)
      }
      
      ##### Power
      if(input$solvefor_OM == "Power"){
        #### Prep
        ### Numeric
        if(NorS_OM$NorS == "N"){
          # Update Slider Inputs
          updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM)
          updateSliderInput(session, "nullmu_S_OM", value = input$nullmu_N_OM)
          updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
          updateSliderInput(session, "N_S_OM", value = input$N_N_OM)
        }
        ### Slider
        if(NorS_OM$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "mu_N_OM", value = input$mu_S_OM)
          updateSliderInput(session, "nullmu_N_OM", value = input$nullmu_S_OM)
          updateSliderInput(session, "alpha_N_OM", value = input$alpha_S_OM)
          updateSliderInput(session, "N_N_OM", value = input$N_S_OM)
        }
        # Power Calculations
        n = input$N_N_OM
        d = input$mu_N_OM - input$nullmu_N_OM
        sig.level = input$alpha_N_OM
        power = NULL
        type = "one.sample"
        alternative = "two.sided"
        power = round(pwr.t.test(n, d, sig.level, power, type, alternative)$power, digits = 2)
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
          updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM)
          updateSliderInput(session, "sd_S_OM", value = input$sd_N_OM)
          updateSliderInput(session, "N_S_OM", value = input$N_N_OM)
          updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
        }
        ### Slider
        if(NorS_OM$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "mu_N_OM", value = input$mu_S_OM)
          updateSliderInput(session, "sd_N_OM", value = input$sd_S_OM)
          updateSliderInput(session, "N_N_OM", value = input$N_S_OM)
          updateSliderInput(session, "alpha_N_OM", value = input$alpha_S_OM)
        }
        # Precision Calculations
        mu = input$mu_N_OM
        sd = input$sd_N_OM
        n = input$N_N_OM
        alpha = input$alpha_N_OM
        Z.alpha.2 = qnorm(1 - alpha/2)
        se = sd/sqrt(n)
        moe = round(Z.alpha.2*se, digits = 2)
        lo = round(mu - moe, digits = 2)
        hi = round(mu + moe, digits = 2)
        # Output
        updateNumericInput(session, "moe_N_OM", value = moe)
        updateSliderInput(session, "moe_S_OM", value = moe)
        updateSliderInput(session, "ci_S_OM", value = c(lo, hi))
      }
    }) # Observe
    
    
  }
) # shinyApp