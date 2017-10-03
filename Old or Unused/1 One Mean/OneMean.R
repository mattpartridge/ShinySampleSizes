library(shiny); library(shinydashboard)
# One Mean App

# Needs
# For some reason, only the first conditionalPanel is working. It is possible that each of the other conditions needs to be part of the same conditionalPanel()

# Power Calculations
# Precision Calculations


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
                      selectInput(inputId = "solvefor_OM", label = "Solve For", choices = c("Power", "Sample Size", "Precision"), selected = "Power", width = "100%"))),
                conditionalPanel(
                  condition = "input.solvefor_OM == 'Sample Size'",
                  fluidRow(
                    # Inputs
                    box(
                      numericInput(inputId = "mu_N_OM", label = "Mu", min = 0, max = 2, value = 0.5, step = 0.1),
                      sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 2, value = 0.5, step = 0.1),
                      numericInput(inputId = "sd_N_OM", label = "SD", min = 0, max = NA, value = 1, step = 0.1),
                      sliderInput(inputId = "sd_S_OM", label = "", min = 0, max = 5, value = 1, step = 0.1),
                      numericInput(inputId = "nullmu_N_OM", label = "Null Hypothesis Mean", min = 0, max = 2, value = 1, step = 0.1),
                      sliderInput(inputId = "nullmu_S_OM", label = "", min = 0, max = 2, value = 1, step = 0.1),
                      numericInput(inputId = "alpha_N_OM", label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01),
                      sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
                      numericInput(inputId = "power_N_OM", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
                      sliderInput(inputId = "power_S_OM", label = "", min = 0, max = 1, value = 0.8, step = 0.05)),
                    # Outputs
                    box(
                      numericInput(inputId = "N_N_OM", label = "N", min = 0, max = NA, value = 100),
                      sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 100)))),
                conditionalPanel(
                  condition = "input.solvefor_OM == 'Power'",
                  fluidRow(
                    # Inputs
                    box(
                      numericInput(inputId = "mu_N_OM", label = "Mu", min = 0, max = 2, value = 0.5, step = 0.1),
                      sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 2, value = 0.5, step = 0.1),
                      numericInput(inputId = "sd_N_OM", label = "SD", min = 0, max = NA, value = 1, step = 0.1),
                      sliderInput(inputId = "sd_S_OM", label = "", min = 0, max = 5, value = 1, step = 0.1),
                      numericInput(inputId = "N_N_OM", label = "N", min = 0, max = NA, value = 100),
                      sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 100),
                      numericInput(inputId = "nullmu_N_OM", label = "Null Hypothesis Mean", min = 0, max = 2, value = 1, step = 0.1),
                      sliderInput(inputId = "nullmu_S_OM", label = "", min = 0, max = 2, value = 1, step = 0.1),
                      numericInput(inputId = "alpha_N_OM", label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01),
                      sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
                    # Outputs
                    box(
                      numericInput(inputId = "power_N_OM", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
                      sliderInput(inputId = "power_S_OM", label = "", min = 0, max = 1, value = 0.8, step = 0.05)))),
                conditionalPanel(
                  condition = "input.solvefor_OM == 'Precision'",
                  fluidRow(
                    # Inputs
                    box(
                      numericInput(inputId = "mu_N_OM", label = "Mu", min = 0, max = 2, value = 0.5, step = 0.1),
                      sliderInput(inputId = "mu_S_OM", label = "", min = 0, max = 2, value = 0.5, step = 0.1),
                      numericInput(inputId = "sd_N_OM", label = "SD", min = 0, max = NA, value = 1, step = 0.1),
                      sliderInput(inputId = "sd_S_OM", label = "", min = 0, max = 5, value = 1, step = 0.1),
                      numericInput(inputId = "N_N_OM", label = "N", min = 0, max = NA, value = 100),
                      sliderInput(inputId = "N_S_OM", label = "", min = 0, max = 1000, value = 100),
                      numericInput(inputId = "alpha_N_OM", label = "Significance", min = 0, max = 1, value = 0.05, step = 0.01),
                      sliderInput(inputId = "alpha_S_OM", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
                    # Outputs
                    box(
                      numericInput(inputId = "moe_N_OM", label = "+/- Precision", min = 0, max = 3, value = 1),
                      sliderInput(inputId = "moe_S_OM", label = "", min = 0, max = 3, value = 1),
                      sliderInput(inputId = "ci_S_OM", label = "Confidence Interval", min = 0, max = 5, value = c(0.402, 0.598))))))))
  ), # dashboardPage
  
  server = function(input, output, clientData, session){
    
    ##### Numeric or Slider
    NorS_OM = reactiveValues( tab = "", NorS = "" )
    observe({
      NorS_OM$tab = input$solvefor_OM
    })
    observe({
      input$mu_N_OM; input$sd_N_OM; input$nullmu_N_OM; input$alpha_N_OM; input$power_N_OM; input$N_N_OM; input$moe_N_OM
      NorS_OM$NorS = "N"
    })
    observe({
      input$mu_S_OM; input$sd_S_OM; input$nullmu_S_OM; input$alpha_S_OM; input$power_S_OM; input$N_S_OM; input$moe_S_OM
      NorS_OM$NorS = "S"
    })
    
    #    observe({
    #      ##### Sample Size
    #       if(input$solvefor_OM == "Sample Size"){
    #         #### Prep
    #         ### Numeric
    #         if(NorS_OM$NorS == "N"){
    #           # Update Slider Inputs
    #           updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM)
    #           updateSliderInput(session, "sd_S_OM", value = input$sd_N_OM)
    #           updateSliderInput(session, "nullmu_S_OM", value = input$nullmu_N_OM)
    #           updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
    #           updateSliderInput(session, "power_S_OM", value = input$power_N_OM)
    #           # Sample Size Calculations
    #           mu = input$mu_N_OM
    #           sd = input$sd_N_OM
    #           nullmu = input$nullmu_N_OM
    #           alpha = input$alpha_N_OM
    #           beta = 1 - (input$power_N_OM/100)
    #           Z.alpha.2 = qnorm(1 - alpha/2)
    #           Z.beta = qnorm(1 - beta)
    #           n = ceiling((sd*((Z.alpha.2 + Z.beta)/(mu - nullmu)))^2)
    #           # Output
    #           updateNumericInput(session, inputId = "N_N_OM", value = n)
    #           updateSliderInput(session, inputId = "N_S_OM", value = n)
    #         }
    #         ### Slider
    #         else{if(NorS_OM$NorS == "S"){
    #           # Update Numeric Inputs
    #           updateSliderInput(session, "mu_N_OM", value = input$mu_S_OM)
    #           updateSliderInput(session, "sd_N_OM", value = input$sd_S_OM)
    #           updateSliderInput(session, "nullmu_N_OM", value = input$nullmu_S_OM)
    #           updateSliderInput(session, "alpha_N_OM", value = input$alpha_S_OM)
    #           updateSliderInput(session, "power_N_OM", value = input$power_S_OM)
    #           # Sample Size Calculations
    #           mu = input$mu_S_OM
    #           sd = input$sd_S_OM
    #           nullmu = input$nullmu_S_OM
    #           alpha = input$alpha_S_OM
    #           beta = 1 - (input$power_S_OM/100)
    #           Z.alpha.2 = qnorm(1 - alpha/2)
    #           Z.beta = qnorm(1 - beta)
    #           n = ceiling((sd*((Z.alpha.2 + Z.beta)/(mu - nullmu)))^2)
    #           # Output
    #           updateNumericInput(session, inputId = "N_N_OM", value = n)
    #           updateSliderInput(session, inputId = "N_S_OM", value = n)
    #         }}
    #       }
    #     })
    
    observe({
      if(NorS_OM$tab == "Power"){
        #### Prep
        ### Numeric
        if(NorS_OM$NorS == "N"){
          # Update Slider Inputs
          updateSliderInput(session, "mu_S_OM", value = input$mu_N_OM)
          updateSliderInput(session, "sd_S_OM", value = input$sd_N_OM)
          updateSliderInput(session, "nullmu_S_OM", value = input$nullmu_N_OM)
          updateSliderInput(session, "alpha_S_OM", value = input$alpha_N_OM)
          updateSliderInput(session, "N_S_OM", value = input$N_N_OM)
          # Sample Size Calculations
#           mu = input$mu_N_OM
#           sd = input$sd_N_OM
#           nullmu = input$nullmu_N_OM
#           alpha = input$alpha_N_OM
#           beta = 1 - (input$power_N_OM/100)
#           Z.alpha.2 = qnorm(1 - alpha/2)
#           Z.beta = qnorm(1 - beta)
#           n = ceiling((sd*((Z.alpha.2 + Z.beta)/(mu - nullmu)))^2)
#           # Output
#           updateNumericInput(session, inputId = "N_N_OM", value = n)
#           updateSliderInput(session, inputId = "N_S_OM", value = n)
        }
        ### Slider
        else{if(NorS_OM$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "mu_N_OM", value = input$mu_S_OM)
          updateSliderInput(session, "sd_N_OM", value = input$sd_S_OM)
          updateSliderInput(session, "nullmu_N_OM", value = input$nullmu_S_OM)
          updateSliderInput(session, "alpha_N_OM", value = input$alpha_S_OM)
          updateSliderInput(session, "N_N_OM", value = input$N_S_OM)
          # Sample Size Calculations
#           mu = input$mu_S_OM
#           sd = input$sd_S_OM
#           nullmu = input$nullmu_S_OM
#           alpha = input$alpha_S_OM
#           Z.alpha.2 = qnorm(1 - alpha/2)
#           N = input$N_S_OM
#           power = round((1 - (1 - pnorm((mu - nullmu)*sqrt(N/sd) - Z.alpha.2)))*100, digits = 2)
#           # Output
#           updateNumericInput(session, inputId = "power_N_OM", value = beta)
#           updateSliderInput(session, inputId = "power_S_OM", value = n)
        }}
      }
    })
    
    
  }
) # shinyApp