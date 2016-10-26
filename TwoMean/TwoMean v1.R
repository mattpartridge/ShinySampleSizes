###############################
###############################
###      Matt Partridge     ###
### University of Minnesota ###
###    MS Plan B Project    ###
###    Shiny Sample Sizes   ###
###         Two Means       ###
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
        menuItem("Two Sample Means", tabName = "TM"))),
    dashboardBody(
      tabItems(
        
        #######################################
        ############## Two Means ##############
        #######################################
        tabItem(tabName = "TM",
                fluidRow(
                  box(width = 12,
                      selectInput(inputId = "solvefor_TM", label = "Solve For", choices = c("Sample Size", "Power"), selected = "Sample Size", width = "100%")),
                  uiOutput("TM")))))
  ), # dashboardPage
  
  
  server = function(input, output, clientData, session){
    
    ##### User Interface
    output$TM = renderUI({
      ### Sample Size
      if(input$solvefor_TM == "Sample Size"){
        list(
          # Inputs
          box(
            numericInput(inputId = "mu1_N_TM", label = "Mean One", min = 0, max = 10, value = 1.0, step = 0.01),
            sliderInput(inputId = "mu1_S_TM", label = "", min = 0, max = 10, value = 1.0, step = 0.01),
            numericInput(inputId = "N1_N_TM", label = "Sample Size One", min = 0, max = 10000, value = 100, step = 1),
            sliderInput(inputId = "N1_S_TM", label = "", min = 0, max = 10000, value = 100, step = 1),
            numericInput(inputId = "mu2_N_TM", label = "Mean Two", min = 0, max = 10, value = 1.3, step = 0.01),
            sliderInput(inputId = "mu2_S_TM", label = "", min = 0, max = 10, value = 1.3, step = 0.01),
            numericInput(inputId = "alpha_N_TM", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_TM", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
            numericInput(inputId = "power_N_TM", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
            sliderInput(inputId = "power_S_TM", label = "", min = 0, max = 1, value = 0.8, step = 0.05)),
          # Outputs
          box(
            numericInput(inputId = "N2_N_TM", label = "Sample Size Two", min = 0, max = 10000, value = 695, step = 1),
            sliderInput(inputId = "N2_S_TM", label = "", min = 0, max = 10000, value = 695, step = 1)))
      }
      ### Power
      else{if(input$solvefor_TM == "Power"){
        list(
          # Inputs
          box(
            numericInput(inputId = "mu1_N_TM", label = "Mean One", min = 0, max = 10, value = 1.0, step = 0.01),
            sliderInput(inputId = "mu1_S_TM", label = "", min = 0, max = 10, value = 1.0, step = 0.01),
            numericInput(inputId = "N1_N_TM", label = "Sample Size One", min = 0, max = 10000, value = 100, step = 1),
            sliderInput(inputId = "N1_S_TM", label = "", min = 0, max = 10000, value = 100, step = 1),
            numericInput(inputId = "mu2_N_TM", label = "Mean Two", min = 0, max = 10, value = 1.3, step = 0.01),
            sliderInput(inputId = "mu2_S_TM", label = "", min = 0, max = 10, value = 1.3, step = 0.01),
            numericInput(inputId = "N2_N_TM", label = "Sample Size Two", min = 0, max = 10000, value = 695, step = 1),
            sliderInput(inputId = "N2_S_TM", label = "", min = 0, max = 10000, value = 695, step = 1),
            numericInput(inputId = "alpha_N_TM", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_TM", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
          # Outputs
          box(
            numericInput(inputId = "power_N_TM", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
            sliderInput(inputId = "power_S_TM", label = "", min = 0, max = 1, value = 0.8, step = 0.05))
        )
      }}
    })
    
    ##### Numeric or Slider
    NorS_TM = reactiveValues( NorS = "" )
    observe({
      input$mu1_N_TM; input$N1_N_TM; input$mu2_N_TM; input$N2_N_TM; input$alpha_N_TM; input$power_N_TM
      NorS_TM$NorS = "N"
    })
    observe({
      input$mu1_S_TM; input$N1_S_TM; input$mu2_S_TM; input$N2_S_TM; input$alpha_S_TM; input$power_S_TM
      NorS_TM$NorS = "S"
    })
    
    # Calculations
    observe({
      ##### Sample Size
      if(input$solvefor_TM == "Sample Size"){
        #### numericInput was updated
        if(NorS_TM$NorS == "N"){
          ### Update sliderInputs
          updateSliderInput(session, "mu1_S_TM", value = input$mu1_N_TM)
          updateSliderInput(session, "N1_S_TM", value = input$N1_N_TM)
          updateSliderInput(session, "mu2_S_TM", value = input$mu2_N_TM)
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
        n2 = round(pwr.t2n.test(n1, n2, d, sig.level, power, alternative)$n2, digits = 0)
        # Output
        updateNumericInput(session, inputId = "N2_N_TM", value = n2)
        updateSliderInput(session, inputId = "N2_S_TM", value = n2)
      }
      
      ##### Power
      if(input$solvefor_TM == "Power"){
        #### Prep
        ### Numeric
        if(NorS_TM$NorS == "N"){
          # Update Slider Inputs
          updateSliderInput(session, "mu1_S_TM", value = input$mu1_N_TM)
          updateSliderInput(session, "N1_S_TM", value = input$N1_N_TM)
          updateSliderInput(session, "mu2_S_TM", value = input$mu2_N_TM)
          updateSliderInput(session, "N2_S_TM", value = input$N2_N_TM)
          updateSliderInput(session, "alpha_S_TM", value = input$alpha_N_TM)
        }
        ### Slider
        if(NorS_TM$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "mu1_N_TM", value = input$mu1_S_TM)
          updateSliderInput(session, "N1_S_TM", value = input$N1_N_TM)
          updateSliderInput(session, "mu2_N_TM", value = input$mu2_S_TM)
          updateSliderInput(session, "N2_S_TM", value = input$N2_N_TM)
          updateSliderInput(session, "alpha_N_TM", value = input$alpha_S_TM)
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
        power = round(pwr.t2n.test(n1, n2, d, sig.level, power, alternative)$power, digits = 2)
        # Output
        updateNumericInput(session, inputId = "power_N_TM", value = power)
        updateSliderInput(session, inputId = "power_S_TM", value = power)
      }
    }) # Observe
  } # server
) # shinyApp