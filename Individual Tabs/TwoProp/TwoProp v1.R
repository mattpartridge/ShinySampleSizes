###############################
###############################
###      Matt Partridge     ###
### University of Minnesota ###
###    MS Plan B Project    ###
###    Shiny Sample Sizes   ###
###     Two Proportions     ###
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
        menuItem("Two Sample Proportions", tabName = "TP"))),
    dashboardBody(
      tabItems(
        
        #############################################
        ############## Two Proportions ##############
        #############################################
        tabItem(tabName = "TP",
                fluidRow(
                  box(width = 12,
                      selectInput(inputId = "solvefor_TP", label = "Solve For", choices = c("Sample Size", "Power"), selected = "Sample Size", width = "100%")),
                  uiOutput("TP")))))
  ), # dashboardPage
  
  
  server = function(input, output, clientData, session){
    
    ##### User Interface
    output$TP = renderUI({
      ### Sample Size
      if(input$solvefor_TP == "Sample Size"){
        list(
          # Inputs
          box(
            numericInput(inputId = "p1_N_TP", label = "Proportion One", min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput(inputId = "p1_S_TP", label = "", min = 0, max = 1, value = 0.5, step = 0.01),
            numericInput(inputId = "N1_N_TP", label = "Sample Size One", min = 0, max = 10000, value = 100, step = 1),
            sliderInput(inputId = "N1_S_TP", label = "", min = 0, max = 10000, value = 100, step = 1),
            numericInput(inputId = "p2_N_TP", label = "Proportion Two", min = 0, max = 1, value = 0.7, step = 0.01),
            sliderInput(inputId = "p2_S_TP", label = "", min = 0, max = 1, value = 0.7, step = 0.01),
            numericInput(inputId = "alpha_N_TP", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_TP", label = "", min = 0, max = 1, value = 0.05, step = 0.01),
            numericInput(inputId = "power_N_TP", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
            sliderInput(inputId = "power_S_TP", label = "", min = 0, max = 1, value = 0.8, step = 0.05)),
          # Outputs
          box(
            numericInput(inputId = "N2_N_TP", label = "Sample Size Two", min = 0, max = 10000, value = 695, step = 1),
            sliderInput(inputId = "N2_S_TP", label = "", min = 0, max = 10000, value = 695, step = 1)))
      }
      ### Power
      else{if(input$solvefor_TP == "Power"){
        list(
          # Inputs
          box(
            numericInput(inputId = "p1_N_TP", label = "Proportion One", min = 0, max = 1, value = 0.5, step = 0.01),
            sliderInput(inputId = "p1_S_TP", label = "", min = 0, max = 1, value = 0.5, step = 0.01),
            numericInput(inputId = "N1_N_TP", label = "Sample Size One", min = 0, max = 10000, value = 100, step = 1),
            sliderInput(inputId = "N1_S_TP", label = "", min = 0, max = 10000, value = 100, step = 1),
            numericInput(inputId = "p2_N_TP", label = "Proportion Two", min = 0, max = 1, value = 0.7, step = 0.01),
            sliderInput(inputId = "p2_S_TP", label = "", min = 0, max = 1, value = 0.7, step = 0.01),
            numericInput(inputId = "N2_N_TP", label = "Sample Size Two", min = 0, max = 10000, value = 695, step = 1),
            sliderInput(inputId = "N2_S_TP", label = "", min = 0, max = 10000, value = 695, step = 1),
            numericInput(inputId = "alpha_N_TP", label = "Significance Level", min = 0, max = 1, value = 0.05, step = 0.01),
            sliderInput(inputId = "alpha_S_TP", label = "", min = 0, max = 1, value = 0.05, step = 0.01)),
          # Outputs
          box(
            numericInput(inputId = "power_N_TP", label = "Power", min = 0, max = 1, value = 0.8, step = 0.05),
            sliderInput(inputId = "power_S_TP", label = "", min = 0, max = 1, value = 0.8, step = 0.05)))
      }}
    })
    
    ##### Numeric or Slider
    NorS_TP = reactiveValues( NorS = "" )
    observe({
      input$p1_N_TP; input$N1_N_TP; input$p2_N_TP; input$N2_N_TP; input$alpha_N_TP; input$power_N_TP
      NorS_TP$NorS = "N"
    })
    observe({
      input$p1_S_TP; input$N1_S_TP; input$p2_S_TP; input$N2_S_TP; input$alpha_S_TP; input$power_S_TP
      NorS_TP$NorS = "S"
    })
    
    # Calculations
    observe({
      ##### Sample Size
      if(input$solvefor_TP == "Sample Size"){
        #### numericInput was updated
        if(NorS_TP$NorS == "N"){
          ### Update sliderInputs
          updateSliderInput(session, "p1_S_TP", value = input$p1_N_TP)
          updateSliderInput(session, "N1_S_TP", value = input$N1_N_TP)
          updateSliderInput(session, "p2_S_TP", value = input$p2_N_TP)
          updateSliderInput(session, "alpha_S_TP", value = input$alpha_N_TP)
          updateSliderInput(session, "power_S_TP", value = input$power_N_TP)
        }
        #### sliderInput was updated
        if(NorS_TP$NorS == "S"){
          ### Update numericInputs
          updateNumericInput(session, "p1_N_TP", value = input$p1_S_TP)
          updateNumericInput(session, "N1_N_TP", value = input$N1_S_TP)
          updateNumericInput(session, "p2_N_TP", value = input$p2_S_TP)
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
        updateSliderInput(session, inputId = "N2_S_TP", value = n2)
      }
      
      ##### Power
      if(input$solvefor_TP == "Power"){
        #### Prep
        ### Numeric
        if(NorS_TP$NorS == "N"){
          # Update Slider Inputs
          updateSliderInput(session, "p1_S_TP", value = input$p1_N_TP)
          updateSliderInput(session, "N1_S_TP", value = input$N1_N_TP)
          updateSliderInput(session, "p2_S_TP", value = input$p2_N_TP)
          updateSliderInput(session, "N2_S_TP", value = input$N2_N_TP)
          updateSliderInput(session, "alpha_S_TP", value = input$alpha_N_TP)
        }
        ### Slider
        if(NorS_TP$NorS == "S"){
          # Update Numeric Inputs
          updateSliderInput(session, "p1_N_TP", value = input$p1_S_TP)
          updateSliderInput(session, "N1_S_TP", value = input$N1_N_TP)
          updateSliderInput(session, "p2_N_TP", value = input$p2_S_TP)
          updateSliderInput(session, "N2_S_TP", value = input$N2_N_TP)
          updateSliderInput(session, "alpha_N_TP", value = input$alpha_S_TP)
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
    }) # Observe
  } # server
) # shinyApp