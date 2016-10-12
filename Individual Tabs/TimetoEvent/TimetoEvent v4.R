###############################
###############################
###      Matt Partridge     ###
### University of Minnesota ###
###    MS Plan B Project    ###
###    Shiny Sample Sizes   ###
###      Time to Event      ###
###############################
###############################

library(shiny); library(shinydashboard); library(gsDesign)


### Needs
# Need to find reputable references about the calculations. This whole tab needs to validate the equations.

# Bounds on sliders (and numerics) still needs to be able to change fluidly.


shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Sample Size Calculations"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Time to Event", tabName = "TTE"))),
    dashboardBody(
      tabItems(
        
        ###########################################
        ############## Time to Event ##############
        ###########################################
        tabItem(tabName = "TTE",
                fluidRow(
                  box(width = 12,
                      selectInput(inputId = "solvefor_TTE", label = "Solve For", choices = c("Sample Size", "Power"), selected = "Sample Size", width = "100%")),
                  # Study Information
                  conditionalPanel("input.solvefor_TTE == 'Sample Size'",
                                   box(width = 4,
                                       selectInput(inputId = "enrollment_SE_TTE",
                                                   label = "Enrollment Schedule",
                                                   choices = c("All at Once", "Over a Period", "Continuous Throughout"),
                                                   selected = "All at Once"),
                                       conditionalPanel("input.enrollment_SE_TTE == 'Over a Period' | input.enrollment_SE_TTE == 'Continuous Throughout'",
                                                        selectInput(inputId = "enrollmentdist_SE_TTE",
                                                                    label = "Distribution of Enrollment",
                                                                    choices = c("Uniform", "Exponential Decay"),
                                                                    selected = "Uniform"),
                                                        conditionalPanel("input.enrollmentdist_SE_TTE == 'Exponential Decay'",
                                                                         numericInput(inputId = "gamma_N_TTE",
                                                                                      label = "Decay Rate",
                                                                                      value = 0.5,
                                                                                      min = 0,
                                                                                      max = 1,
                                                                                      step = 0.01),
                                                                         sliderInput(inputId = "gamma_S_TTE",
                                                                                     label = "",
                                                                                     value = 0.5,
                                                                                     min = 0,
                                                                                     max = 1,
                                                                                     step = 0.01)
                                                        )
                                       ),
                                       numericInput(inputId = "studyduration_N_TTE",
                                                    label = "Study Duration (units of time)",
                                                    value = 5,
                                                    min = 0,
                                                    max = 10,
                                                    step = 0.25),
                                       sliderInput(inputId = "studyduration_S_TTE",
                                                   label = "",
                                                   value = 5,
                                                   min = 0,
                                                   max = 10,
                                                   step = 0.25),
                                       conditionalPanel("input.enrollment_SE_TTE == 'Over a Period' | input.enrollment_SE_TTE == 'Continuous Throughout'",
                                                        numericInput(inputId = "enrollmentduration_N_TTE",
                                                                     label = "Enrollment Duration",
                                                                     value = 2.5,
                                                                     min = 0,
                                                                     max = 10,
                                                                     step = 0.25),
                                                        sliderInput(inputId = "enrollmentduration_S_TTE",
                                                                    label = "",
                                                                    value = 2.5,
                                                                    min = 0,
                                                                    max = 10,
                                                                    step = 0.25)
                                       )
                                   )
                  ),
                  # Calculation Information
                  uiOutput("CalculationInfo"),
                  uiOutput("Outputs")
                  ))))
  ), # dashboardPage
  
  
  server = function(input, output, clientData, session){
    
    ##### RenderUIs
    output$CalculationInfo = renderUI({
      box(width = ifelse(input$solvefor_TTE == "Sample Size", 4, 6),
          numericInput(inputId = "alpha_N_TTE",
                       label = "Significance Level",
                       value = 0.05,
                       min = 0,
                       max = 1,
                       step = 0.01),
          sliderInput(inputId = "alpha_S_TTE",
                      label = "",
                      value = 0.05,
                      min = 0,
                      max = 1,
                      step = 0.01),
          if(input$solvefor_TTE == "Sample Size"){
            list(
              numericInput(inputId = "power_N_TTE",
                           label = "Power",
                           value = 0.8,
                           min = 0,
                           max = 1,
                           step = 0.01),
              sliderInput(inputId = "power_S_TTE",
                          label = "",
                          value = 0.8,
                          min = 0,
                          max = 1,
                          step = 0.01)
            )
          },
          if(input$solvefor_TTE == "Power"){
            list(
              numericInput(inputId = "Npergroup_N_TTE",
                           label = "Sample Size per Group",
                           value = 500,
                           min = 0,
                           max = 10000,
                           step = 0.01),
              sliderInput(inputId = "Npergroup_TTE",
                          label = "",
                          value = 500,
                          min = 0,
                          max = 10000,
                          step = 0.01),
              numericInput(inputId = "totalN_N_TTE",
                           label = "Total Sample Size",
                           value = 1000,
                           min = 0,
                           max = 10000,
                           step = 0.01),
              sliderInput(inputId = "totalN_S_TTE",
                          label = "",
                          value = 1000,
                          min = 0,
                          max = 10000,
                          step = 0.01)
            )
          },
          numericInput(inputId = "ctrllambda_N_TTE",
                       label = "Control Group's Event Rate per Unit of Time",
                       value = 1,
                       min = 0,
                       max = 10,
                       step = 0.01),
          sliderInput(inputId = "ctrllambda_S_TTE",
                      label = "",
                      value = 1,
                      min = 0,
                      max = 10,
                      step = 0.01),
          numericInput(inputId = "explambda_N_TTE", 
                       label = "Experimental Group's Event Rate per Unit of Time",
                       value = 1.5,
                       min = 0,
                       max = 10,
                       step = 0.01),
          sliderInput(inputId = "explambda_S_TTE",
                      label = "",
                      value = 1.5,
                      min = 0,
                      max = 10,
                      step = 0.01),
          if(input$solvefor_TTE == "Sample Size"){
            list(
              numericInput(inputId = "ctrlcensorrate_N_TTE",
                           label = "Control Group's Censoring Rate",
                           value = 0,
                           min = 0,
                           max = 1,
                           step = 0.01),
              sliderInput(inputId = "ctrlcensorrate_S_TTE",
                          label = "",
                          value = 0,
                          min = 0,
                          max = 1,
                          step = 0.01),
              numericInput(inputId = "expcensorrate_N_TTE",
                           label = "Experimental Group's Censoring Rate",
                           value = 0,
                           min = 0,
                           max = 1,
                           step = 0.01),
              sliderInput(inputId = "expcensorrate_S_TTE",
                          label = "",
                          value = 0,
                          min = 0, 
                          max = 1,
                          step = 0.01)
            )
          }
      )
    })
    output$Outputs = renderUI({
      box(width = ifelse(input$solvefor_TTE == "Sample Size", 4, 6),
          if(input$solvefor_TTE == "Sample Size"){
            list(
              numericInput(inputId = "Npergroup_N_TTE",
                           label = "Sample Size per Group",
                           value = 500,
                           min = 0,
                           max = 10000,
                           step = 0.01),
              sliderInput(inputId = "Npergroup_TTE",
                          label = "",
                          value = 500,
                          min = 0,
                          max = 10000,
                          step = 0.01),
              numericInput(inputId = "totalN_N_TTE", 
                           label = "Total Sample Size",
                           value = 1000,
                           min = 0,
                           max = 10000,
                           step = 0.01),
              sliderInput(inputId = "totalN_S_TTE",
                          label = "",
                          value = 1000,
                          min = 0,
                          max = 10000,
                          step = 0.01)
            )
          },
          if(input$solvefor_TTE == "Power"){
            list(
              numericInput(inputId = "power_N_TTE",
                           label = "Power",
                           value = 0.8,
                           min = 0,
                           max = 1,
                           step = 0.01),
              sliderInput(inputId = "power_S_TTE",
                          label = "",
                          value = 0.8,
                          min = 0,
                          max = 1,
                          step = 0.01)
            )
          }
      )
    })
    
    ##### Numeric or Slider
    NorS_TTE = reactiveValues( NorS = "" )
    observe({
      input$studyduration_N_TTE; input$enrollmentduration_N_TTE; input$gamma_N_TTE; input$alpha_N_TTE; input$power_N_TTE; input$ctrllambda_N_TTE; input$explambda_N_TTE; input$ctrlcensorrate_N_TTE; input$expcensorrate_N_TTE; input$Npergroup_N_TTE; input$totalN_N_TTE
      NorS_TTE$NorS = "N"
    })
    observe({
      input$studyduration_S_TTE; input$enrollmentduration_S_TTE; input$gamma_S_TTE; input$alpha_S_TTE; input$power_S_TTE; input$ctrllambda_S_TTE; input$explambda_S_TTE; input$ctrlcensorrate_S_TTE; input$expcensorrate_S_TTE; input$Npergroup_S_TTE; input$totalN_S_TTE
      NorS_TTE$NorS = "S"
    })
    
    # Calculations
    observe({
      ##### Sample Size
      if(input$solvefor_TTE == "Sample Size"){
        #### numericInput was updated
        if(NorS_TTE$NorS == "N"){
          updateSliderInput(session, "studyduration_S_TTE", value = input$studyduration_N_TTE)
          updateSliderInput(session, "enrollmentduration_S_TTE", value = input$enrollmentduration_N_TTE)
          updateSliderInput(session, "gamma_S_TTE", value = input$gamma_N_TTE)
          updateSliderInput(session, "alpha_S_TTE", value = input$alpha_N_TTE)
          updateSliderInput(session, "power_S_TTE", value = input$power_N_TTE)
          updateSliderInput(session, "ctrllambda_S_TTE", value = input$ctrllambda_N_TTE)
          updateSliderInput(session, "explambda_S_TTE", value = input$explambda_N_TTE)
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
          updateNumericInput(session, "ctrllambda_N_TTE", value = input$ctrllambda_S_TTE)
          updateNumericInput(session, "explambda_N_TTE", value = input$explambda_S_TTE)
          updateNumericInput(session, "ctrlcensorrate_N_TTE", value = input$ctrlcensorrate_S_TTE)
          updateNumericInput(session, "expcensorrate_N_TTE", value = input$expcensorrate_S_TTE)
        }
        # Sample Size Calculations
        ctrllambda = ifelse( is.null(input$ctrllambda_N_TTE), 1, input$ctrllambda_N_TTE)
        ctrlcensorrate = ifelse( is.null(input$ctrlcensorrate_N_TTE), 0, input$ctrlcensorrate_N_TTE)
        lambda1 = ctrllambda*(ctrllambda/(ctrllambda + ctrlcensorrate))
        explambda = ifelse( is.null(input$explambda_N_TTE), 1.5, input$explambda_N_TTE)
        expcensorrate = ifelse( is.null(input$expcensorrate_N_TTE), 0, input$expcensorrate_N_TTE)
        lambda2 = explambda*(explambda/(explambda + expcensorrate))
        Tstudy = ifelse( is.null(input$studyduration_N_TTE), 5, input$studyduration_N_TTE)
        Tenrollment = ifelse(is.null(input$enrollmentduration_N_TTE), 0, input$enrollmentduration_N_TTE)
        eta = 0
        ratio = 1
        alpha = input$alpha_N_TTE
        beta =  1 - input$power_N_TTE
        sided = 2
        approx = FALSE
        type = "rr" # Im not sure
        entry = ifelse(is.null(input$enrollmentdist_SE_TTE), "unif", ifelse(input$enrollmentdist_SE_TTE == "Uniform", "unif", "expo"))
        gamma = ifelse( entry == "unif", NA, 1)
        #gamma = ifelse( is.null(input$gamma_N_TTE), 16, input$gamma_N_TTE) # This could be a problem
        #print(c(lambda1 = lambda1, lambda2 = lambda2, Ts = Tstudy, Tr = Tenrollment, eta = eta, ratio = ratio, alpha = alpha, beta = beta, sided = sided, approx = approx, type = type, entry = entry, gamma = gamma))
        N = ceiling(nSurvival(lambda1 = lambda1, lambda2 = lambda2, Ts = Tstudy, Tr = Tenrollment, eta = eta, ratio = ratio, alpha = alpha, beta = beta, sided = sided, approx = approx, type = type, entry = entry, gamma = gamma)$n)
        n = N/2
        # Output
        updateNumericInput(session, inputId = "Npergroup_N_TTE", min = n - .2*n, value = n, max = n + .2*n)
        updateSliderInput(session, inputId = "Npergroup_S_TTE", min = n - .2*n, value = n, max = n + .2*n)
        updateNumericInput(session, inputId = "totalN_N_TTE", min = N - .2*N, value = N, max = N + .2*N)
        updateSliderInput(session, inputId = "totalN_S_TTE", min = N - .2*N, value = N, max = N + .2*N)
      }
      
      ##### Power
      if(input$solvefor_TTE == "Power"){
        #### Prep
        #### numericInput was updated
        if(NorS_TTE$NorS == "N"){
          updateSliderInput(session, "studyduration_S_TTE", value = input$studyduration_N_TTE)
          updateSliderInput(session, "enrollmentduration_S_TTE", value = input$enrollmentduration_N_TTE)
          updateSliderInput(session, "gamma_S_TTE", value = input$gamma_N_TTE)
          updateSliderInput(session, "alpha_S_TTE", value = input$alpha_N_TTE)
          updateSliderInput(session, "Npergroup_S_TTE", value = input$Npergroup_N_TTE)
          updateSliderInput(session, "totalN_S_TTE", value = input$totalN_N_TTE)
          updateSliderInput(session, "ctrllambda_S_TTE", value = input$ctrllambda_N_TTE)
          updateSliderInput(session, "explambda_S_TTE", value = input$explambda_N_TTE)
          updateSliderInput(session, "ctrlcensorrate_S_TTE", value = input$ctrlcensorrate_N_TTE)
          updateSliderInput(session, "expcensorrate_S_TTE", value = input$expcensorrate_N_TTE)
        }
        #### sliderInput was updated
        if(NorS_TTE$NorS == "S"){
          updateNumericInput(session, "studyduration_N_TTE", value = input$studyduration_S_TTE)
          updateNumericInput(session, "enrollmentduration_N_TTE", value = input$enrollmentduration_S_TTE)
          updateNumericInput(session, "gamma_N_TTE", value = input$gamma_S_TTE)
          updateNumericInput(session, "alpha_N_TTE", value = input$alpha_S_TTE)
          updateNumericInput(session, "Npergroup_N_TTE", value = input$Npergroup_S_TTE)
          updateNumericInput(session, "totalN_N_TTE", value = input$totalN_S_TTE)
          updateNumericInput(session, "ctrllambda_N_TTE", value = input$ctrllambda_S_TTE)
          updateNumericInput(session, "explambda_N_TTE", value = input$explambda_S_TTE)
          updateNumericInput(session, "ctrlcensorrate_N_TTE", value = input$ctrlcensorrate_S_TTE)
          updateNumericInput(session, "expcensorrate_N_TTE", value = input$expcensorrate_S_TTE)
        }
        # Power Calculations
        # There are currently no power calculations
        # Output
        
      }
    }) # Observe
  } # server
) # shinyApp