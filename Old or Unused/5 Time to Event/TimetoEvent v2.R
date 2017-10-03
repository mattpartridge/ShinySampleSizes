###############################
###############################
###      Matt Partridge     ###
### University of Minnesota ###
###    MS Plan B Project    ###
###    Shiny Sample Sizes   ###
###      Time to Event      ###
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
                  uiOutput("TTE")))))
  ), # dashboardPage
  
  
  server = function(input, output, clientData, session){
    
    ##### User Interface
    output$TTE = renderUI({
      list(
        # Study Information
        box(
          selectInput(inputId = "enrollment_SE_TTE", label = "Enrollment Schedule", choices = c("All at Once", "Over a Period", "Continuous Throughout"), selected = "All at Once"),
#             if(input$enrollment_SE_TTE %in% c("Over a Period", "Continuous Throughout")){
#               list(
#                 selectInput(inputId = "enrollmentdist_SE_TTE", label = "Distribution of Enrollment", choices = c("Uniform", "Exponential Decay"), selected = "Uniform")
#                 if(input$enrollmentdist_SE_TTE == "Exponential Decay"){
#                   list(
#                     numericInput(inputId = "gamma_N_TTE", label = "Decay Rate", value = 0.5, min = 0, max = 1, step = 0.01),
#                     sliderInput(inputId = "gamma_S_TTE", label = "", value = 0.5, min = 0, max = 1, step = 0.01))}
                # )},
            numericInput(inputId = "studyduration_N_TTE", label = "Study Duration", value = 5, min = 0, max = 10, step = 0.25),
            sliderInput(inputId = "studyduration_S_TTE", label = "", value = 5, min = 0, max = 10, step = 0.25)
#             if(input$enrollment_SE_TTE %in% c("Over a Period", "Continuous Throughout")){
#               list(
#                 numericInput(inputId = "enrollmentduration_N_TTE", label = "Enrollment Duration", value = 2.5, min = 0, max = 10, step = 0.25),
#                 sliderInput(inputId = "enrollmentduration_S_TTE", label = "", value = 2.5, min = 0, max = 10, step = 0.25))}
)
        # Calculation Information
#         box(
#           numericInput(inputId = "alpha_N_TTE", label = "Significance Level", value = 0.05, min = 0, max = 1, step = 0.01),
#           sliderInput(inputId = "alpha_S_TTE", label = "", value = 0.05, min = 0, max = 1, step = 0.01),
#           if(input$solvefor_TTE == "Sample Size"){
#             list(
#               numericInput(inputId = "power_N_TTE", label = "Power", value = 0.8, min = 0, max = 1, step = 0.01),
#               sliderInput(inputId = "power_S_TTE", label = "", value = 0.8, min = 0, max = 1, step = 0.01))},
#           if(input$solvefor_TTE == "Power"){
#             list(
#               numericInput(inputId = "Npergroup_N_TTE", label = "Sample Size per Group", value = 500, min = 0, max = 10000, step = 0.01),
#               sliderInput(inputId = "Npergroup_TTE", label = "", value = 500, min = 0, max = 10000, step = 0.01),
#               numericInput(inputId = "totalN_N_TTE", label = "Total Sample Size", value = 1000, min = 0, max = 10000, step = 0.01),
#               sliderInput(inputId = "totalN_S_TTE", label = "", value = 1000, min = 0, max = 10000, step = 0.01))},
#           numericInput(inputId = "ctrllambda_N_TTE", label = "Control Group's Event Rate per Unit of Time", value = 1, min = 0, max = 10, step = 0.01),
#           sliderInput(inputId = "ctrllambda_S_TTE", label = "", value = 1, min = 0, max = 10, step = 0.01),
#           numericInput(inputId = "explambda_N_TTE", label = "Experimental Group's Event Rate per Unit of Time", value = 1.5, min = 0, max = 10, step = 0.01),
#           sliderInput(inputId = "explambda_S_TTE", label = "", value = 1.5, min = 0, max = 10, step = 0.01),
#           numericInput(inputId = "ctrlcensorrate_N_TTE", label = "Control Group's Censor Rate", value = 0, min = 0, max = 1, step = 0.01),
#           sliderInput(inputId = "ctrlcensorrate_S_TTE", label = "", value = 0, min = 0, max = 1, step = 0.01),
#           numericInput(inputId = "expcensorrate_N_TTE", label = "Experimental Group's Censor Rate", value = 0, min = 0, max = 1, step = 0.01),
#           sliderInput(inputId = "expcensorrate_S_TTE", label = "", value = 0, min = 0, max = 1, step = 0.01)),
#         # Outputs
#         box(
#           if(input$solvefor_TTE == "Sample Size"){
#             list(
#               numericInput(inputId = "Npergroup_N_TTE", label = "Sample Size per Group", value = 500, min = 0, max = 10000, step = 0.01),
#               sliderInput(inputId = "Npergroup_TTE", label = "", value = 500, min = 0, max = 10000, step = 0.01),
#               numericInput(inputId = "totalN_N_TTE", label = "Total Sample Size", value = 1000, min = 0, max = 10000, step = 0.01),
#               sliderInput(inputId = "totalN_S_TTE", label = "", value = 1000, min = 0, max = 10000, step = 0.01))},
#           if(input$solvefor_TTE == "Power"){
#             list(
#               numericInput(inputId = "power_N_TTE", label = "Power", value = 0.8, min = 0, max = 1, step = 0.01),
#               sliderInput(inputId = "power_S_TTE", label = "", value = 0.8, min = 0, max = 1, step = 0.01))})
)
    })
    
    ##### Numeric or Slider
    NorS_TTE = reactiveValues( NorS = "" )
    observe({
      # List of all numericInput vars
      NorS_TTE$NorS = "N"
    })
    observe({
      # List of all sliderInput vars
      NorS_TTE$NorS = "S"
    })
    
    # Calculations
    observe({
      ##### Sample Size
      if(input$solvefor_TTE == "Sample Size"){
        #### numericInput was updated
        if(NorS_TTE$NorS == "N"){
          ### Update sliderInputs
        }
        #### sliderInput was updated
        if(NorS_TTE$NorS == "S"){
          ### Update numericInputs
        }
        # Sample Size Calculations
        
        # Output

      }
      
      ##### Power
      if(input$solvefor_TTE == "Power"){
        #### Prep
        ### Numeric
        if(NorS_TTE$NorS == "N"){
          # Update Slider Inputs
        }
        ### Slider
        if(NorS_TTE$NorS == "S"){
          # Update Numeric Inputs
        }
        # Power Calculations

        # Output

      }
    }) # Observe
  } # server
) # shinyApp