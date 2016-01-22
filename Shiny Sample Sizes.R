# Matt Partridge
# Advisor: Julian Wolfson
# Shiny Sample Sizes

######  ###########
### Libraries ###
#################
# library(shiny)
# library(shinydashboard)
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Sample Size Calculations"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("One Proportion", tabName = "oneproportion"),
        menuItem("Two Proportions", tabName = "twoproportions"),
        menuItem("Two Means", tabName = "twomeans"))),
    dashboardBody(
      tabItems(
        tabItem(tabName = "oneproportion",
                fluidRow(
                  box(
                    numericInput(inputId = "Noneprop", label = "N", min = 0, max = NA, value = ""),
                    sliderInput(inputId = "pioneprop", label = "Pi", min = 0, max = 1, value = 0.5),
                    selectizeInput(inputId = "confoneprop", label = "Confidence", choices = c(0.9, 0.95, 0.98, 0.99, 0.995))),
                  box(
                    sliderInput(inputId = "moeoneprop", label = "Margin of Error", min = 0, max = 80, value = 0.5)
                  )
                )),
        
        
        tabItem(tabName = "twoproportions",
                fluidRow(
                  box(
                    sliderInput(inputId = "alpha", label = "Significance:",
                                min = 0, max = 100, value = 95.0),
                    sliderInput(inputId = "beta", label = "Power:",
                                min = 0, max = 100, value = 80.0)),
                  box(
                    sliderInput(inputId = "p1", label = "Proportion Affected by Treatment 1:",
                                min = 0, max = 1, value = 0.5),
                    sliderInput(inputId = "p2", label = "Proportion Affected by Treatment 2:",
                                min = 0, max = 1, value = 0.4))),
                fluidRow(
                  box(
                    textOutput("n.per.group.props"),
                    textOutput("TotalN.props"),
                    textOutput("TEST")))),
        
        tabItem(tabName = "twomeans",
                fluidRow(
                  box(
                    sliderInput(inputId = "alpha", label = "Significance:",
                                min = 0, max = 100, value = 95.0),
                    sliderInput(inputId = "beta", label = "Power:",
                                min = 0, max = 100, value = 80.0)),
                  box(
                    numericInput(inputId = "mu1", label = "Mu_1", value = ""),
                    numericInput(inputId = "sd", label = "Standard Deviation", value = ""),
                    numericInput(inputId = "mu2", label = "Mu_2", value = ""))),
                fluidRow(
                  box(
                    textOutput("n.per.group.means"),
                    textOutput("TotalN.means"))))
        ) # tabItems
      ) # dashboardBody
    ), # dashboardPage
  
  server = function(input, output){
    output$TEST = renderText({
      ifelse(input$solvefor == "alpha", paste("alpha"), ifelse(input$solvefor == "Power", paste("Power"), paste("Sample Size")))
    })
    output$n.per.group.props <- renderText({
      alpha = (100 - input$alpha)/100
      beta = input$beta/100
      p1 = input$p1
      p2 = input$p2
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
      paste("The number of people needed per group is: ", n)
    })
    output$TotalN.props <- renderText({
      alpha = (100 - input$alpha)/100
      beta = input$beta/100
      p1 = input$p1
      p2 = input$p2
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * (p1*(1 - p1) + (p2*(1 - p2)))) / ((p1 - p2)^2))
      N = 2*n
      paste("The total number of people needed is: ", N)
    })
    
    output$n.per.group.means <- renderText({
      alpha = (100 - input$alpha)/100
      beta = input$beta/100
      mu1 = input$mu1
      mu2 = input$mu2
      sd = input$sd
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      n = ceiling(((Z.alpha.2 + Z.beta)^2 * 2*(sd^2))/((mu1 - mu2)^2))
      paste("The number of people needed per group is: ", n)
    })
    
    output$TotalN.means <- renderText({
      alpha = (100 - input$alpha)/100
      beta = input$beta/100
      mu1 = input$mu1
      mu2 = input$mu2
      sd = input$sd
      Z.alpha.2 = qnorm(1 - alpha/2)
      Z.beta = qnorm(beta)
      N = 2*ceiling(((Z.alpha.2 + Z.beta)^2 * 2*(sd^2))/((mu1 - mu2)^2))
      paste("The total number of people needed is: ", N)
    })
  }
)

