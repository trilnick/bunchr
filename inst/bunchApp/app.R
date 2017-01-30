#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)
library(bunchr)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
  titlePanel("bunchApp - Explore Bunching Simulations"),

  sidebarLayout(

   # Sidebar with a slider input for number of bins
   sidebarPanel(
      selectInput("numobs", label = ("# Observations"),
                 choices = list("1,000" = 1000,
                                "10,000" = 10000,
                                "100,000" = 100000),
                 selected = 10000),

      numericInput("zstar",
                  "Kink point:", 1000,
                  min = 1,
                  max = 5000),
      sliderInput("t1",
                 "Marginal Tax Rate Before:",
                 min = 0,
                 max = 0.9,
                 value = 0.1,
                 step = 0.1),
      sliderInput("t2",
                   "Marginal Tax Rate After:",
                   min = 0,
                   max = 0.9,
                   value = 0.3,
                   step = 0.1),
      numericInput("Tax",
                   "Tax at Notch:", 0,
                   min = 0,
                   max = 5000),
      sliderInput("elas",
                   "Earning Elasticity w.r.t MTR:",
                   min = 0,
                   max = 1.5,
                   value = 0.2,
                   step = 0.1),
      #checkboxInput("noise", label = "Add Noise", value = TRUE),
      #selectInput("noise", label = ("Noise"),
      #            choices = list("No Noise" = 0,
      #                           "SD = 50" = 1,
      #                           "SD = 100" = 2,
      #                           "SD = 200" = 4),
      #            selected = 1),
      #radioButtons("noise", label = "Noise SD",
      #             choices = list("0" = 0,
      #                            "50" = 1,
      #                            "100" = 2,
      #                            "200" = 4),
      #             inline = TRUE,
      #             selected = 1),
      sliderInput("noise",
                  "Noise Standard Deviation:",
                  min = 0,
                  max = 500,
                  value = 50,
                  step = 50)
      ),

     # Show a plot of the generated distribution
     mainPanel(
      plotOutput("plot_1"),

      hr(),

      fluidRow(
        column(width = 4,
          sliderInput("binw",
                      "Bin Width:",
                      min = 1,
                      max = 100,
                      value = 40)),
        column(width = 8,
               uiOutput("uiCF")
               )
      ),

      fluidRow(
        column(width = 4,
               radioButtons("sumzero", label = "Sum Zero Integral",
                            choices = list("Yes" = 1, "No" = 2), inline = TRUE,
                            selected = 1)
               ),
        column(width = 8,
               uiOutput("uiEX")
              )
      )
     )
   ),

  fluidRow(
    column(width = 4,
           h3(textOutput("caption"), style = "color:blue")
           ),
    column(width = 8,
           h3(textOutput("est.elas"), style = "color:blue")
           )
  ),

  fluidRow(
    hr(),
    #add the explanation text below:
    column(width = 12,
           includeMarkdown(file.path(system.file("bunchApp\\files", "app_readme.md",
                           package = "bunchr")))
          )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  set.seed(42)
  ability_vec <- reactive({
    4000 * rbeta(input$numobs, 2, 5)
  })

  noise_vec <- reactive({
    #rnorm(n = input$numobs, mean = 0, sd = 50)
    rnorm(n = input$numobs, mean = 0, sd = 1)
  })


  earning_vec <- reactive({
               sapply(ability_vec(), earning_fun,
                  input$elas,
                  input$t1,
                  input$t2,
                  input$Tax,
                  input$zstar) +
                  as.numeric(input$noise) * noise_vec()
                  })

  tax_equal <- reactive({
    as.character(input$t1 == input$t2 & input$Tax == 0)
    })

  output$plot_1 <- renderPlot({
       switch(tax_equal(),

          "FALSE" = bunchr::bunch(earning_vec(),
            input$zstar,
            input$t1,
            input$t2,
            input$Tax,
            binw = input$binw,
            cf_start = -input$cf_zone[1],
            cf_end = input$cf_zone[2],
            exclude_before = -input$excluded_zone[1],
            exclude_after = input$excluded_zone[2],
            correct = switch(input$sumzero, "1" = TRUE, "2" = FALSE),
            force_after = switch(input$sumzero, "1" = FALSE, "2" = TRUE),
            title = paste0("Earning Simulation: N = ", input$numobs)),
         "TRUE" = bunchr::bunch_viewer(earning_vec(),
            input$zstar,
            cf_start = -input$cf_zone[1],
            cf_end = input$cf_zone[2],
            exclude_before = -input$excluded_zone[1],
            exclude_after = input$excluded_zone[2],
            binw = input$binw,
            title = paste0("Earning Simulation: N = ", input$numobs),
            varname = "Earnings")
       )
    })

  output$caption <- renderText("Estimated Elasticity:")


  output$est.elas <- renderText({
    switch(tax_equal(),
           "FALSE" = round(bunch(earning_vec(),
                                 input$zstar,
                                 input$t1,
                                 input$t2,
                                 input$Tax,
                                 binw = input$binw,
                                 cf_start = -input$cf_zone[1],
                                 cf_end = input$cf_zone[2],
                                 exclude_before = -input$excluded_zone[1],
                                 exclude_after = input$excluded_zone[2],
                                 draw = FALSE)$e, digits = 2),
    "TRUE" = "Can't Calculate Elasticity With Kink when t1 = t2" )
  })

  # controling for the slider bars
  output$uiCF <- renderUI({
    sliderInput("cf_zone",
                "Counter-Factual Range (bins around kink)",
                min = floor(-1500/input$binw),
                max = floor(1500 /input$binw),
                value = c(-20,20),
                step = 1)

  })


  output$uiEX <- renderUI({
    sliderInput("excluded_zone",
                "Excluded Range (bins around kink)",
                min = floor(-1500/input$binw) + 1,
                max = floor(1500/input$binw) - 1,
                value = as.vector(
                  switch(as.character(input$Tax > 0),
                       "TRUE" = c(-2,10),
                       "FALSE" = c(-3,3) )
                ),
                step = 1)

  })


}

# Run the application
shinyApp(ui = ui, server = server)

