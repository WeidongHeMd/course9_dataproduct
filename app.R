
library(shiny)

# Define UI for application to estimate mpg of a motor vehicle
# given its horse power and number of cylinders

ui <- fluidPage(

    # Application title
    titlePanel("Estimate MPG for Motor Vehicles"),

    # Sidebar with inputs for hp and cyl
    sidebarLayout(
        
        sidebarPanel(
            sliderInput("hp",
                        "Horse Power:",
                        min = 50,
                        max = 350,
                        value = 100),
            
            selectInput("cyl", 
                        "Number of Cylinders",
                        choices = c(4,6,8),
                        selected = 4),
            
            submitButton("Submit"),
            br(),
            p("Note: to estimate mpg of a vehicle, 
             enter horse power with the slider, select 
             cylinders from the dropdown menu, and click 'Submit'. 
             mpg will appear below the figure and show on the figure.")
        ),
        

        # Show a plot of the orignal data points, fitted linear 
        # model, and the estimated mpg in reponse to the user input: hp and cyl
        
        mainPanel(
           plotOutput("mpgPlot"),
           p("Estimated MPG for the vehicle is: "),
           textOutput("mpg"),
           br(),
           p("*For given a cylinder number, the x-axis limits are set as 
           the horse power range in the data. If the input horse power 
              is outside the limits, mpg will still be estimated but 
             not appear on the plot.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #ncyl <- reactiveValues(input$cyl)
    data_cyl <- reactive({subset(mtcars, cyl == input$cyl)})
    
    modfit <- reactive({lm(mpg~hp, 
                           data = data_cyl())}
                       )
        
    output$mpgPlot <- renderPlot({
        plot(data_cyl()$hp, data_cyl()$mpg, 
             xlab = paste("course power, cyl=", as.character(input$cyl)),
             ylab = "mpg", xlim = range(data_cyl()$hp))
        abline(modfit(), col="red")
        #hp <- input$hp
        mpg <- predict(modfit(), newdata=data.frame(hp=input$hp))
        points(input$hp, mpg, pch = 10, cex = 2)
    })
    
    output$mpg <- renderText({
        predict(modfit(), newdata=data.frame(hp=input$hp))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
