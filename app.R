#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Color Game"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            p('Drag the sliders below to change your color pie,
              and try to make it closeser to mine.
              Then click the "My Score" button.'),
            sliderInput("red","Red:",min = 0,max = 255,value = 0),
            sliderInput("green","Green:",min = 0,max = 255,value = 0),
            sliderInput("blue","Blue:",min = 0,max = 255,value = 0),
            actionButton('go','My Score')
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            column(6,
                   h4('Your Color'),
                   plotOutput("distPlot")),
            column(6,
                   h4('My Color'),
                   plotOutput('samplePlot')),
            h3(textOutput('colordist')),
            br("This is a simulator to FunGyan's game ",
              strong('Guess Thy Color')," on Google Play Store."),
            a('https://play.google.com/store/apps/details?id=com.fungyan.guessthycolor'),
            br('The "percent match" calculation is based on Euclidian Distance, see:'),
            a('https://en.wikipedia.org/wiki/Color_difference')
            ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    Range <- 256
    rR <- sample(0:255,1)
    rG <- sample(0:255,1)
    rB <- sample(0:255,1)
    output$samplePlot <- renderPlot({
        pie(1,col = rgb(rR/Range,rG/Range,rB/Range),
            labels = NA,lty = 0)
    })
    output$distPlot <- renderPlot({
        R <- input$red
        G <- input$green
        B <- input$blue
        pie(1,col = rgb(R/Range,G/Range,B/Range),
            labels = NA,lty = 0)
    })
    output$colordist <- renderText({
        input$go
        dist <- isolate(1-(sqrt((input$red-rR)^2+(input$green-rG)^2+(input$blue-rB)^2)/sqrt((255^2)*3)))
        paste0('Percent Match: ',round(dist*100,0),'%')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

