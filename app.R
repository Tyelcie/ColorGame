library(shiny)
library(shinyjs)
ui <- fluidPage(
    useShinyjs(),
    titlePanel("Color Game"),
    mainPanel(
        fluidRow(column(3,
                        h4('Your Color'),
                        plotOutput("distPlot")),
                 column(3,
                        h4('My Color'),
                        plotOutput('samplePlot'))
                 ),
            fluidRow(
                column(2,sliderInput("red","Red:",min = 0,max = 255,value = 0)),
                column(2,sliderInput("green","Green:",min = 0,max = 255,value = 0)),
                column(2,sliderInput("blue","Blue:",min = 0,max = 255,value = 0))
                ),
            fluidRow(
                column(3,actionButton('go','My Score')),
                column(3,actionButton('refresh','Play Again'))
                ),
            fluidRow(
                column(6,
                       p('Drag the sliders above to change your color pie,
              and try to make it closeser to mine.
              Then click the "My Score" button.'),
                       h3(textOutput('colordist')),
                       hr(),
                       br("This is a simulator to FunGyan's game ",
                          strong('Guess Thy Color')," on Google Play Store."),
                       a('https://play.google.com/store/apps/details?id=com.fungyan.guessthycolor'),
                       br('The "percent match" calculation is based on Euclidian Distance, see:'),
                       a('https://en.wikipedia.org/wiki/Color_difference'))      
                       )
            ))

server <- function(input, output,session) {
    Range <- 256
    rR <- sample(0:255,1)
    rG <- sample(0:255,1)
    rB <- sample(0:255,1)
    output$samplePlot <- renderPlot({
        par(mar = c(0,0,0,0))
        pie(1,col = rgb(rR/Range,rG/Range,rB/Range),
            labels = NA,lty = 0)
    })
    output$distPlot <- renderPlot({
        R <- input$red
        G <- input$green
        B <- input$blue
        par(mar = c(0,0,0,0))
        pie(1,col = rgb(R/Range,G/Range,B/Range),
            labels = NA,lty = 0)
    })
    observeEvent(input$go,{
        output$colordist <- renderText({
        dist <- 1-(sqrt((input$red-rR)^2+(input$green-rG)^2+(input$blue-rB)^2)/sqrt((255^2)*3))
        paste0('Percent Match: ',round(dist*100,0),'%')
        })})
    
    observeEvent(input$refresh,{
        session$reload()
    })
}
shinyApp(ui = ui, server = server)
