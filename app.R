library(shiny)
library(colourpicker)

server <- function(input, output) {
  
  PlotHeight <- reactive(
    return(input$height)
  )
  
  gcd <- function(p, q) {
    while(q) {
      temp = q
      q = p %% q
      p = temp
    }
    return(p)
  }
  
  is.odd <- function(x) x %% 2 != 0
  
  output$polarPlot <- renderPlot({
    
    A <- input$a/gcd(input$a, input$b)
    B <- input$b/gcd(input$a, input$b)
    C <- input$step/gcd(input$step, 180)
    D <- 180/gcd(input$step, 180)
    
    if(is.odd(input$a) & is.odd(input$b)) T <- B*D/gcd(B,C) else T <- 2*B*D/gcd(2*B,C)
    
    xylim <- 1/input$zoom
    
    equation <- bquote(italic(r)==sin (.(A)* theta/.(B)))
    if (B==1) equation <- bquote(italic(r)==sin( .(A) * theta))
    if (A==1) equation <- bquote(italic(r)==sin( theta / .(B)))
    if (A==1 & B==1) equation <- bquote(italic(r)==sin ( theta ))
    
    to <- T*input$step #in degrees
    theta <- seq(from=0, to=to, by=input$step)
    theta <- theta*pi/180
    r <- sin((A/B)*theta)
    x <- r*cos(theta)
    y <- r*sin(theta)
    par(bg = input$bgcol)
    if (input$png) png(filename=paste0(A,"over",B,"by",input$step,"zoom",input$zoom,".png"),
                       width=18000, height=24000, bg=input$bgcol)
    plot(y~x, type="l", axes=FALSE, lwd=input$lwd, col=input$color, xlab="", ylab="",
         asp=1, xlim=c(-xylim-input$xoff, xylim-input$xoff), ylim=c(-xylim-input$yoff, xylim-input$yoff))
    if (input$eqn) mtext(equation, cex=2, col=input$color, side=3, adj=0)
    if (input$png) dev.off()
  })
  
  output$plot.ui <- renderUI({
    plotOutput("polarPlot", height = PlotHeight())
  })
}

ui <- fluidPage(
  fluidRow(
    column(width=2,
           sliderInput("a", label = "Angle Coefficient Numerator", min=1, max=90, value=4, step=1)),
    column(width=2,
           sliderInput("step", label = "Step Size (Degrees)", min=0.5, max=180, value=137, step=0.5)),
    column(width=2,
           sliderInput("lwd", label = "Line Width", min=1, max=200, value=1, step=1)),
    column(width=2,
           sliderInput("yoff", label = "Vertical Offset", min=0, max=1, value=0, step=0.001)),
    column(width=1,
           checkboxInput("eqn", label = "Show Equation", value = FALSE, width = NULL)),
    column(width=1,
           checkboxInput("png", label = "To png File", value = FALSE, width = NULL)),
    column(width=2,
           submitButton(text="Refresh Image")),
    column(width=2,
           div("PolarTrig", style = 'font-weight: bold; font-size: 30px;'))

  ),
  fluidRow(
    column(width=2,
           sliderInput("b", label = "Angle Coefficient Denominator", min=1, max=90, value=3, step=1)),
    column(width=2,
           sliderInput("zoom", label = "Zoom Factor", min=1, max=100, value=1, step=1)),
    column(width=2,
           sliderInput("height", label = "Canvas Height (Pixels)", min = 600, max = 20000, value = 1000, step=1)),
    column(width=2,
           sliderInput("xoff", label = "Horizontal Offset", min=0, max=1, value=0, step=0.001)),
    column(width=2,
           colourInput("bgcol", label="Background Color", value="blue", showColour="background")),
    column(width=2,
           colourInput("color", label= "Graph Color", value="yellow", showColour="background"))
  ),
  fluidRow(uiOutput("plot.ui"))
)

shinyApp(ui = ui, server = server)
