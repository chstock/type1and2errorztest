library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel( div(HTML("Type 1 and type 2 error in the two-sided <em>z</em>-test"))),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(width=4,

          sliderInput(inputId = "n", label = "Sample size per group (n)", 
                      min=20, max=200, step=1, value=86),
          
          sliderInput(inputId = "muX", label = "True mean in group X:", 
                      min=0, max=100, step=1, value=60),
          
          sliderInput(inputId = "muY", label = "True mean in group Y:", 
                      min=0, max=100, step=1, value=70),

          sliderInput(inputId = "sigma", label = "Common standard deviation (sigma):", 
                      min=0, max=50, step=0.5, value=20),          
                    
          sliderInput("alpha", "Significance level (alpha):", 
                      min = 0, max = 0.2, value = 0.05, step=0.01, ticks = T),
          
        ),

        # Show a plot of the generated distribution
        mainPanel(width=8,
           plotOutput("distPlot", width="100%"),
           textOutput("lambda"),
           textOutput("alpha"),
           textOutput("beta"),
           textOutput("power")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Reactive values 

  # lambda
  lambda <- reactive(return(sqrt((input$n^2)/(2*input$n))*((input$muY-input$muX)/input$sigma)))
  
  # beta
  beta <- reactive(
    if (lambda()>0)
      return(pnorm(q = qnorm(1-input$alpha/2), mean = lambda(), sd = 1))
    else 
      return(pnorm(q = qnorm(1-input$alpha/2), mean = -lambda(), sd = 1))
  )
  
  # power
  power <- reactive(return(1-beta()))
  
  
doPlot <- function(){

  plot.height <- 0.47
  
  arr.beta <- list(x=ifelse(lambda()>0,(2/3)*qnorm(1-input$alpha/2),-(2/3)*qnorm(1-input$alpha/2)), 
                   y=0.035)
  
  arr.alpha <- list(lx = 0-2.9*1,
                    ly = 0.04,
                    lxend = -qnorm(1-input$alpha/2)-0.3*1,
                    lyend = 0.015,
                    rx = 0+2.9*1,
                    ry = 0.04,
                    rxend = qnorm(1-input$alpha/2)+0.3*1,
                    ryend = 0.015
  ) 
  
  params.beta.low <- ifelse(lambda()>0, -10, -qnorm(1-input$alpha/2))
  params.beta.up <- ifelse(lambda()>0, qnorm(1-input$alpha/2), 10)  
  
  ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
    stat_function(fun=dnorm, n=200, args= list(mean=0, sd=1),
                  aes(colour = "f0"), size=1.1) +
    stat_function(fun=dnorm, n=200, args= list(mean=lambda(), sd=1),  
                  aes(colour = "fA"), size=1.1) + 
    scale_y_continuous(breaks = NULL) + 
    scale_y_continuous(breaks = NULL, 
                       limits = c(0, plot.height)) + 
    geom_segment(aes(x=qnorm(1-input$alpha/2), y=0,
                     xend=qnorm(1-input$alpha/2), yend=plot.height),
                 size=0.5, color="gray47") + 
    geom_segment(aes(x=-qnorm(1-input$alpha/2), y=0,
                     xend=-qnorm(1-input$alpha/2), yend=plot.height),
                 size=0.5, color="gray47") +
    theme_bw(base_size = 22) + 
    theme(legend.position="bottom") +
    xlab("Z") +  ylab("") +
    xlim(min(0,lambda())-3.5, max(0,lambda())+3.5)+
    scale_colour_manual("", values = c("black", "black", "firebrick3", "royalblue")) +
    annotate("text", label = expression("f"["0"]), size=8, 
             x = 0, y = dnorm(0.5,mean = 0, sd = 1)+0.081) +
    annotate("text", label = expression("f"["A"]), size=8, 
           x = lambda(), y = dnorm(0.5,mean = 0, sd = 1)+0.081) +
    theme(legend.position="none") +
    geom_segment(aes(x=arr.alpha$lx, y=arr.alpha$ly, 
                     xend=arr.alpha$lxend, yend=arr.alpha$lyend),
                 size=0.5, color="gray30", 
                 arrow=arrow(angle=25, type="closed", length=unit(0.1,"inches"))) +
    annotate("text", label = expression(alpha/2), size=7, hjust = 1.2,
             x = arr.alpha$lx, y = arr.alpha$ly) +
    geom_segment(aes(x=arr.alpha$rx, y=arr.alpha$ry, 
                     xend=arr.alpha$rxend, yend=arr.alpha$ryend),
                 size=0.5, color="gray30", 
                 arrow=arrow(angle=25, type="closed",length=unit(0.1,"inches"))) +  
    annotate("text", label = expression(alpha/2), size=7, hjust = -0.2,
             x = arr.alpha$rx, y = arr.alpha$ry) +
    annotate("text", label = expression(beta), size=7, hjust = 0,
             x = arr.beta$x, y = arr.beta$y) +
    stat_function(fun=dnorm, n=200, 
                  args= list(mean=0, sd=1),
                  geom="area", xlim=c(-10,-qnorm(1-input$alpha/2)),
                  fill="red",colour=NA, alpha=0.5) + 
    stat_function(fun=dnorm, n=200, 
                  args= list(mean=0, sd=1),
                  geom="area", xlim=c(qnorm(1-input$alpha/2),10),
                  fill="red",colour=NA, alpha=0.5) +
    stat_function(fun=dnorm, n=200, 
                  args= list(mean=lambda(), sd=1),
                  geom="area", xlim=c(params.beta.low, params.beta.up),
                  fill="royalblue", colour=NA, alpha=0.5) 
}

  # Plot
  output$distPlot <- renderPlot( {
      p <- doPlot()
      print(p)
    })
    
  output$lambda <-  renderText({ 
    paste("Noncentrality parameter (lambda) = ", round(lambda(),2))
  })

  output$alpha <-  renderText({ 
    paste("Type 1 error (alpha) = ", round(input$alpha,2), " (", round(input$alpha,2)*100, "%)", sep="")
  })   
  
  output$beta <-  renderText({ 
    paste("Type 2 error (beta) = ", round(beta(),2), " (", round(beta(),2)*100, "%)", sep="")
  }) 

  output$power <-  renderText({ 
    paste("Statistical power (1-beta) = ", round(power(),2), " (", round(power(),2)*100, "%)", sep="")
  }) 

}

# Run the application 
shinyApp(ui = ui, server = server)
