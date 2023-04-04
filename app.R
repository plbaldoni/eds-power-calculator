library(shiny)
library(data.table)
library(ggplot2)
library(pwr)
library(scales)

# Power formula
power <- function(alpha,delta,sigma,n,two.sided = TRUE){
  # Normal approx.
  # a <- ifelse(isTRUE(two.sided),alpha/2,alpha)
  # pnorm(delta / sqrt(2*(sigma ^ 2) / n) - qnorm(1 - a))
  
  # Using pwr.t.test instead
  a <- ifelse(isTRUE(two.sided),'two.sided',ifelse(delta > 0,'greater','less'))
  out <- pwr.t.test(n = n,d = delta/sigma,sig.level = alpha,type = "two.sample",alternative = a)
  out$power
}

ss <- function(alpha,delta,sigma,beta,two.sided = TRUE){
  out <- tryCatch(expr = {
    # Using pwr.t.test
    a <- ifelse(isTRUE(two.sided),'two.sided',ifelse(delta > 0,'greater','less'))
    pwr.t.test(power = 1 - beta,d = delta/sigma,sig.level = alpha,type = "two.sample",alternative = a)
  },error = function(e){
    # In edge cases, use normal formula approx.
    a <- ifelse(isTRUE(two.sided),alpha/2,alpha)
    n.norm <- 2 * ((qnorm(1 - a) + qnorm(1 - beta)) * sigma / delta) ^ 2
    list('n' = pmax(n.norm,2))
  })
  out$n
}

foo.breaks <- function(x,is.power,max.width = 15){
  # Need to improve breaks below
  # if(isTRUE(is.power)){
  #   out <- pretty(x)
  # } else{
  #   width <- diff(range(ceiling(x)))
  #   if (width <= max.width) {
  #     out <- fullseq(range(x),1)
  #   } else{
  #     out <- pretty(x,n = max.width)
  #   } 
  # }
  out <- pretty(x)
  return(out)
}

# Define UI for slider demo application
ui <- shinyUI(pageWithSidebar(
  headerPanel = headerPanel("Power Calculator"),
  sidebarPanel = sidebarPanel(
    textInput(inputId = "alpha", label = "Significance Level of The Test:",value = "0.05"),
    checkboxInput("checkbox", "Two-sided Test", value = TRUE),
    textInput(inputId = "delta",label =  "Difference in Means:",value = "1.5"),
    textInput(inputId = "sigma", label = "Standard Deviation:",value = "1.5"),
    uiOutput("helper"),
    radioButtons(inputId = 'radio',label = 'Calculate Power or Sample Size?',choices = list('Power' = 1,'Sample Size' = 2)),
    conditionalPanel(condition = "input.radio == 1",sliderInput(inputId = "ss", label = "Planned Sample Size:",min = 2, max = 100, value = 17, step = 1)),
    conditionalPanel(condition = "input.radio == 2",sliderInput(inputId = "power", label = "Planned Power:",min = 0.01, max = 0.99, value = 0.80, step = 0.01)),
    actionButton("refresh","Run"),
    helpText("For an independent two-sample t-test, this calculator computes either:"),
    HTML("<ul><li>The power given the planned sample size per group</li><li>The minimum necessary sample size per group given the planned power</li></ul>"),
    helpText("This app was developed by Pedro Baldoni as part of the WEHI Statistics Education Committee material. Calculations are based on the R package 'pwr'. Contributors: Quentin Gouil.")
  ),
  mainPanel = mainPanel(plotOutput(outputId = "lineplot1"),plotOutput(outputId = "lineplot2"))
))

server <- shinyServer(function(input, output) {
  
  # Table for sample size/power plot
  dt <- reactive({
    dt <- data.table(alpha = as.numeric(input$alpha),delta = as.numeric(input$delta), sigma = as.numeric(input$sigma),n = 2:1000)
    dt[,power := power(alpha = alpha,delta = delta,sigma = sigma,n = n,two.sided = input$checkbox)]
    return(dt)
  })
  
  # Calculating power/ss given input
  pwr <- reactive({power(alpha = as.numeric(input$alpha),delta = as.numeric(input$delta),sigma = as.numeric(input$sigma),n = input$ss,two.sided = input$checkbox)})
  ssn <- reactive({ss(alpha = as.numeric(input$alpha),delta = as.numeric(input$delta),sigma = as.numeric(input$sigma),beta = 1 - input$power,two.sided = input$checkbox)})
  
  # Table for sensitivity plot
  dt_effect <- reactive({
    if (input$radio == 1) {
      dt <- data.table(alpha = as.numeric(input$alpha),delta = seq(0.5*as.numeric(input$delta),1.5*as.numeric(input$delta),length = 100))
      dt <- dt[,.(sigma = seq(0.5*as.numeric(input$sigma),1.5*as.numeric(input$sigma),length = 100)),by = c('alpha','delta')]
      dt[,n := input$ss]
      dt[,z := power(alpha = alpha,delta = delta,sigma = sigma,n = n,two.sided = input$checkbox)]
      dt[z == 1, z := max(dt[z < 1, z])]
    } else{
      dt <- data.table(alpha = as.numeric(input$alpha),delta = seq(0.5*as.numeric(input$delta),1.5*as.numeric(input$delta),length = 50))
      dt <- dt[,.(sigma = seq(0.5*as.numeric(input$sigma),1.5*as.numeric(input$sigma),length = 50)),by = c('alpha','delta')]
      dt[,power := input$power]
      dt[,z := ss(alpha = alpha,delta = delta,sigma = sigma,beta = 1 - power,two.sided = input$checkbox),by = seq_len(nrow(dt))]
    }
    return(dt)
  })
  
  # Table for power/ss annotation
  dt_anno <- reactive({rbindlist(list(
    data.table(a = as.numeric(input$alpha),delta = as.numeric(input$delta), sigma = as.numeric(input$sigma),x = input$ss,y = -1,xend = input$ss,yend = pwr(),Type = 'Power'),
    data.table(a = as.numeric(input$alpha),delta = as.numeric(input$delta), sigma = as.numeric(input$sigma),x = -1,y = pwr(),xend = input$ss,yend = pwr(),Type = 'Power'),
    data.table(a = as.numeric(input$alpha),delta = as.numeric(input$delta), sigma = as.numeric(input$sigma),x = ssn(),y = -1,xend = ssn(),yend = input$power,Type = 'SS'),
    data.table(a = as.numeric(input$alpha),delta = as.numeric(input$delta), sigma = as.numeric(input$sigma),x = -1,y = input$power,xend = ssn(),yend = input$power,Type = 'SS')
  ))})
  
  # Table for sensitivity annotation
  dt_anno_effect <- reactive({data.table(delta = as.numeric(input$delta),sigma = as.numeric(input$sigma))})
  
  # Getting breaks for sensitivity plot
  breaks <- reactive({foo.breaks(dt_effect()$z,is.power = input$radio == 1)})
  
  # Plots
  figure1 <- eventReactive(input$refresh,{
    ggplot(dt(),aes(x = n,y = power)) +
      geom_line() +
      geom_segment(aes(x = x, y = y, xend = xend, yend = yend),linetype = 'dashed',data = dt_anno()[Type == ifelse(input$radio == 1,'Power','SS'),],colour = '#22A884FF',alpha = 0.55) +
      geom_point(size = 4,shape = 20,aes(x = xend,y = yend),data = dt_anno()[Type == ifelse(input$radio == 1,'Power','SS'),],color = '#22A884FF') +
      theme_bw() +
      coord_cartesian(ylim = c(0,1),xlim = c(1,max(10,dt()[(1 - power) < 0.001,min(n)]))) +
      scale_y_continuous(breaks = seq(0,1,0.1)) +
      scale_x_continuous(breaks = seq(1,1000,5)) +
      theme(panel.grid.minor = element_blank()) +
      labs(x = 'Sample Size per Group (N)',y = expression(paste(Power," (",1 - beta,")")),
           title = 'Power Curve',
           subtitle = ifelse(input$radio == 1,paste('With',input$ss,'samples per group, the power of the test is',formatC(pwr(),digits = 2,format = 'f')),paste('The minimum sample size to achieve',input$power,'power is',formatC(ceiling(ssn()),digits = 0,format = 'f'))))
  })
  
  figure2 <- eventReactive(input$refresh,{
    ggplot(data = dt_anno_effect(),aes(y = sigma,x = delta)) +
      geom_contour_filled(data = dt_effect(),aes(y = sigma,x = delta,z = z),inherit.aes = FALSE,breaks = breaks()) +
      geom_point(size = 4,shape = 4,color = 'white') +
      theme_bw() +
      theme(panel.grid = element_blank()) +
      scale_fill_viridis_d(option = "viridis",direction = -1) +
      scale_x_continuous(breaks = seq(min(dt_effect()$delta),max(dt_effect()$delta),length = 5)) +
      scale_y_continuous(breaks = seq(min(dt_effect()$sigma),max(dt_effect()$sigma),length = 5)) +
      labs(fill = ifelse(input$radio == 1,expression(paste(Power," (",1 - beta,")")),expression(paste(Sample~Size," (",N,")"))),
           x = expression(paste(Mean~Difference," (",delta,")")),
           y = expression(paste(Standard~Deviation," (",sigma,")")),
           title = 'Sensitivity Plot',
           subtitle = ifelse(input$radio == 1,
                             paste0('How the power of the test changes for the planned sample size (N = ',input$ss,')\nassuming an effect size of ',formatC(as.numeric(input$delta)/as.numeric(input$sigma),digits = 2,format = 'f'),' at a significance level of ',input$alpha),
                             paste0('How the minimum sample size changes for the planned power (Power = ',input$power,')\nassuming an effect size of ',formatC(as.numeric(input$delta)/as.numeric(input$sigma),digits = 2,format = 'f'),' at a significance level of ',input$alpha)))
  })
  
  # Show the values using an HTML table
  output$lineplot1 <- renderPlot({figure1()})
  output$lineplot2 <- renderPlot({figure2()})
  
  output$helper <- renderUI({
    helpText(paste("The assumed effect size is ",formatC(as.numeric(input$delta)/as.numeric(input$sigma),digits = 2,format = 'f')," (Cohen's d)"))
  })
})

# Create Shiny object
shinyApp(ui = ui, server = server)
