library(shiny)
library(ggplot2)
source('./ext/logistic.R')

options(digits.secs=4)

# Define server logic to manage learning about logistic regression
shinyServer(function(input, output) {
  
  
  data_sub1 <- reactive({
    if(!('fixed_seed' %in% input$options)){
      probability <- 1-input$bias
      the_bias <- log((1/probability) - 1)
      input$new_data  
      isolate({  # Create unreplicable random seed
        the_seed <- substr(as.POSIXlt(Sys.time(), "HST"), 22, 24)
        the_seed <- strsplit(the_seed, NULL)[[1]]
        the_seed <- rev(the_seed)
        the_seed <- as.integer(paste(the_seed, collapse=''))
        plot_data <- logitdat(vars = 1, weights=input$effect_size, n = input$sample_size, bias=the_bias, fixed_seed=the_seed)   
        return(plot_data)
      })
    }
  })
  
  data_sub2 <- reactive({
    if('fixed_seed' %in% input$options){
      probability <- 1-input$bias
      the_bias <- log((1/probability) - 1)
      the_seed <- 666  # Pass replicable random seed
      plot_data <- logitdat(vars = 1, weights=input$effect_size, n = input$sample_size, bias=the_bias, fixed_seed=the_seed)   
    }
  })
  
  
  data_reactive <- reactive({
    if('fixed_seed' %in% input$options){
      return(data_sub2())
    } else if (!('fixed_seed' %in% input$options)){
      return(data_sub1())
    }
  })
  
  
  output$plot_output <- renderPlot({
    p <- ggplot(data_reactive(), aes(x=x1, y=y))
    
    logistic_se <- F
    linear_se <- F
    if('linear_se' %in% input$modeltype){
      linear_se <- T      
    }
    if('logistic_se' %in% input$modeltype){
      logistic_se <- T      
    }
    
    if('jitter' %in% input$options){
      p <- p + geom_point(position = position_jitter(w = 0, h = 0.05))
    } else{
      p <- p + geom_point()
    }
    
    p <- p + scale_colour_discrete(name  ="Disease",
                                   breaks=c("0", "1"),
                                   labels=c("No Disease", "Disease"))
    
    if("linear" %in% input$modeltype){
      p <- p + geom_smooth(method=lm, se=linear_se, color='blue')
    }
    if("logistic" %in% input$modeltype){
      p <- p + stat_smooth(method="glm", family="binomial", se=logistic_se, color='red')
    }
    if("loess" %in% input$modeltype){
      p <- p + geom_smooth(se=FALSE, color='purple')
    }
    
    p <- p + xlim(-3.5, 3.5) + ylim(-.3, 1.3)
    
    p <- p +  ylab('Pr(Disease)') + xlab('Predictor')
    
    if('intercept' %in% input$options){
      intercept <- 1/(1 + exp(-(data_analysis()$coef[1])))
      tmpDF <- data.frame(x=0, y=intercept)
      p <- p + geom_point(data=tmpDF,aes(x=x,y=y),colour="dark green",size=4)
    }
    
    p
  })
  
  
  # Generate the analysis of the Logistic Regression
  data_analysis <- reactive({
    dataset <- data_reactive()
    glm_output <- glm(y ~ x1, data=dataset, family=binomial(link="logit"))
    return(glm_output)
  })
  
  
  
  
  # Produce a summary of the Logistic Regression analysis
  output$summary <- renderPrint({
    if('summary_output' %in% input$options){
      summary(data_analysis())
    }
  })
  
})
