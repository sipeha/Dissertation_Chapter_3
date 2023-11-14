#app for visualizing parameters expert elicitation

#load in packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(mc2d)

#load in functions
beta.fit1 <- function(min.data,ml.data,max.data,conf){
  calculate.mode <- function(alpha, beta) {
    return((alpha-1) / (alpha+beta-2))
  }
  calculate.mean<-function(alpha, beta){
    return(alpha/(alpha+beta))
  }
  #first beta.ss gets good initial values for fitting
  beta.ss <- function(par){ 
    #parameters to estimate
    alpha <- par[1]
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    #beta quantile function
    min.model <- qbeta(p = pr1, shape1 = alpha, shape2 = (alpha*(1-ml.data))/ml.data, lower.tail = TRUE, log = FALSE)
    max.model <- qbeta(p = pr2, shape1 = alpha, shape2 = (alpha*(1-ml.data))/ml.data, lower.tail = TRUE, log = FALSE)
    mode.model <- (alpha-1)/(alpha+(alpha*(1-ml.data))/(ml.data)-2)
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data-mode.model)^2
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit <- optim(c(1),beta.ss, method = "Brent",lower = 0, upper = 5000)
  
  alpha1 <- fit$par
  beta1 <- (alpha1*(1-ml.data))/(ml.data)
  
  #beta.ss2 fits the mode better, using beta.fit estimated alpha and beta
  beta.ss2 <- function(par2){ 
    #parameters to estimate
    alpha <- par2[1]
    beta<-par2[2]
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    #beta quantile function
    min.model <- qbeta(p = pr1, shape1 = alpha, shape2 = beta, lower.tail = TRUE, log = FALSE)
    max.model <- qbeta(p = pr2, shape1 = alpha, shape2 = beta, lower.tail = TRUE, log = FALSE)
    
    mode.model <- calculate.mode(alpha, beta)

    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data-mode.model)^2
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit2 <- optim(par=c(alpha1,beta1),beta.ss2, method="CG")

  alpha.out <- fit2$par[1]
  beta.out <- fit2$par[2]
  mode.out <- (alpha.out-1) / (alpha.out+beta.out-2)
  params <- list("alpha" = alpha.out,"beta" = beta.out, "mode"=mode.out)
  return(params)
}

gamma.fit <- function(min.data,ml.data,max.data,conf){
  calculate.mode <- function(alpha, beta) {
    return((alpha-1) / (beta))
  }
  #function to return sum of squares for fitting gamma parameters 
  gamma.ss <- function(par){ 
    
    #parameters to estimate
    alpha <- par[1]
    
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    
    #gamma quantile function
    min.model <-qgamma(p=pr1, shape = alpha, rate = (alpha/ml.data), lower.tail = T, log=F)
    max.model <-qgamma(p=pr2, shape = alpha, rate = (alpha/ml.data), lower.tail = T, log=F)
    mode.model <- (alpha-1)/(alpha/ml.data)
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data - mode.model)^2
    
    return(ss)
    
  }
  #run optim - use min and max as starting values 
  fit <- optim(c(2),gamma.ss, method = "Brent",lower = 0, upper = 5000)
  
  alpha1 <- fit$par
  beta1 <- alpha1/ml.data 
  gamma.ss <- function(par){ 
    
    #parameters to estimate
    alpha <- par[1]
    beta <- par[2]
    
    #get desired quantiles from confidence 
    pr1 <- (1-conf)/2 
    pr2 <- 1-(1-conf)/2
    
    #gamma quantile function
    min.model <-qgamma(p=pr1, shape = alpha, rate = beta, lower.tail = T, log=F)
    max.model <-qgamma(p=pr2, shape = alpha, rate = beta, lower.tail = T, log=F)
    mode.model <- calculate.mode(alpha, beta)
    #sum of squares
    ss <- (min.data-min.model)^2 + (max.data-max.model)^2 + (ml.data - mode.model)^2
    
    return(ss)
    
  }
  fit2 <- optim(c(alpha1, beta1),gamma.ss, method="CG")
  alpha.out<-fit2$par[1]
  beta.out<-fit2$par[2]
  mode.out<-(alpha.out-1)/beta.out
  params <- list("alpha" = alpha.out,"beta" = beta.out, "mode"=mode.out)
  return(params)
}

ui <- tagList(
  navbarPage(
    "Expert Elicitation - Test Values",
    
    
    tabPanel("Overview",
             sidebarPanel(
               h1("Visualize your elicited values")
             ),
             mainPanel(
               tags$head(
                 tags$style(type='text/css', 
                            ".nav-tabs {font-size: 20px} ")),
               tabsetPanel(
                 tabPanel(h3("Instructions"),
                          h4("All you need to do is navigate to the 'Test your values' tab, then enter your values for 
                             either a probability or a non-probability."),
                          h4(""),
                          h4("Once you have entered in those values, hit the 'Simulate' button and 
                             a plot will show up"),
                          h4(""),
                          h4("You can re-enter values as many times as you would like."),
                          h4(""),
                          h4("Note that it will take a second to make plots once you hit the 'Simulate' button."),
                          h4(""),
                          h4("Note the 'Probability' tab will simulate values from 0 to 1 and 
                             the 'Non-probability' tab will simulate values from 0 to greater than 1."), 
                          h4("An example of a non-probability value would be the number of fledglings per nest.")
                 ),
                 
               )
             )
    ),
    tabPanel("Test your values",
             tabsetPanel(
               tabPanel("Probability",
                        sidebarPanel(h3("To visualize your values for a probability, enter in 
                                        your values below. Ensure that values are between 0 and 1"),
                                     h3(""),
                                     h4("What is your best guess for the parameter?"),
                                     numericInput("best1", "Best estimate",value=NULL, min = 0, max = 1),
                                     h4("What is the lowest possible value of the parameter?"),
                                     numericInput("min1", "Minimum Estimate", value=NULL, min=0,max=1),
                                     h4("What is the highest possible value of the parameter?"),
                                     numericInput("max1", "Maximum Estimate", value=NULL, min=0,max=1),
                                     h4("How confident are you that your interval, 
                                        lowest possible value to highest possible 
                                        value, captures the true value? 
                                        Provide your confidence from 50% to 100%."),
                                     numericInput("conf1", "Confidence in % from 50 to 100", value=NULL, min=50, max=100),
                                     actionButton("simulate1", "Simulate",
                                                  style = "background-color:white;
                                            color:black;
                                            border-color:grey;
                                            border-width:2px;
                                            font-size:20px;")
                                     
                        ),
                        mainPanel(
                          plotOutput("hist1", width="70%"),
                          )),
               
               tabPanel("Non-probability",
                        sidebarPanel(h3("To visualize your values for a non-probability (numbers from 0 to one or greater than one),
                                      enter your values below."),
                                     h3(""),
                                     h4("What is your best guess for the parameter?"),
                                     numericInput("best3", "Best estimate",value=NULL, min = 0),
                                     h4("What is the lowest possible value of the parameter?"),
                                     numericInput("min3", "Minimum Estimate", value=NULL, min=0),
                                     h4("What is the highest possible value of the parameter?"),
                                     numericInput("max3", "Maximum Estimate", value=NULL, min=0),
                                     h4("How confident are you that your interval, 
                                        lowest possible value to highest possible 
                                        value, captures the true value? 
                                        Provide your confidence from 50% to 100%."),
                                     numericInput("conf3", "Confidence in % from 50 to 100", value=NULL, min=50, max=100),
                                     actionButton("simulate3", "Simulate",
                                                  style = "background-color:white;
                                            color:black;
                                            border-color:grey;
                                            border-width:2px;
                                            font-size:20px;")
                        ),
                        mainPanel(
                          plotOutput("hist3", width="70%"),
                        )),
               
               
             )
    ),
  ),
  tags$style(type = 'text/css', '.navbar {font-size: 22px; }',
             '.navbar-dropdown { font-size: 22px;}',
             '.navbar-default .navbar-brand {font-style:italic;}')
)



server = function(input, output) {
  
  x11_raw<-eventReactive(input$simulate1,{
    temp1<-suppressWarnings(beta.fit1(min.data=input$min1,ml.data=input$best1,
                     max.data=input$max1,conf=(input$conf1/100)))
    rbeta(1000000,shape1 = temp1$alpha, shape2=temp1$beta)
    
  })
  x12_stan<-eventReactive(input$simulate1,{

    tempx<-suppressWarnings(beta.fit1(min.data=input$min1,ml.data=input$best1,
                     max.data=input$max1,conf=(input$conf1/100)))
    tempxq<-qbeta(c(0.05,0.95), tempx$alpha, tempx$beta) 
    temptemp90<-suppressWarnings(beta.fit1(tempxq[1], input$best1, tempxq[2], (input$conf1/100)))
    rbeta(1000000, temptemp90$alpha, temptemp90$beta)
  })
  x123_stan<-eventReactive(input$simulate1,{

    tempx<-suppressWarnings(beta.fit1(min.data=input$min1,ml.data=input$best1,
                                      max.data=input$max1,conf=(input$conf1/100)))
    tempxq<-qbeta(c(0.005,0.995), tempx$alpha, tempx$beta) 
    temptemp99<-suppressWarnings(beta.fit1(tempxq[1], input$best1, tempxq[2], (input$conf1/100)))
    rbeta(1000000, temptemp99$alpha, temptemp99$beta)
  })
  output$hist1 <- renderPlot({
    outviolin<-data.frame(ID=c(rep(paste0(input$conf1, "% confidence"),length((x11_raw()))), 
                               rep("90% confidence",length((x12_stan()))),
                               rep("99% confidence",length((x123_stan())))),
                          value=c((x11_raw()),(x12_stan()), x123_stan()))
    pv<-ggplot(outviolin, aes(x=ID, y=value, fill=ID))+geom_violin(show.legend = F, trim=T, scale="width")+
      theme_bw(base_size=16)+labs(y="Probability",x="")+expand_limits(y=c(0,1))
    pv 
  }, res = 96)
  

  x3<-eventReactive(input$simulate3,{

    temp1g<-suppressWarnings(gamma.fit(min.data=input$min3,ml.data=input$best3,
                                       max.data=input$max3,conf=(input$conf3/100)))
    rgamma(1000000,shape = temp1g$alpha, rate=temp1g$beta)
  })
  x33<-eventReactive(input$simulate3,{
    tempxg<-suppressWarnings(gamma.fit(min.data=input$min3,ml.data=input$best3,
                                       max.data=input$max3,conf=(input$conf3/100)))
    tempg<-qgamma(c(0.05,0.95), shape=tempxg$alpha,rate=tempxg$beta) 
    temptempg<-suppressWarnings(gamma.fit(tempg[1], input$best3, tempg[2], (input$conf3/100)))
    rgamma(1000000, shape= temptempg$alpha, rate = temptempg$beta)

    
  })
  x333<-eventReactive(input$simulate3,{
    tempxg<-suppressWarnings(gamma.fit(min.data=input$min3,ml.data=input$best3,
                                       max.data=input$max3,conf=(input$conf3/100)))
    tempg<-qgamma(c(0.005,0.995), shape=tempxg$alpha,rate=tempxg$beta) 
    temptempg99<-suppressWarnings(gamma.fit(tempg[1], input$best3, tempg[2], (input$conf3/100)))
    rgamma(1000000, shape= temptempg99$alpha, rate = temptempg99$beta)
    
  })
  output$hist3 <- renderPlot({
    outviolin3<-data.frame(ID=c(rep(paste0(input$conf3, "% confidence"),length(x3())), 
                                rep("90% confidence",length(x33())),
                                rep("99% confidence",length(x333()))),
                           value=c(x3(),x33(),x333()))
    pv3<-ggplot(outviolin3, aes(x=ID, y=value, fill=ID))+geom_violin(show.legend=F)+
      theme_bw(base_size=16)+labs(y="Value",x="")+ylim(c(0,max(x3())))
    pv3
  }, res = 96)
  
  
}
shinyApp(ui, server)


