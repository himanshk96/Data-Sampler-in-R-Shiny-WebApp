library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Sampler"),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Technique",
                  c("simple" = "simple",
                    "systematic" = "systematic",
                    "stratified" = "stratified",
                    "clustered"="clustered",
                    "quota"="quota",
                    "judgement"="judgement",
                    "snowball"="snowball"
                    
                  )),
      fileInput("data", h3("Choose CSV File"),multiple = FALSE),
      numericInput("size", "Size:", 0.1, min = 0, max = 1),  
      selectInput("testtyp", "Select Test Type:",
                  c("two-tailed" = "two.sided",
                    "lower-tailed" = "less",
                    "upper-tailed" = "greater")),
      conditionalPanel(condition = "input.variable=='ons'",
                       numericInput('Mean','Mean : ',500))
    ),
    mainPanel(
      
      
      print("simple Random Sampling"),
      tableOutput("simple"),
      print("stratified  Sampling"),
      tableOutput("stratify"),
      print("Systematic Sampling"),
      tableOutput("systematic"),
      #print("Snowball Sampling"),
      #tableOutput("snowball"),
      print("SMOTE(Synthetic Minority Over-sampling Technique)"),
      tableOutput("SMOTE")
      #print("ROSE"),
      #tableOutput("ROSE")
      
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  selectedInput <- reactive({
    
    switch(input$variable,
           'sample' = 'sample',
           'stratified' = 'stratified',
           'pas' = 'pas')
  })
  
  file <- reactive({
    read.csv(input$data$datapath)
  })
  
  
  output$cluster<-renderTable({
    
  })
  
  
  
  
  output$stratify<-renderTable({
    data<-file()
    data=data.frame(data)
    
    stratified <- function(df, group, size, select = NULL, 
                           replace = FALSE, bothSets = FALSE) {
      if (is.null(select)) {
        df <- df
      } else {
        if (is.null(names(select))) stop("'select' must be a named list")
        if (!all(names(select) %in% names(df)))
          stop("Please verify your 'select' argument")
        temp <- sapply(names(select),
                       function(x) df[[x]] %in% select[[x]])
        df <- df[rowSums(temp) == length(select), ]
      }
      df.interaction <- interaction(df[group], drop = TRUE)
      df.table <- table(df.interaction)
      df.split <- split(df, df.interaction)
      if (length(size) > 1) {
        if (length(size) != length(df.split))
          stop("Number of groups is ", length(df.split),
               " but number of sizes supplied is ", length(size))
        if (is.null(names(size))) {
          n <- setNames(size, names(df.split))
          message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
                  paste(n, collapse = ", "), "),\n.Names = c(",
                  paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
        } else {
          ifelse(all(names(size) %in% names(df.split)),
                 n <- size[names(df.split)],
                 stop("Named vector supplied with names ",
                      paste(names(size), collapse = ", "),
                      "\n but the names for the group levels are ",
                      paste(names(df.split), collapse = ", ")))
        }
      } else if (size < 1) {
        n <- round(df.table * size, digits = 0)
      } else if (size >= 1) {
        if (all(df.table >= size) || isTRUE(replace)) {
          n <- setNames(rep(size, length.out = length(df.split)),
                        names(df.split))
        } else {
          message(
            "Some groups\n---",
            paste(names(df.table[df.table < size]), collapse = ", "),
            "---\ncontain fewer observations",
            " than desired number of samples.\n",
            "All observations have been returned from those groups.")
          n <- c(sapply(df.table[df.table >= size], function(x) x = size),
                 df.table[df.table < size])
        }
      }
      temp <- lapply(
        names(df.split),
        function(x) df.split[[x]][sample(df.table[x],
                                         n[x], replace = replace), ])
      set1 <- do.call("rbind", temp)
      
      if (isTRUE(bothSets)) {
        set2 <- df[!rownames(df) %in% rownames(set1), ]
        list(SET1 = set1, SET2 = set2)
      } else {
        set1
      }
    }
    
    stratified(data, ncol(data), input$size)
    #stratified(data, ncol(data),input$size)
    
  })
  
  output$systematic<-renderTable({
    
    #This R code selects a systematic sample of size n from a  
    # population of size N. 
    # The values of N and n must be provided 
    sys.sample = function(N,n){
      k = ceiling(N/n)
      #ceiling(x) rounds to the nearest integer that's larger than x. 
      #This means ceiling (2.1) = 3 
      r = sample(1:k, 1)
      sys.samp = seq(r, r + k*(n-1), k)
      return(sys.samp)
      #show("The selected systematic sample is: ", sys.samp)
      # Note: the last command "\"\n" prints the result in a new line
    }
    
    
    data<-file()
    data=data.frame(data)
    print(data)
    library(dplyr)
    sizes=input$size * nrow(data)
    # To select a systematic sample, type the following command
    # providing the values of N and n
    i= sys.sample(nrow(data), sizes)
    print(i)
    i=as.vector(i)
    data[i,]
    
    
  })
  output$simple<-renderTable(
    
    {
      data<-file()
      data=data.frame(data)
      library(dplyr)
      sizes=input$size * nrow(data)
      #print(sizes)
      sample_n(data,size=sizes,replace=TRUE)
      
    }
  )
  
  output$snowball<-renderTable(
    
    {
      data<-file()
      data=data.frame(data)
      library(snowboot)
      sizes=input$size * nrow(data)
      #print(sizes)
      net <- artificial_networks[[1]]
      head(net)
      a <- LSMI(net, n.seeds = 5, n.neigh = 2)
      #sample_n(data,size=sizes,replace=TRUE)
      a
      
    }
  )
  
  
  output$ROSE<-renderTable(
    {
      data<-file()
      library(ROSE)
      data=data.frame(data)
      library(DMwR)
      sizes=input$size * nrow(data)
      #print(sizes)
      
      library(dplyr)
      sizes=input$size * nrow(data)
      #print(sizes)
      data=data[1:75,]
      #data=sample_n(data,size=sizes,replace=TRUE)
      colnames(data)[ncol(data)] <- "target"
      
      newData <- ROSE(target ~ ., data)
      
    }
  )
  
  output$SMOTE<-renderTable(
    {
      data<-file()
      
      data=data.frame(data)
      library(DMwR)
      sizes=input$size * nrow(data)
      #print(sizes)
      
      library(dplyr)
      sizes=input$size * nrow(data)
      #print(sizes)
      data=sample_n(data,size=sizes,replace=TRUE)
      colnames(data)[ncol(data)] <- "target"
      
      newData <- SMOTE(target ~ ., data,perc.over = 100,perc.under=200)
      
    }
  )
  
  output$clustered<-renderTable(
    
    {
      library(sampling)
      data<-file()
      data=data.frame(data)
      library(dplyr)
      sizes=input$size * nrow(data)
      #print(sizes)
      clu=cluster(data,clustername=names(data)[ncol(data)],size=2,method=c("srswor"))
      getdata(data,clu)
      
      
    }
  )
  
  
  
  output$introduction <- renderUI({
    choice <- selectedInput() 
    if(choice == 'ons'){
      str1 <- paste("<h2>One sample t-test</h2><br/>
                    One sample t-test is used to test whether the population mean is equal to the <br/>
                    specified value or not.")
      str2 <- paste("<h2>Assumptions</h2><br/>
                    1. The population from which, the sample drawn is assumed as Normal distribution.<br/>
                    2. The population variance mu is unknown.
                    ")
      str3 <- paste("<h2> Problem - One sample t-test</h2><br/>
                    The specimen of copper wires drawn form a large lot have the following breaking strength<br/>
                    (in kg. weight):")
      HTML(paste(str1, str2,str3, sep = '<br/>'))
    }
    else if(choice == 'tws'){
      str1 <- paste("<h2>Independent Sample t-test</h2><br/>
                    Independent sample t-test is used to check whether there is statistically significant difference between the means in two independent groups.")
      str2 <- paste("<h2>Assumptions:</h2><br/>
                    Assumptions for two sample t-test are as follows:<br/>
                    1	The two samples are independently distributed<br/>
                    2	The population from which, the two samples drawn are Normally distributed.<br/>
                    3	The two population variances are unknown (equal or unequal).<br/>
                    ")
      str3 <- paste("<h2>Problem - Two sample t-test</h2>
                    Blood pressure data from two different group of patient which are using old drug and new <br/>
                    drug respectively are given below:")
      HTML(paste(str1, str2,str3, sep = '<br/>'))
    }
    else if(choice == 'pas'){
      str1 <- paste("<h2>Paired t-test</h2><br/>
                    The paired t-test is used when we have dependent samples. It is used to compare two population means in the case of dependent. Paired t-test <br/>
                    for dependent samples is used in 'before-after' studies, or when the samples are the matched pairs.
                    ")
      str2 <- paste("<h2>Asuumptions</h2><br/>
                    1	The two samples are dependent.<br/>
                    2	The difference between the two samples are independently distributed.<br/>
                    3	The difference between the two samples are normally distributed.<br/>
                    ")
      str3 <- paste("<h2>Problem - Paired t-test</h2><br/>
                    In a test given to two groups of students, the marks obtained were as follows:
                    ")
      HTML(paste(str1, str2,str3, sep = '<br/>'))
    }
  })
  
  output$summary <- renderPrint({
    choice <- selectedInput() 
    strength <- file()
    if(choice == 'ons'){
      
      summary(strength$name)
    }
    else if(choice == 'tws'){
      summary(strength$drug_old)
      summary(strength$drug_new)
    }
    else if(choice == 'pas'){
      summary(strength$first_group - strength$second_group)
    }
  })
  
  output$norm <- renderUI({
    
    choice <- selectedInput() 
    if(choice == 'ons'){
      str1 <- paste("Test (using Student's t-statistic) whether the mean breaking strength of the lot may be taken to be 578 kg. weight (Test at 5 per cent level of significance).<br/>
                    Let mu be the mean breaking strength of copper wires.<br/>
                    The hypothesis testing problem is:<br/>
                    H_0:mu=578 against H_1:mu ne 578
                    ")
      str2 <- paste("<h2>Check the normality</h2><br/>
                    Shapiro-Wilks test is used to check the normality of the data.
                    ")
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    else if(choice == 'tws'){
      str1 <- paste("Test at 5 per cent level whether the average blood pressure is the same between the drug_old and drug_new.<br/>
                    Let mu1 and mu2 be the mean weight of two groups.<br/>
                    The hypothesis testing problem is:<br/>
                    H_0: mu1= mu2 against H_1:mu1 ne mu2
                    ")
      str2 <- paste("<h2>Check the normality</h2><br/>
                    Shapiro-Wilks test is used to check the normality of the data.
                    ")
      HTML(paste(str1, str2, sep = '<br/>'))
    }
    else if(choice == 'pas'){
      str1 <- paste("Examine the significance of difference between mean marks obtained by students of the above two groups. <br/>
                    Test at five per cent level of significance.Let mu2 be the mean marks of first group of students and mu2 be the mean mean marks of second group of students. The hypothesis testing problem is:<br/>
                    H_0:mu2-mu2=0 against H_1:mu2-mu2 ne 0 <br/>
                    ")
      str2 <- paste("<h2>Check the normality</h2><br/>
                    Shapiro-Wilks test is used to check the normality of the data.
                    ")
      HTML(paste(str1, str2, sep = '<br/>'))
    }
  })
  
  output$normality <- renderPrint({
    choice <- selectedInput() 
    strength <- file()
    if(choice == 'ons'){
      shapiro.test(strength$name)
    }
    else if(choice == 'tws'){
      shapiro.test(strength$drug_old)
      
    }
    else if(choice == 'pas'){
      resks <- ks.test(strength$first_group, strength$second_group, "pnorm")
    }
  })
  
  output$extra <- renderUI({
    
    choice <- selectedInput() 
    if(choice == 'ons'){
      str1 <- paste("The p-value of the Shapiro-Wilk Normality test is 0.05113 which is not less than 0.<br/>
                    05, we fail to reject the null hypothesis about the nomality. That is the strength <br/>
                    of wires are normally distributed.
                    ")
      HTML(paste(str1, sep = '<br/>'))
    }
    else if(choice == 'tws'){
      str1 <- paste("The p-value of the Shapiro-Wilk Normality test is 0.6729 for drug_old and 0.8612 for drug_new <br/>
                    which are both not less than 0.05, we fail to reject the null hypothesis about the nomality. <br/>
                    That is the weights of chickens are normally distributed.
                    ")
      HTML(paste(str1, sep = '<br/>'))
    }
    else if(choice == 'pas'){
      str1 <- paste("The p-value of the Kolmogorov-Smirnov Test is 0.4005 which is not less than 0.05.<br/>
                    We fail to reject the null hypothesis about the nomality.We conclude that the distribution<br/>
                    of the difference between the marks of students of two different groups of same class is normally distributed.
                    ")
      HTML(paste(str1, sep = '<br/>'))
    }
  })
  
  output$plot <- renderPlot({
    choice <- selectedInput() 
    strength <- file()
    if(choice == 'ons'){
      qqnorm(strength$name) # draw qq plot
      qqline(strength$name) # add reference line
    }
    else if(choice == 'tws'){
      qqnorm(strength$drug_old) # draw qq plot
      qqline(strength$drug_old) # add reference line
    }
  })
  
  
  
  output$test <- renderPrint({
    choice <- selectedInput() 
    strength <- file()
    if(choice == 'ons'){
      
      Result1<-t.test(strength$name,mu=input$Mean,alternative = input$testtyp,conf.level = input$siglvl)
      Result1 # display the result of t-test
    }
    else if(choice == 'tws'){
      vtest <- var.test(strength$drug_old,strength$drug_new,alternative = input$testtyp,conf.level =input$siglvl )
      vtest
    }
    else if(choice == 'pas'){
      res01 <- t.test(strehgth$first_group, strength$second_group, paired=T, alternative=input$testtyp,conf.level = input$siglvl)
      res01
    }
  })
  
  output$final <- renderUI({
    
    choice <- selectedInput() 
    if(choice == 'ons'){
      str1 <- paste("The test statistic is -1.4917 and the p-value is 0.17. As the p-value (0.17) is greater than 0.05 (level of significance)<br/>
                    we accept the null hypothesis at 0.05 level of significance.There is enough evidence to support the null hypothesis that the mean<br/>
                    strength of wires may be taken as 578 kg. weight.
                    ")
      HTML(paste(str1, sep = '<br/>'))
    }
    else if(choice == 'tws'){
      str1 <- paste("The p-value of the test is 0.2246, which is not less than 0.05, we fail to reject the null hypothesis about the equality of variances.<br/>
                    There is enough evidence to support the null hypothesis that the mean blood pressure of both groups of patients may be considered equal.<br/>
                    That is new drug has no significant impact.
                    ")
      HTML(paste(str1, sep = '<br/>'))
    }
    else if(choice == 'pas'){
      str1 <- paste("The p-value of the test is 0.377, which is not less than 0.05, we fail to reject the null<br/>
                    hypothesis about the equality of means.There is enough evidence to conclude that there is no difference in the<br/>
                    performance of students of both groups in same exam.
                    ")
      HTML(paste(str1, sep = '<br/>'))
    }
  })
  
    }

# Run the app ----
shinyApp(ui = ui, server = server)