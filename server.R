function(input, output) {

    # Call simulated data, if "create" chosen
    simtab <- reactive({simulate(nid=input$nid,beta0=input$beta0,beta1=input$beta1,sd.y=input$sd.y,V0=input$V0,sigma0=input$sigma0,V1=input$V1,sigma1=input$sigma1,rho=input$rho,ncount=input$ncount,min.interval=input$min.interval,max.interval=input$max.interval,seed=input$seed)})
    
    output$create.data <- downloadHandler(
      filename = function() {
        paste0(getwd(),"/output/simdata.csv")
      },
      content = function(con) {
        write.csv(simtab()[[2]], con, row.names = F, na = "")
        #assign("simdata",simtab()[[2]],envir=dpm)
      }
    )
    
    output$simtab <- renderDT({simtab()[[2]]
    })


    simplot <- reactive({ggplot(aes(x=time,y=Ytime,group=id),data=simtab()[[2]])+
      geom_line()+
      geom_point()+
      theme_pubr()})
    output$simplot <- renderPlot({simplot()})

    # Call example data, if "example" chosen    
    example <- reactive({dpm$example[[2]]})
    
    output$example.data <- downloadHandler(
      filename = function() {
        paste0(getwd(),"/output/simdata.csv")
      },
      content = function(con) {
        write.csv(example(), con, row.names = F, na = "")
        #assign("simdata",example(),envir=dpm)
      }
    )
    
    output$example <- renderDT({example()})
 
    exampleplot <- reactive({ggplot(aes(x=time,y=Ytime,group=id),data=example())+
                            geom_line()+
                            geom_point()+
                            theme_pubr()})
    output$exampleplot <- renderPlot({exampleplot()})

    
    output$max.id.fit <- renderUI({numericInput("max.id.fit","Number of individuals included in the first part of the data set. (This part will be used to estimate the fixed effects, residuals SD, correlation, and SD of the random effects)",input$nid-1,min=10,max=input$nid-1)})
    
    output$id.update<- renderUI({numericInput("id.update","Enter the individual ID, of whom the random effects and the future outcome will be predicted.",input$nid,min=11,max=input$nid)})
    
    dattab <- read.csv("output/simdata.csv")
    #dattab <- dpm$simdata
    output$timeID<- renderUI({selectInput("time.plot","Enter the time point, from which the future outcome will be predicted.",choices=dattab$time[dattab$id==input$id.update] )})
    
    # Run predict function
    predict <- eventReactive(input$go, {
   # reactive({
    withProgress(message = 'Running MCMC ...', value = 0, {
    for (i in dattab$time[dattab$id==input$id.update]) {run_predict(dattab=dattab,max.id.fit=input$max.id.fit,id.update=input$id.update,start.time.update=i,seed=input$seed,nt=input$nt,nc=input$nc,nb=input$nb,ni=input$ni)
      cat(i)
      # Incremental Progress Bar (add some more info if neccessary)
      incProgress(1/max(dattab$time[dattab$id==input$id.update]),detail  = paste0("First timepoint with outcome to be predicted: ",i))
      
      # Pause
      Sys.sleep(0.1)
      
    }
    
    })
    })
    output$predict <- renderText({predict()})

    summary.func <- eventReactive(input$goplot, {
      summaryfunc(dattab=dattab,re="y",re0="Ytime",max.id.fit=input$max.id.fit,id.update=input$id.update,time.plot=as.numeric(input$time.plot))})
    
    output$summary <- renderPlot({summary.func()})
      
    plot.func <- eventReactive(input$goplot2, {plotfunc(dattab=dattab,re="y",re0="Ytime",max.id.fit=input$max.id.fit,id.update=input$id.update)})
    output$plot <- renderPlot({plot.func()})
}