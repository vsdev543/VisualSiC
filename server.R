server<-function(input,output,session){
  
  r<-reactiveValues(dSteps=NULL,cbmc=NULL,procViz=FALSE)
  
  df<-reactive({
    req(input$genDIn)
    
    df<-as.matrix(fread(input$genDIn$datapath,sep = "\t",header = T),rownames=1)
    
    df
  })
  
  dfa<-reactive({
    read.delim(input$annDIn$datapath, row.names=1)
  })
  
  
  output$genDdt<-renderDT({
    req(input$genDIn)
    head(df(),10)
  },options=list(scrollX=T, dom = 't'),server = FALSE)
  
  
  output$annDdt<-renderDT({
    req(input$annDIn)
    dfa()
  })
  

  output$dPro<-renderUI({
    func<-input$func
    
    div(style="background-color:#fff;color:#333;padding:10px;border-radius:10px;",
      h4(func),
    lapply(names(args[[func]]),function(arg){
      type<-args[[func]][[arg]][["type"]]
      value<-eval(parse(text = args[[func]][[arg]][["value"]]))
      
      if(type=='logical'){
        checkboxInput(inputId = paste(func,arg),label = arg,value = value)
      }else if(type=='numeric'){
        numericInput(inputId = paste(func,arg),label = arg,value = value,width = "100%")
      }else if(type=='numeric_range'){
        fluidRow(
          column(6,
        numericInput(inputId = paste(func,arg,"1"),label = paste(arg,"- from"),value = value[1],width = "100%")),
        column(6,
        numericInput(inputId = paste(func,arg,"2"),label = paste(arg,"- to"),value = value[2],width = "100%")
        )
        )
      }
    }),hr(),
    actionBttn('addPr',"Add Step",style = 'stretch',color = 'primary',block = T,icon = icon('plus'))
    )
    
  })
  
  observeEvent(input$addPr,{
    func<-input$func
    
    argId<-names(args[[func]])
    
    k<-c()
    
    for(i in argId){
      type<-args[[func]][[i]][["type"]]
      val<-switch(type,
                  "logical"={input[[paste(func,i)]]},
                  "numeric"={input[[paste(func,i)]]},
                  "numeric_range"={
                    paste0(input[[paste(func,i,"1")]],":",input[[paste(func,i,"2")]])
                  }
                  )
      k<-c(k,paste(i ,"=",val))
    }
    
    k1<-paste(k,collapse = ",")
    
    if(k1==""){
      fun<-paste(func,"(",
                 paste(sep=",",
                       "object=cbmc"
                 ),
                 ")")
    }else{
    fun<-paste(func,"(",
               paste(sep=",",
                     "object = cbmc",
                     k1
               ),
               ")")
    }
    
    r$dSteps<-c(r$dSteps,fun)
    
    names(r$dSteps)[length(r$dSteps)]<-func
  })
  
  
  
  
  output$dPro_step<-renderUI({
    
    timelineBlock(
      width = 12,
      reversed = T,
      timelineEnd(color = "danger"),
      timelineLabel("Data processing", color = "pink"),
      timelineItem(
        elevation = 4,
        color = "info",
        icon=icon('cog'),
        title = "CreateSeuratObject",
        code("cbmc <- CreateSeuratObject(counts = cbmc.rna)")
      ),
      lapply(names(r$dSteps),function(step){
        timelineItem(elevation = 4,
                     color="primary",
                     icon=icon("cog"),
                     title = step,
                     tags$code(
                       paste("cbmc <-",r$dSteps[step])
                     )
                     )
      }),
      timelineItem(
        elevation = 4,
        color = "info",
        icon=icon('cog'),
        title = "AddMetaData",
        code("cbmc <- AddMetaData(object = cbmc, metadata = cell_annotation)")
      ),
      timelineStart(color = "secondary")
    )
  })

  
  observeEvent(input$doDPr,{
    req(input$genDIn)
    
    withProgress(message = "Data processing on progress",detail = "This will take a while",session = session,
                 {
    cbmc.rna<-as.sparse(df())
    
    
    setProgress(value = 0.05,message = "Creating Seurat object",session = session)
    
    cbmc<-CreateSeuratObject(counts = cbmc.rna)
    
    setProgress(value = 0.1,message = "Performing functions for the object",session = session)
    
    for(i in r$dSteps){
      cbmc<-eval(parse(text = i))
      incProgress(amount = 0.7/length(r$dSteps),message = i,session = session)
    }
    
    setProgress(value = 0.8,message = "Adding meta data",session = session)
    
    cbmc<-AddMetaData(object = cbmc, metadata = dfa())
    
    r$cbmc<-cbmc
    
    saveRDS(cbmc,"data/cbmc.Rds")
    
    rm(cbmc,cbmc.rna)
                 }
    )
    shinyalert(title = "Done",text = "Processing Done",type = 'success',timer = 5000)
    r$procViz<-T
  })
  
  
  output$downSUI<-renderUI({
    if(!is.null(r$cbmc)){
      downloadBttn(outputId = 'downCbmc',label = "Download proccessed Seurat object",style = "stretch",block = T)
      }
  })
  
  output$downCbmc<-downloadHandler(filename = "cbmc.Rds",content = function(file){
    saveRDS(r$cbmc,file)
  })
  
  # k<-readRDS("data/cbmc.Rds")
  
  # callModule(viz,id="viz",cbmc=reactive(r$cbmc))
  callModule(viz,id="viz",cbmc=reactive(r$cbmc),procViz=reactive(r$procViz),parent=session)
  # callModule(viz,id="viz")
  
  
  
  observeEvent(input$resetDPr,{
    r$dSteps<-NULL
  })
  
  # observeEvent(input$addMD,{
  #   
  # withProgress(message = "Reading annotation data file",detail = "This will take a while",session = session,{
  # 
  #   ann1<-read.delim(input$nwAnn$datapath, row.names=1)
  #   
  #   setProgress(value = 0.5,message = "Adding meta data",session = session)
  #   
  #   r$cbmc<-AddMetaData(object = r$cbmc, metadata = dfa())
  #   
  #   setProgress(value = 0.8,message = "Saving data file",session = session)
  #   
  #   saveRDS(r$cbmc,"data/cbmc.Rds")
  #   
  # })
  #   shinyalert(title = "Done",text = "Annotation added to the dataset",type = 'success',timer = 5000)
  # })
  # 
  # output$annNwdt<-renderDT({
  #   head(read.delim(input$nwAnn$datapath, row.names=1))
  # })
  
}