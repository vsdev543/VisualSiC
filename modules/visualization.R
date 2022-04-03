viz_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9,
             plotlyOutput(ns('genePlot'),height = "590px")%>%withSpinner(),
             hr(),
             fluidRow(
               column(6,
                      plotlyOutput(ns('hist1'))
                      ),
               column(6,
                      plotlyOutput(ns('hist2'))
                      )
             ),
             DTOutput(ns("testDT"))
             ),
      column(3,
             selectizeInput(ns('gene_name'), 'Gene', choices = NULL,width = "100%",multiple=F),
             sliderInput(ns('gene_val'),label = "Range",min = 0,max = 10,step = 0.01,value = c(1,10),width = "100%"),
             # selectizeInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%",multiple=T),
             pickerInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%",multiple=T,options = list(`actions-box` = TRUE,`live-search`=TRUE)),
             awesomeCheckbox(ns('hideNa'),"Hide Other clusters",value = F),
             uiOutput(ns('otherF'))
             )
    )
  )
}

viz <- function(input, output, session,cbmc,procViz,parent) {
  ns<-session$ns

  cbmcD<-reactive({
    if(!procViz()){
      if(file.exists("data/cbmc.Rds")){
      readRDS("data/cbmc.Rds")}else{
        NULL
      }
    }else{
      cbmc()
    }
    })
  

  
  observe({
    req(cbmcD())
    updateSelectizeInput(session=session,inputId = 'gene_name',choices = rownames(cbmcD()),server = T)
  })
  
  fetchx<-reactive({
    k<-c('Cluster','nCount_RNA','nFeature_RNA')
    
    if(!is.null(input$metaTfilter)){
      k<-c(k,input$metaTfilter)
      k<-unique(k)
    }
    
    FetchData(object = cbmcD(), vars = c("UMAP_2", "UMAP_1", input$gene_name,k))
  })
  
  observeEvent(input$gene_name,{
    
    req(input$gene_name)
    # updateSelectizeInput(session=session,inputId = 'cluster_name',
    #                      choices = c("All",fetchx()$Cluster),
    #                      selected = "All",
    #                      server = T
    # )
    
    updatePickerInput(session=session,inputId = 'cluster_name',
                         choices = unique(fetchx()$Cluster),
                         selected = unique(fetchx()$Cluster)
    )
    
    updateSliderInput(session=session,inputId = 'gene_val',
                      label = paste(input$gene_name, "range"),
                      min = floor(min(fetchx()[,input$gene_name],na.rm = T)),
                      max = ceiling(max(fetchx()[,input$gene_name],na.rm = T)),
                      value = c(min(fetchx()[,input$gene_name],na.rm = T),max(fetchx()[,input$gene_name],na.rm = T))
                      )
  })
  
#######################################################################################################
  
  output$genePlot<-renderPlotly({
    
    gene<-input$gene_name
    data<-fetch()
    

    # if(!input$hideNa){
    # data<-data%>%
    #   filter((!!as.symbol(gene)>=input$gene_val[1] & !!as.symbol(gene)<=input$gene_val[2]) | is.na(!!as.symbol(gene)))
    # }else{
    #   data<-data%>%
    #     filter(!!as.symbol(gene)>=input$gene_val[1] & !!as.symbol(gene)<=input$gene_val[2])
    # }
    
    
    if(!all(is.na(data[,gene]))){
    if(nrow(data)>0){
    plot_ly(data=data,x=~UMAP_1,y=~UMAP_2,color = data[,gene],
            type = "scatter",mode='markers',
            transforms = list(
              list(
                type = 'groupby',
                groups = data$Cluster
              ))
    )%>%
      layout(
        xaxis = list(range = list(min(fetch()$UMAP_1,na.rm = T)-1, max(fetch()$UMAP_1,na.rm = T)+1)),
        yaxis = list(range = list(min(fetch()$UMAP_2,na.rm = T)-1, max(fetch()$UMAP_2,na.rm = T)+1))
      )}else{
        NULL
      }
      }
  })
  
  
  output$cbarUI<-renderUI({
    tagList(
    tags$i("Default annotation, 'Cluster' is always available to filter the data from. If more metadata needs to be accessed to filter data. Use below to add them"),
    hr(),
    checkboxGroupInput(ns('metaTfilter'),"Metadata column names",width = "100%",choices = names(cbmcD()@meta.data)[names(cbmcD()@meta.data)!='Cluster'])
    )
    })
  
  
  output$otherF<-renderUI({
    if(is.null(input$metaTfilter)){
      tags$i("You can add more metadata to filter, from the control panel on the right.")
    }else{
      lapply(input$metaTfilter,function(i){
        tagList(
          if(class(fetchx()[,i])%in%c("factor",'charachter')){
            # selectInput(ns(paste0('subi',i)),i,choices = c("All",unique(fetchx()[,i])),selected = "All",multiple = T,width = '100%')
            pickerInput(ns(paste0('subi',i)),i,choices = unique(fetchx()[,i]),multiple = T,selected = unique(fetchx()[,i]),width = "100%",options = list(`actions-box` = TRUE,`live-search`=TRUE))
          }else if(class(fetchx()[,i])%in%c("integer",'numeric')){
            sliderInput(ns(paste0('subi',i)),i,min = floor(min(fetchx()[,i])),max = ceiling(max(fetchx()[,i])),value = c(min(fetchx()[,i]),max(fetchx()[,i])),width = "100%")
          }
        )
      })
    }
  })
  
  
  fetch<-reactive({
    k<-fetchx()
    
    l<-c(input$cluster_name)
    
    l[l=="NA"]<-NA
    
    if(!'All'%in%l & !is.null(l)){
      k[!k$Cluster%in%l,input$gene_name]<-NA
    }
    
    
    
    
    if(!is.null(input$metaTfilter)){
    for(i in input$metaTfilter){
      val<-input[[paste0(paste0('subi',i))]]
      
      if(class(fetchx()[,i])%in%c("factor",'charachter')){
        k[!k[,i]%in%val,input$gene_name]<-NA
      }
      
      if(class(fetchx()[,i])%in%c("integer",'numeric')){
        k<-k[k[,i]>val[1] & k[,i]<val[2],]
      }
      
    }
    }
    
    gene<-input$gene_name
    
    if(!input$hideNa){
      k<-k%>%
        filter((!!as.symbol(gene)>=input$gene_val[1] & !!as.symbol(gene)<=input$gene_val[2]) | is.na(!!as.symbol(gene)))
    }else{
      k<-k%>%
        filter(!!as.symbol(gene)>=input$gene_val[1] & !!as.symbol(gene)<=input$gene_val[2])
    }
    
    k
  })
  
  
  
  output$testDT<-renderDT(fetch())
  
  
  output$hist1<-renderPlotly({
    data<-fetchx()[!is.na(fetchx()[,input$gene_name]),]
    
    plot_ly(x = data[,"nCount_RNA"], type = "histogram")%>%
      layout(
        xaxis=list(title="nCount_RNA"),
        yaxis=list(title="Count"),
        title="nCount_RNA Histogram"
      )
  })
  
  output$hist2<-renderPlotly({
    data<-fetchx()[!is.na(fetchx()[,input$gene_name]),]
    
    plot_ly(x = data[,"nFeature_RNA"], type = "histogram")%>%
      layout(
        xaxis=list(title="nFeature_RNA"),
        yaxis=list(title="Count"),
        title="nFeature_RNA Histogram"
      )
  })
  
}





