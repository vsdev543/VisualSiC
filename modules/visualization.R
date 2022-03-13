viz_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(9,
             plotlyOutput(ns('genePlot'),height = "590px")%>%withSpinner()
             ),
      column(3,
             # selectizeInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%"),
             selectizeInput(ns('gene_name'), 'Gene', choices = NULL,width = "100%",multiple=F),
             sliderInput(ns('gene_val'),label = "Range",min = 0,max = 10,step = 0.01,value = c(1,10),width = "100%"),
             selectizeInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%",multiple=T),
             awesomeCheckbox(ns('hideNa'),"Hide Other clusters",value = F)
             )
    )#,
    # hr(),
    # uiOutput(ns("main_viz"))
    # ,DTOutput(ns('someD'))
  )
}

viz <- function(input, output, session,cbmc,procViz) {
# viz <- function(input, output, session) {
  
  # cbmc<-cbmcx()
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
  
  #####
  # observe({
  #   k<-rownames(
  #     as.matrix(cbmc()@assays$RNA@data)
  #   )
  #   
  #   if(!is.null(cbmc())){
  #     updateSelectizeInput(session = session,inputId = "gene_name",choices = k,server = T,selected = k[1:10])
  #     updateSelectizeInput(session = session,inputId = "cluster_name",choices = cbmc()@meta.data$Cluster,server = T)
  #     }
  # })
  # 
  # 
  # output$main_viz<-renderUI({
  #   div(class="grid-2",
  #       renderPlot(VariableFeaturePlot(cbmc())),
  #       renderPlotly(DimPlot(cbmc(), reduction = "pca")),
  #       renderPlotly(ElbowPlot(cbmc())),
  #       renderPlotly(DimPlot(cbmc(), reduction = "umap"))
  #       
  #       )
  # })
  # 
  # output$genePlot<-renderPlotly({
  #   req(input$gene_name)
  #   
  #   obj<-cbmc()
  #   
  #   if(!is.null(input$cluster_name)){
  #     obj<-subset(obj, subset = Cluster %in% input$cluster_name)
  #   }
  #   
  #   plot<-FeaturePlot(obj, input$gene_name)
  #   
  #   plot
  # })
  # 
  # output$snapui<-renderUI({
  #   renderPlotly(
  #     VlnPlot(cbmc(),input$gene_name)
  #   )
  # })
  # 
  # output$cbarUI<-renderUI({
  #   tagList(
  #     bs4Card(width = 12,
  #             title = "Cluster",collapsible = T,closable = F,collapsed = T,maximizable = F,label = "lmnop",
  #             "Osanda"
  #             )
  #   )
  # })
  ######
  
  observe({
    req(cbmcD())
    updateSelectizeInput(session=session,inputId = 'gene_name',choices = rownames(cbmcD()),server = T)
  })
  
  fetchx<-reactive({
    FetchData(object = cbmcD(), vars = c("UMAP_2", "UMAP_1", input$gene_name,"Cluster"))
  })
  
  observeEvent(input$gene_name,{
    
    req(input$gene_name)
    updateSelectizeInput(session=session,inputId = 'cluster_name',
                         choices = c("All",fetchx()$Cluster),
                         selected = "All",
                         server = T
    )
    
    updateSliderInput(session=session,inputId = 'gene_val',
                      label = paste(input$gene_name, "range"),
                      min = floor(min(fetchx()[,input$gene_name],na.rm = T)),
                      max = ceiling(max(fetchx()[,input$gene_name],na.rm = T)),
                      value = c(min(fetchx()[,input$gene_name],na.rm = T),max(fetchx()[,input$gene_name],na.rm = T))
                      )
  })
  
  observe({
    req(input$gene_name)
    
    print(
      min(fetchx()[,input$gene_name])
    )
  })
  
  fetch<-reactive({
    k<-fetchx()
    
    if(!'All'%in%input$cluster_name){
      k[!k$Cluster%in%input$cluster_name,input$gene_name]<-NA
    }
    k
  })
  
  output$genePlot<-renderPlotly({
    
    gene<-input$gene_name
    # data<-fetch()[!is.na(fetch()[,gene]),]
    # dataNa<-fetch()[is.na(fetch()[,gene]),]
    data<-fetch()
    
    
    if(!input$hideNa){
    data<-data%>%
      filter((!!as.symbol(gene)>=input$gene_val[1] & !!as.symbol(gene)<=input$gene_val[2]) | is.na(!!as.symbol(gene)))
    }else{
      data<-data%>%
        filter(!!as.symbol(gene)>=input$gene_val[1] & !!as.symbol(gene)<=input$gene_val[2])
    }
    
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
      )
  })
  
}





