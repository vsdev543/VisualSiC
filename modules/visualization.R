viz_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
             plotlyOutput(ns('genePlot'),height = "590px")
             ),
      column(4,
             # selectizeInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%"),
             selectizeInput(ns('gene_name'), 'Gene', choices = NULL,width = "100%",multiple=T),
             selectizeInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%",multiple=T),
             uiOutput(ns('snapui'))
             )
    ),
    hr(),
    uiOutput(ns("main_viz"))
  )
}

viz <- function(input, output, session,cbmc) {
# viz <- function(input, output, session) {
  
  # cbmc<-cbmcx()
  # cbmc<-readRDS("data/cbmc.Rds")
  
  observe({
    k<-rownames(
      as.matrix(cbmc()@assays$RNA@data)
    )
    
    if(!is.null(cbmc())){
      updateSelectizeInput(session = session,inputId = "gene_name",choices = k,server = T,selected = k[1:10])
      updateSelectizeInput(session = session,inputId = "cluster_name",choices = cbmc()@meta.data$Cluster,server = T)
      }
  })
  
  
  output$main_viz<-renderUI({
    div(class="grid-2",
        renderPlot(VariableFeaturePlot(cbmc())),
        renderPlotly(DimPlot(cbmc(), reduction = "pca")),
        renderPlotly(ElbowPlot(cbmc())),
        renderPlotly(DimPlot(cbmc(), reduction = "umap"))
        
        )
  })
  
  output$genePlot<-renderPlotly({
    req(input$gene_name)
    
    obj<-cbmc()
    
    if(!is.null(input$cluster_name)){
      obj<-subset(obj, subset = Cluster %in% input$cluster_name)
    }
    
    plot<-FeaturePlot(obj, input$gene_name)
    
    plot
  })
  
  output$snapui<-renderUI({
    renderPlotly(
      VlnPlot(cbmc(),input$gene_name)
    )
  })
  
  output$cbarUI<-renderUI({
    tagList(
      bs4Card(width = 12,
              title = "Cluster",collapsible = T,closable = F,collapsed = T,maximizable = F,label = "lmnop",
              "Osanda"
              )
    )
  })
}





