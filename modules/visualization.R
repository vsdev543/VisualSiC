viz_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
             uiOutput(ns('genePlot'))
             ),
      column(4,
             # selectizeInput(ns('cluster_name'), 'Cluster', choices = NULL,width = "100%"),
             selectizeInput(ns('gene_name'), 'Gene', choices = NULL,width = "100%"),
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
    if(!is.null(cbmc())){
    updateSelectizeInput(session = session,inputId = "gene_name",choices = rownames(
      as.matrix(cbmc()@assays$RNA@data)
    ),server = T)
      
      updateSelectizeInput(session = session,inputId = "cluster_name",choices = rownames(
        unique(cbmc()@meta.data[,"Cluster"])
      ),server = T)
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
  
  output$genePlot<-renderUI({
    req(input$gene_name)
    renderPlot(FeaturePlot(cbmc(), input$gene_name),height = 490)
  })
  
  output$snapui<-renderUI({
    renderPlotly(
      VlnPlot(cbmc(),input$gene_name)
    )
  })
}





