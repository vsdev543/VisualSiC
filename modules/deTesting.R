de_UI <- function(id){
  ns <- NS(id)
  tagList(
  h1("Differential expression testing"),
  tags$i(h4(textOutput(ns('info')))),
  fluidRow(
    column(9,
           plotlyOutput(ns('volcano'),height = "600px")
    ),
    column(3,
           div(style="background-color:#fff;padding:10px; border-radius:10px;",
           pickerInput(inputId = ns('ident'),choices = NULL,multiple = F,label = "Identity class",selected = NULL,width = "100%"),
           pickerInput(inputId = ns('ident1'),choices = NULL,multiple = F,label = "Ident 1",width = "100%"),
           pickerInput(inputId = ns('ident2'),choices = NULL,multiple = F,label = "Ident 2",width = "100%"),
           actionBttn(inputId = ns('calcIn'),label = "Calculate",icon = icon('cogs'),color = 'primary',style = "stretch",block = T),
           ),
           br(),
           div(style="background-color:#fff;padding:10px; border-radius:10px;",
             h3("Significant range"),
             sliderInput(ns("pvalR"),label = "P_Val Significant range",min = 0,max = 0.5,value = 0.5,step = 0.001,width = "100%"),
             sliderInput(ns("logR"),label = "Avg_Log2FC non Significant range (Absolute)",min = 0,max = 30,value = 1.5,step = 0.01,width = "100%"),
             numericInput(ns('peakL'),"Number of peaks to label",5)
           )
           )
    
    )
  )
}

de_Server <- function(input, output, session,cbmc,procViz) {
  
  r<-reactiveValues(markers=NULL)
  
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
    if(!is.null(cbmcD())){
    updatePickerInput(session=session,inputId = 'ident',choices = names(cbmcD()@meta.data),selected = "seurat_clusters")
    }
  })
  
  observeEvent(input$ident,{
    ch<-sort(as.character(na.omit(unique(cbmcD()@meta.data[[input$ident]]))))
    updatePickerInput(session = session,inputId = "ident1",choices = ch,selected = ch[1])
    updatePickerInput(session = session,inputId = "ident2",choices = c('NULL',ch),selected = 'NULL')
  })
  
  
  
  observeEvent(input$calcIn,{
    cbmcx<-cbmcD()
    withProgress(message = "Setting indent class to",detail = input$ident,session = session,{
    cbmcx<-SetIdent(object = cbmcx,value=input$ident)
    setProgress(value = 0.2,message = "Finding markers:",detail = paste("ident.1=",input$ident1, "&", 'ident.2=',input$ident2 ),session = session)
    markers<-list(
      method=paste("Showing results for class",input$ident,"with ident.1=",input$ident1, "&", 'ident.2=',input$ident2),
      data=FindMarkers(object = cbmcx,ident.1 =input$ident1,ident.2=input$ident2))
    })
    
    saveRDS(markers,"data/markers.Rds")
    
    r$markers<-markers
    
    shinyalert(title = "Done",text = paste("Finding markers done for class",input$ident,"with ident.1=",input$ident1, "&", 'ident.2=',input$ident2 ),
               type = "success",closeOnEsc = T,closeOnClickOutside = T,timer = 2000)
  })
  
  observe({
    if(all(is.null(r$markers),file.exists("data/markers.Rds"))){
      r$markers<-readRDS("data/markers.Rds")
    }
  })
  
  output$info<-renderText({
    r$markers[[1]]
  })
  
  # output$deDt<-renderDT({
  #   r$markers[[2]]
  # })
  
  output$volcano<-renderPlotly({
    tbl<-r$markers[[2]]
    
    tbl$group<-'NotSignificant'
    
    # change the grouping for the entries with significance but not a large enough Fold change
    tbl[which(tbl$p_val <= input$pvalR & abs(tbl$avg_log2FC) <= input$logR ),"group"] <- "Significant"
    # change the grouping for the entries a large enough Fold change but not a low enough p value
    tbl[which(tbl$p_val > input$pvalR & abs(tbl$avg_log2FC) > input$logR ),"group"] <- "FoldChange"
    # change the grouping for the entries with both significance and large enough fold change
    tbl[which(tbl$p_val <= input$pvalR & abs(tbl$avg_log2FC) > input$logR ),"group"] <- "Significant&FoldChange"
    
    # Find and label the top peaks..
    top_peaks <- tbl[with(tbl, order(avg_log2FC, p_val)),][1:input$peakL,]
    top_peaks <- rbind(top_peaks, tbl[with(tbl, order(-avg_log2FC, p_val)),][1:input$peakL,])
    
    a <- list()
    for (i in seq_len(nrow(top_peaks))) {
      m <- top_peaks[i, ]
      a[[i]] <- list(
        x = m[["avg_log2FC"]],
        y = m[["p_val"]],
        text = rownames(m),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 0.5,
        ax = 20,
        ay = -40
      )
    }
    
    plot_ly(data = tbl, x = ~avg_log2FC, y = ~p_val, text = rownames(tbl), mode = "markers", color = ~group) %>% 
      layout(title ="Volcano Plot") %>%
      layout(annotations = a)
    
  })
  

  
    }