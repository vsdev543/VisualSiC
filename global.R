library(renv)
library(shiny)
library(bs4Dash)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(shinyalert)

library(data.table)
library(DT)
library(plotly)

library(ggplot2)

library(devtools)
library(readr)
library(tidyr)
library(purrr)
library(dplyr)  
library(Seurat)
library(umap)
library(gridExtra)


options(shiny.maxRequestSize=30*1024^3)


useCss<-function(file="style.css"){
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = file)
  )
}


modalNames<-c(
  "NormalizeData",
  "FindVariableFeatures",
  "ScaleData",
  "RunPCA",
  "FindNeighbors",
  "FindClusters",
  "RunUMAP"
)


args=list(
  "NormalizeData"=list(),
  'FindVariableFeatures'=list(),
  "ScaleData"=list(),
  "RunPCA"=list("verbose"=list(type="logical",value="FALSE")),
  "FindNeighbors"=list("dims"=list(type='numeric_range',value="c(1,30)")),
  "FindClusters"=list("resolution"=list(type="numeric",value="0.8"),"verbose"=list(type="logical",value="FALSE")),
  "RunUMAP"=list("min.dist"=list(type="numeric",value="0.3"),"spread" = list(type="numeric",value='1'),"dims"=list(type='numeric_range',value="c(1,30)"))
)

source("modules/visualization.R")