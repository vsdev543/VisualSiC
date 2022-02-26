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
  "RunPCA"=list("verbose"=list(type="binary",value="FALSE")),
  "FindNeighbors"=list("dims"=list(type='numeric_range',value="c(1,30)")),
  "FindClusters"=list("resolution"=list(type="numeric",value="0.8"),"verbose"=list(type="binary",value="FALSE")),
  "RunUMAP"=list("min.dist"=list(type="numeric",value="0.3"),"spread" = list(type="numeric",value='1'),"dims"=list(type='numeric_range',value="c(1,30)"))
)

callToSeurat<-function(func,args){
  
  # func<-"FindVariableFeatures"
  k1<-lapply(names(args[[func]]),function(i){
    if (args[[func]][[i]][["type"]] != "numeric_range") {
      parse(text = args[[func]][[i]][["value"]])
    } else{
      t<-args[[func]][[i]][["value"]]
      paste0(eval(parse(text = t))[1],':',eval(parse(text = t))[2])
    }
  })
  
  names(k1)<-names(args[[func]])
  
k1
}



cbmc <- CreateSeuratObject(counts = cbmc.rna)

cbmc <- NormalizeData(cbmc)
cbmc <- FindVariableFeatures(cbmc)
cbmc <- ScaleData(object = cbmc)
cbmc <- RunPCA(cbmc, verbose = FALSE)
cbmc <- FindNeighbors(cbmc, dims = 1:30)
cbmc <- FindClusters(cbmc, resolution = 0.8, verbose = FALSE)
cbmc <- RunUMAP(cbmc, min.dist = 0.3, spread = 1, dims = 1:30)

## Adding metadata to the structure
cbmc <- AddMetaData(object = cbmc, metadata = cell_annotation, col.name = 'Cluster')

func<-"FindNeighbors"

arg<-callToSeurat(func = func,args = args)

# arg$object=cbmc

# cbmc<-do.call(func,args = arg)


fun<-parse(text = paste(
  func,'(',
  paste(sep=",",
  "object = cbmc",
  paste(collapse = ",",
        lapply(names(arg),function(a){
          paste(a,'=',arg[[a]])
        })
  )
  )
  ,')'
))

eval(fun)





cbmc<-readRDS("data/cbmc.Rds")

head(cbmc@meta.data)


FeaturePlot(cbmc,"MYH11")

test.seurat <- subset(cbmc, subset = Cluster == "C2 (CM)")







