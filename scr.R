
mrk1<-data.frame(external_gene_name=names(mrk),Fold=mrk$avg_log2FC,FDR=mrk$p_val)



# add a grouping column; default value is "not significant"
mrk1$group <- "NotSignificant"

# for our plot, we want to highlight 
# FDR < 0.05 (significance level)
# Fold Change > 1.5

# change the grouping for the entries with significance but not a large enough Fold change
mrk1[which(mrk1['FDR'] < 0.05 & abs(mrk1['Fold']) < 1.5 ),"group"] <- "Significant"

# change the grouping for the entries a large enough Fold change but not a low enough p value
mrk1[which(mrk1['FDR'] > 0.05 & abs(mrk1['Fold']) > 1.5 ),"group"] <- "FoldChange"

# change the grouping for the entries with both significance and large enough fold change
mrk1[which(mrk1['FDR'] < 0.05 & abs(mrk1['Fold']) > 1.5 ),"group"] <- "Significant&FoldChange"


# Find and label the top peaks..
top_peaks <- mrk1[with(mrk1, order(Fold, FDR)),][1:5,]
top_peaks <- rbind(top_peaks, mrk1[with(mrk1, order(-Fold, FDR)),][1:5,])


# Add gene labels for all of the top genes we found
# here we are creating an empty list, and filling it with entries for each row in the dataframe
# each list entry is another list with named items that will be used by Plot.ly
a <- list()
for (i in seq_len(nrow(top_peaks))) {
  m <- top_peaks[i, ]
  a[[i]] <- list(
    x = m[["Fold"]],
    y = m[["FDR"]],
    text = m[["external_gene_name"]],
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 0.5,
    ax = 20,
    ay = -40
  )
}


plot_ly(data = mrk1, x = ~Fold, y = ~FDR, text = ~external_gene_name, mode = "markers", color = ~group) %>% 
  layout(title ="Volcano Plot") %>%
  layout(annotations = ~a)



















