#Libraries used
library("readxl")
library("tidyverse")
library("openxlsx")
library("missForest")
library("writexl")
library("pheatmap")
library("dplyr")

#Import dataset into Environment. 
read_excel("C:/Users/data.xlsx",
           sheet = "Sheet4",
           col_names = TRUE)
#Rename the data set. This term "res" can be replaced with anything that comes to mind, there is no function here.
res <- read_excel("C:/Users/data.xlsx",
                  sheet = "Sheet4",
                  col_names = TRUE)
view(res) 
#Set gene ID (symbol) as row names. This changes that column 1 disappears and the gene or protein names are listed.
rer <- res%>%select(-Genes)
rownames(rer) <- res$Genes
view(rer)

#transpose the data
vert <- t(rer)
view(vert)

#MissForest: will impute missing values 
noi <- missForest::missForest(as.matrix(vert),verbose = T)
noir <- noi$ximp
view (noir)

#Turn the data back to a data frame
sd <- t(noir)
sdd <- data.frame(sd)
view(sdd)

sdd <- data.frame(sdd)
view(sdd)

#export the data as an excel file
write_xlsx(sdd,"C:/Users/IMP.xlsx", col_names = TRUE)

#Open the new "imputed" data set. Before check that the removed column 1 (gene/protein names) is added back into the file.
#Create annotation 
#OBS! check that the annotation count matches the sample count. 
annotation <- data.frame(Condition = factor(c("Control", "Test","Control")))
view(annotation)
rownames(annotation) <- colnames(sdd)
annotation_colors <- list(Condition = c("Control" = "red", "Test" = "blue"))
heatmap_colors <- colorRampPalette(c("magenta", "cyan"))(n = 10)

#Create the heatmap. Here I can now modify a bit more, based on what I would like the heatmap to look like.
pheatmap(rer, scale = "row", 
         cluster_rows = TRUE,
         clustering_method = "ward.D2",
         clustering_distance_rows = "euclidean", 
         clustering_distance_cols = "euclidean",
         show_rownames = F,
         show_colnames = TRUE,
         treeheight_row = 0, 
         angle_col = "45",
         fontsize = 5,
         color = heatmap_colors)

??pheatmap

