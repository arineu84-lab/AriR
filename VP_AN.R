#BiocManager::install('EnhancedVolcano')
#devtools::install_github('kevinblighe/EnhancedVolcano')
library("ggplot2")
library("ggrepel")
library("readxl")
library("tidyverse")
library("writexl")

#Before importing the data, make sure to have the columns labelled uniform: Genes (or Proteins), pvalue and log2FC
#Data should include Log2FC and -Log10pvalue (adjusted). Import data and the correct sheet (import 2, one is used to set the row names)
res <- read_excel("validproteins.xlsx",
                  sheet ="CvsPn",col_names = TRUE)

#Set gene ID as rownames. Make sure that each rowname is unique
rer <- res%>%select(-Proteins)
rownames(rer) <- res$Proteins
view(rer)

# Add a column to the data frame to specify if they are UP- or DOWN- regulated (log2FoldChange respectively positive or negative)
rer$Regulation <- "-"
rer$Regulation[rer$log2FC > 0.75 & rer$log10pvalue > 1] <- "Up"
rer$Regulation[rer$log2FC < -0.75 & rer$log10pvalue > 1] <- "Down"
p <- ggplot(data=rer, aes(x=log2FC, y=log10pvalue, col=Regulation)) + geom_point() 
plot(p)
view(rer)
# Re-plot but this time color the points with "Regulation" and with added lines
p2 <- p + geom_vline(xintercept=c(-0.75, 0.75), col="black") +
  geom_hline(yintercept=1, col="black")
plot(p2)

#ceate a named vector: the values are the colors to be used, the names are the categories they will be assigned to:
mycolors <- c("brown3", "chartreuse4", "darkgrey")
names(mycolors) <- c("Down", "Up", "-")
p3 <- p2 + scale_colour_manual(values = mycolors)

# Now write down the name of genes beside the points...
# Create a new column "delabel" to de, that will contain the name of genes differentially expressed (NA in case they are not)
rer$rerlabel <- NA
rer$rerlabel[rer$Regulation != "-"] <- re$Genes[rer$Regulation != "-"]
ggplot(data=rer, aes(x=log2FC, y=log10pvalue, col=Regulation, label=rerlabel)) + 
  geom_point() + 
  geom_text()

#organize the labels and cancel out too many overlaps + Change scale if necesariy (xlim)
p4 <- ggplot(data=rer, aes(x=log2FC, y=log10pvalue, col=Regulation, label=rerlabel)) +
  geom_point() + 
  xlim(-2,2) +
  geom_text_repel() +
  theme_minimal() +
  theme(text = element_text(size = 12)) +
  scale_color_manual(values=c("darkgrey", "magenta", "cyan")) +
  geom_vline(xintercept=c(-0.75, 0.75), col="black", linetype = "dashed") +
  geom_hline(yintercept=1, col="black",linetype = "dashed") +
  theme(legend.background = )

#Change label names
p4 + labs(x = "Log2FC", y = "-Log10 q-value", title = "Volcano Plot", caption = "Healthy vs non_severe") + guides(color = guide_legend(override.aes = list(size=6)))

 #export the up- and downregulated proteins as defined at row 23
write_xlsx(rer, "VP_CvsPn.xlsx", col_names = TRUE)

