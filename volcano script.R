rm(list=ls())

library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
setwd("C:/Users/ASUS/OneDrive/Desktop/Project/Volcano Plots")
df<- read.table("severevshealthy_degresults-1.txt", sep = ",", header = TRUE, stringsAsFactors = FALSE, row.names = 1)
head(df)

#set Theme
theme_set(theme_classic(base_size = 20) +
            theme(
              axis.title.y = element_text(face = "bold", margin = margin(0,20,0,0), size = rel(1.1), color = 'black'),
              axis.title.x = element_text(hjust = 0.5, face = "bold", margin = margin(20,0,0,0), size = rel(1.1), color = 'black'),
              plot.title = element_text(hjust = 0.5)
            ))


df$diffexpressed<-"NO"
df$diffexpressed[df$log2fc < -0.6 & df$pval < 0.05]<-"Down"
df$diffexpressed[df$log2fc > 0.6 & df$pval < 0.05]<-"UP"

unique(df$diffexpressed)
# Create a new column "delabel" to de, that will contain the name of 
#the top 30 differentially expressed genes (NA in case they are not)

df$delabel <- ifelse(df$gene_symbol %in% head(df[order(df$padj), "gene_symbol"], 30),
                     df$gene_symbol, NA)




ggplot(data = df, aes(x = log2fc, y = -log10(pval), col = diffexpressed, label = delabel)) +
  geom_vline(xintercept = c(-0.6, 0.6), col = "gray", linetype = 'dashed') +
  geom_hline(yintercept = -log10(0.05), col = "gray", linetype = 'dashed') + 
  geom_point(size = 2) + 
  scale_color_manual(values = c("#00AFBB", "grey", "#bb0c00"), # to set the colours of our variable  
                     labels = c("Downregulated", "Not significant", "Upregulated")) + # to set the labels in case we want to overwrite the categories from the dataframe (UP, DOWN, NO)
  coord_cartesian(ylim = c(0, 250), xlim = c(-10, 10)) + # since some genes can have minuslog10padj of inf, we set these limits
  labs(color = 'Severe', #legend_title, 
       x = expression("log"[2]*"FC"), y = expression("-log"[10]*"p-value")) + 
  scale_x_continuous(breaks = seq(-10, 10, 2)) + # to customise the breaks in the x axis
  ggtitle('Thf-like cells in severe COVID vs healthy patients') + # Plot title 
  geom_text_repel(max.overlaps = Inf) # To show all labels 

  
  
  
  
  
  
  
  
  
  
  
