hierarchClust <- function(group){
#Function for Hierarchical Clustering
#arguments group: either VS or MCS
library(dendextend)
library(colorspace)
library(ggplot2)
set.seed(123)

hc <- hclust(dist(group[,-1]), method = "average")
dend <- as.dendrogram(hc)
#
labels_colors(dend) <-
    rainbow_hcl(2)[sort_levels_values(
        as.numeric(group[,1])[order.dendrogram(dend)]
    )]
labels(dend) <- as.character(group[,1])[order.dendrogram(dend)]
# hang the dendrogram
dend <- hang.dendrogram(dend, hang_height=0.001) #hang = -2
# reduce the size of the labels, widht etc:
dend <- set(dend, "labels_cex", 1.2)
dend <- set(dend, "branches_lwd", 1.6)
dend <- set(dend, "leaves_pch", 1)
dend <- set(dend, "leaves_cex", 4)
ggd1 <- as.ggdend(dend) # trsnsform to gg object

theme_bare <- theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(linetype = "solid", colour = "black"),
    plot.title = element_text(vjust = 2, size = 16))

circPlot <- ggplot(ggd1, labels = T,horiz = TRUE) +
    scale_y_reverse(expand = c(0.1, 0)) +
    coord_polar(theta="x") +
    #theme_bare +
    ggtitle("Clustering of subjects based on PE values (the labels give the day/night period)")
#circPlot

# GENERATE HEAT MAP
# #browser()
# dend_r <-  hclust(dist(group[,-1], method = "euclidean"), method = "average") %>% as.dendrogram %>% color_branches(h=0.06) #%>%color_labels(h=1)
# 
# #dend_c <- t(group[,-1]) %>% dist(method = "euclidean") %>% hclust(method = "complete") %>% as.dendrogram %>%ladderize%>%
# #    color_branches(h = 6) %>% hang.dendrogram(hang_height = 0.1)
# #hang.dendrogram(dend, hang = 0.1, hang_height, ...)
# 
# labels(dend_r) <- paste(group[,1][order.dendrogram(dend_r)],
#                         "(",labels(dend_r),")", 
#                         sep = "")
# labels(dend_r) <- as.character(group[,1][order.dendrogram(dend_r)])
# labRow <- rev(as.character(group[,1][order.dendrogram(dend_r)]))                         
# 
# some_col_func <- function(n) (colorspace::diverge_hcl(n, h = c(246, 40), c = 96, l = c(65, 90)))
# 
# titel <- paste(deparse(substitute(group)), 
#                "subjects: cluster analysis")
# 
# gplots::heatmap.2(as.matrix(group[,-1]), 
#                   main = titel,
#                   Rowv = dend_r,
#                   dendrogram = "row",
#                   Colv = NULL,
#                   labRow = group[,1][],
#                   cexRow = 0.8, #font 
#                   trace="none", hline = NA,         
#                   margins =c(3,5),      
#                   key.xlab = "PE values [z-score]",
#                   denscol = "grey57",
#                   density.info = "density",
#                   col = some_col_func,
#                   key = T,
#                   keysize = 1.4, 
#                   scale = "column")
#                   #key.par=list(mar=c(3.5,0,3,0)),
#                   #lmat=rbind(c(5, 4, 2), c(6, 1, 3)), lhei=c(2.5, 5), lwid=c(1, 10, 1)
#                   #lmat=rbind(c(4, 2), c(1, 3)), lhei=c(2, 8), lwid=c(4, 1)
#                   #key.par=list(mar=c(bottom, left, top, right))
#                   #key.par=list(mar=c(4, 4, 4, 4))
#                   #layout(mat = lmat, widths = lwid, heights = lhei)
}


