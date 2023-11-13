#SPEARMAN CORRELATION MATRIX

PM10 = read.csv("matrixPM10_noNa.csv", header=TRUE, dec = ',', sep = ';')
temp = read.csv("matrixtemp_noNa.csv", header=TRUE, dec = ',', sep = ';')
rain = read.csv("matrixrain_noNa.csv", header=TRUE, dec = ',', sep = ';')
wind = read.csv("matrixwind_noNa.csv", header=TRUE, dec = ',', sep = ';')

#compute spearman correlation only among common stations of PM10, wind, temp, rain
vec_PM10=names(PM10)
vec_temp=names(temp)
vec_rain=names(rain)
vec_wind=names(wind)

inters1 =intersect(vec_PM10, vec_temp)
inters2= intersect(inters1, vec_rain)
inters= intersect(inters2, vec_wind)
inters= inters[2:55]


library(fda)
library(roahd)

PM10=PM10[,inters]
temp=temp[,inters]
rain=rain[,inters]
wind=wind[,inters]


#compute correlation matrix
corr_matrix = matrix (0, 4, 4)
colnames(corr_matrix) = c("PM10", "temp", "wind", "rain")
rownames(corr_matrix) = c("PM10", "temp", "wind", "rain")


for (i in 1:54) {
    cov_matrix = matrix (0, 365, 4)
    
    cov_matrix[,1] = as.numeric(PM10[,i])
    cov_matrix[,2] = as.numeric(temp[,i])
    cov_matrix[,3] = as.numeric(wind[,i])
    cov_matrix[,4] = as.numeric(rain[,i])
    
    for (j in 1:4) {
      for (k in 1:4) {
        corr_matrix [j,k] = corr_matrix [j,k] + cor(cov_matrix[,j], cov_matrix[,k], method = "spearman", use = "complete.obs")
      }
    }
    
}

corr_matrix = corr_matrix/54


#4 ways of plotting the spearman correlation matrix (the 4th is the one used at the end)

#grafico 1
library(corrplot)

corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)




#grafico 2

#install.packages("reshape2")
library(reshape2)
melted_cormat <- melt(corr_matrix, na.rm = TRUE)
# Heatmap
library(ggplot2)
quartz()
ggplot(data = melted_cormat, aes(Var1, Var2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 0, vjust = 1, 
                                   size = 9, hjust = 1))+
  coord_fixed()


#grafico 3


# Install and load reshape2 package
#install.packages("reshape2")
library(reshape2)

# creating correlation matrix
corr_mat <- round(corr_matrix,2)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)
head(melted_corr_mat)

# plotting the correlation heatmap
library(ggplot2)
ggplot(data = melted_corr_mat, aes(x=Var1, y=Var2,
                                   fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)



#grafico 4
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

# Reorder the correlation matrix
cormat <- round(reorder_cormat(corr_matrix), 3)
upper_tri <- round(get_upper_tri(corr_matrix),3)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 0, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)

quartz()
ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.4, 0.8),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


