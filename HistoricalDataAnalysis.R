#PCA Implementation on Historical Data
if(!require(readxl)) {
  install.packages("readxl"); require(readxl)
}
if(!require(MASS)) {
  install.packages("MASS"); require(MASS)
}
if(!require("factoextra")) {
  install.packages("factoextra"); require(factoextra)
}


currentDirectory <- getwd()
setwd("C:\\Users\\zoint\\Desktop\\AllFiles\\Projeler\\R_Projects")
directoryChanged <- getwd()
#print out new directory address
print(directoryChanged)

my_data <- read_excel("Autoform_1000.xlsx")
#Normalization
NaValue <- function (x) { sum(is.na(x))}
apply(my_data, 2, NaValue)
#Principal component Analysis Part
my_data.principal <- my_data[1:999, 1:9]
print(head(my_data.principal[,1:9]))
pca.graph <- prcomp(my_data.principal, scale=TRUE)
#Visualisierung
# fviz_pca_biplot(pca.graph, repel = TRUE,
#                 col.var = "#2E9FDF", # Variables color
#                 col.ind = "#696969"  # Individuals color
# )

resultPlot <- fviz_pca_var(pca.graph, col.var="contrib")+
  scale_color_gradient2(low="orange", mid="blue",
                        high="red", midpoint=96) +
  theme_minimal()
print(resultPlot)

# Gradient color
resultHeatMap <- fviz_pca_ind(pca.graph, col.ind="cos2") +
  scale_color_gradient2(low="yellow", mid="blue",
                        high="red", midpoint=0.6)

print(resultHeatMap)

eigen.values <- get_eigenvalue(pca.graph)
print(eigen.values)

result.values <- get_pca_var(pca.graph)
result.values$coord
result.values$contrib
result.values$cos2

# Graph of individuals
# An alias of fviz_pca_biplot()
#Linear Discriminant Analysis
#It is a well-established machine learning techique for predicting categories. Its main advantages,
#compared to other classification algorithms such as neural networks and random forests, are that
#the model is interpretable and that prediction is easy
my_data.lda <- lda(HARDNESSP1 ~ ., data = my_data[1:9])
# print(my_data.lda)
my_data.lda.values <- predict(my_data.lda)
# print(my_data.lda.values)
# print(my_data.lda.values$x[,1])
# ldahist(data = my_data.lda.values$x[,8], g=HARDNESSP1)
# plot(my_data.lda.values$x[,1],my_data.lda.values$x[,2])
# text(my_data.lda.values$x[,1],my_data.lda.values$x[,2], HARDNESSP1, cex=0.7, pos=4, col="red")
