horse_data <- read.csv ("/Users/apple/Desktop/Study/semester4/DataAnalytics_ICT513/Datasets/GeneExpression.csv")
library(class)

knn.cv(train = scale(iris[, 1:2]), cl = iris$Species, k = 1)


