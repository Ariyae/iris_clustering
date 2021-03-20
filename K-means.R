install.packages("tidyverse")       # for data work & visualization
install.packages("cluster")         # for cluster modeling 
install.packages("reshape2")        # for melting data

library(tidyverse)
library(cluster)
library(reshape2)

iris1=iris
View(iris1)
str(iris1)
summary(iris1)
table(iris$Species)



ggplot(iris1)+
  geom_point(aes(x = Sepal.Length, y = Sepal.Width), stroke = 2)

ggplot(iris1)+
  geom_point(aes(x = Petal.Length, y = Petal.Width), stroke = 2)

ggplot(iris1)+
  geom_point(aes(x = Sepal.Length, y = Petal.Length), stroke = 2)

ggplot(iris1)+
  geom_point(aes(x = Sepal.Width, y = Petal.Width), stroke = 2)

set.seed(123) # for reproduction
wcss <- vector()
for (i in 1:10) wcss[i] <- sum(kmeans(iris1[, -5], i)$withinss)
plot(1:10, wcss, type = "b",main = paste("The Elbow Method"),xlab = "Number of Clusters",ylab = "WCSS")
km <- kmeans( x = iris1[, -5] , centers = 3)
yclus <- km$cluster
table(yclus)
clusplot(iris1[, -5],
         yclus,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 0,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Clusters of Iris Flowers")
)
iris1$cluster.kmean <- yclus
cm <- table(iris1$Species, iris1$cluster.kmean)
cm
