df1 <- read.csv("winequality-red.csv", sep = ";")
df2 <- read.csv("winequality-white.csv", sep  = ";")

head(df1)
head(df2)

# apply label to data frames

df1$label <- sapply(df1$pH, function(x){'red'})
df2$label <- sapply(df2$pH, function(x){'white'})

# check head again

head(df1)
head(df2)

wine <- rbind(df1, df2)

str(wine)

# exploratory data analysis

library(ggplot2)

# Histogram of residual sugar

pl <- ggplot(data = wine, aes(x = residual.sugar))+
  geom_histogram(aes(fill = label), color = "black", bins = 50)

pl+scale_fill_manual(values = c("#ae4554", "#faf7ea"))+
  theme_bw()

# Histogram of citric.acid from wine data, colored by wines

pl <- ggplot(data = wine, aes(x = citric.acid))+
  geom_histogram(aes(fill = label), color = "black", bins = 50)

pl+scale_fill_manual(values = c("#ae4554", "#faf7ea"))+
  theme_bw()

# Histogram of alcohol, colored by wine

pl <- ggplot(data = wine, aes(x = alcohol))+
  geom_histogram(aes(fill = label), color = "black", bins = 50)

pl+scale_fill_manual(values = c("#ae4554", "#faf7ea"))+
  theme_bw()

# Scatter plot of residual sugar v citric acid, colored by wine

pl <- ggplot(data = wine, aes(x = citric.acid, y = residual.sugar))+
  geom_point(aes(color = label), alpha = 0.2)

# call dark theme for more contrast

pl + scale_color_manual(values = c("#ae4554", "#faf7ea"))+
  theme_dark()

# another scatterplot, volatile acidity v residual sugar

pl <- ggplot(data = wine, aes(x = volatile.acidity, y = residual.sugar))+
  geom_point(aes(color = label), alpha = 0.2)

pl + scale_color_manual(values = c("#ae4554", "#faf7ea"))+
  theme_dark()

# Looks like we may have a distinguishing feature. Red wines appear to have more acidity

# Moving onto the model

# remember that k means is unsupervised and does not need lables

clus.data <- wine[,1:12]

# check head
head(clus.data)

# Build the clusters

wine.cluster <- kmeans(clus.data, 2)

wine.cluster$centers

# Evaluating the clusters

table(wine$label, wine.cluster$cluster)

#           1    2
#  red   1514   85
#  white 1294 3604

# Red wine seems to be easier to cluster together

