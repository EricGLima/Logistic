# May 06, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## PCA Study Case

#####
# Creation of the Fake Database
data.matrix = matrix(nrow=100, ncol=10)

colnames(data.matrix) = c(
  paste0("wt", 1:5),
  paste0("ko", 1:5)
)

rownames(data.matrix) = paste0("gene", 1:100)

for(i in 1:100) {
  wt.values = rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values = rpois(5, lambda=sample(x=10:1000, size=1))
  
  data.matrix[i,] = c(wt.values, ko.values)
}

head(data.matrix)

#####
# Using PCA and Scree Plot
pca = prcomp(t(data.matrix), scale=T)

plot(pca$x[,1], pca$x[,2])

pca.var = pca$sdev ** 2
pca.var.per = round(pca.var/sum(pca.var)*100, 1)
barplot(
  pca.var.per,
  main="Scree Plot",
  xlab="Principal Component",
  ylab="Percent Variation"
)

#####
# Plot PCA

library(ggplot2)
pca.data = data.frame(
  Sample=rownames(pca$x),
  X=pca$x[,1],
  Y=pca$x[,2]
)

pca.data

ggplot(data=pca.data, aes(x=X,y=Y, label=Sample)) +
  geom_text() +
  xlab(paste0("PC1 - ", pca.var.per[1], "%")) +
  ylab(paste0("PC2 - ", pca.var.per[2], "%")) +
  theme_bw() +
  ggtitle("PCA Graph")

#####
# Verifying the most important genes for PC1

loading_score = pca$rotation[,1]
gene_scores = abs(loading_score)
gene_score_ranked = sort(gene_scores, decreasing = T)
top_10_genes = names(gene_score_ranked[1:10])

top_10_genes

pca$rotation[top_10_genes,1]
