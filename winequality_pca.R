# May 06, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Wine quality PCA

#####
# Scaling the Base
pca.data = data %>%
  select_if(is.numeric) %>%
  map_df(~(./sd(.))) %>%
  select(-density,-pH) %>%
  mutate(density = data$density, pH = data$pH)

pca.data = data %>%
  select(-quality)

#####
# Using PCA and Scree Plot
pca = prcomp(pca.data, scale=T)

plot(pca$x[,1], pca$x[,2])

pca.var = pca$sdev ** 2
pca.var.per = round(pca.var/sum(pca.var)*100, 1)
barplot(
  pca.var.per,
  main="Scree Plot",
  xlab="Principal Component",
  ylab="Percent Variation"
)
cumsum(pca.var.per)

#####
# Plot PCA

library(ggplot2)
pca.data = data.frame(
  #Sample=rownames(pca$x),
  X=pca$x[,1],
  Y=pca$x[,2]
)

pca.data

ggplot(data=pca.data, aes(x=X,y=Y)) +
  #geom_text() +
  geom_point() +
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
