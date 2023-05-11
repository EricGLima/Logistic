# May 04, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using 10 - Fold Cross Validation to discover the best number of trees

#####
# Setup
data$quality = as.factor(ifelse(data$quality>=6, 'Aprovado', 'Reprovado')) 

tree_step = 25
tree_seq = seq(100, 300, tree_step)

k = 10
set.seed(seed)
cv = crossv_kfold(data, k = k)

#####
# Train & Test

barra = progress::progress_bar$new(total = k * length(tree_seq))
models_by_tree = map(tree_seq, function(x) map(cv$train, ~ada(x,.)))

pred = map(models_by_tree,  ~map2_dbl(., cv$test, get_pred))

error_by_tree = map_dbl(pred,mean)
names(error_by_tree) = tree_seq

tree_df = data.frame(trees = tree_seq, error = error_by_tree)

#####
# Plot

ggplot(tree_df,aes(x=trees,y=error)) +
  geom_line(size = 1, linetype = "dashed", color="darkgray") +
  geom_point(size=3.5, shape=20) +
  labs(
    title = "Mean Error by Number of Trees",
    subtitle = "10 - Fold Cross Validation",
    x = "Trees",
    y = "Error"
  ) +
  scale_x_continuous(breaks=tree_seq) +
  theme_linedraw()

 