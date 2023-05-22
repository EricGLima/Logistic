# May 03, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using variations of data and trees

#####
# Setup

train_step = 0.05
train_seq = seq(train_step, 1 - train_step, train_step)

tree_step = 50
tree_seq = seq(tree_step, 300, tree_step)

#####
# Creating the models

stats = map(tree_seq, function(x) map(train_seq, ~get_pred_by_data_and_tree(.,x))) %>%
  list2df() %>%
  group_by(X2) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "X2", values_from = "X1") %>%
  select(-Grp)

stats = bind_cols(train_seq,stats)
colnames(stats) = c('perc_data_used', tree_seq)
stats

#####
# Plotting each model's error

stats_plot = stats %>%
  gather(
    "iterations",
    "error",
    2:dim(stats)[2]
  ) %>%
  mutate(iterations = as.integer(iterations))

ggplot(stats_plot, aes(x= perc_data_used,y=error, color=as.factor(iterations))) +
  geom_line(size=1, linetype="dashed") +
  geom_point(size=3.5, shape=20, stroke=1.5) +
  labs(
    title = "Adaboost Error - Hyperparameters",
    subtitle = "by % of Data and Iterations",
    x = "% of data Used",
    y = "Error",
    color = "Trees"
  ) +
  scale_x_continuous(breaks=train_seq) +
  theme_linedraw() +
  geom_hline(
    yintercept=min(stats_plot$error),
    color="red",
    linetype="dashed"
  )
