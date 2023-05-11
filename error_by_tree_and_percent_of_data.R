test = function(x,y) {
  
  sub = sample(1:l,x*l)
  
  adaboost = boosting(
    quality ~ .,
    data      = data[sub, ],
    mfinal    = y,
    coeflearn = 'Breiman',
    control   = rpart.control(maxdepth=20)
  )
  
  p = predict.boosting(
    adaboost,
    newdata = data[-sub, ]
  )
  
  message(paste(x, y, p$error, sep="-"))
  return(p$error)
}

train_step = 0.05
train_seq = seq(train_step, 1 - train_step, train_step)

tree_step = 50
tree_seq = seq(tree_step, 300, tree_step)

#####
error_list = map_dbl(train_seq, test)

plot(
  train_seq,
  error_list,
  type = 'b',
  pch  = 16,
  xaxp = c(0,1,20),
  ylab = "error",
  xlab = "percent of data used to train"
)
#####

stats = map(tree_seq, function(x) map(train_seq, function(y) test(y,x))) %>%
  list2df() %>%
  group_by(X2) %>%
  mutate(Grp = row_number()) %>%
  pivot_wider(names_from = "X2", values_from = "X1") %>%
  select(-Grp)

stats = bind_cols(train_seq,stats)
colnames(stats) = c('perc_data_used', tree_seq)
stats

#####
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

#####
#write.csv(stats,"adaboost_hyper_erros.csv", row.names = FALSE)






