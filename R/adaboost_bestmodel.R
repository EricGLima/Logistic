# May 22, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using the best Adaboost model in the Base wine quality

# Setup

best_model       = models_by_tree[[4]][[6]]
best_model.train = data[cv$train[[6]][["idx"]], ]
best_model.test  = data[cv$test[[6]][["idx"]], ]

# Predictions

set.seed(seed)
p = predict.boosting(
  best_model,
  newdata = best_model.test
)

p$confusion
p$error

# Plots

evol.train = errorevol(best_model, newdata = best_model.train)
evol.test = errorevol(best_model, newdata = best_model.test)

plot.errorevol(evol.test,evol.train)

# Plotting the error with ggplot

error_evol = data.frame(
  iter = 1:length(evol.train$error),
  train = evol.train$error,
  test = evol.test$error
) %>%
  gather("type","error",-iter)

ggplot(error_evol,aes(x=iter, y=error, color=type)) +
  geom_line(size=1) +
  scale_x_continuous(breaks=seq(0,175,25)) +
  geom_hline(yintercept=p$error, size=1,color="red", linetype="dashed") +
  labs(
    title = "Adaboost - Error Evolution",
    subtitle = "Using 175 Trees",
    x = "Iterations",
    y = "Error",
    color = "Type"
  ) +
  theme_linedraw()
