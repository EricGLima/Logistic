# May 03, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using Adaboost in the Base wine quality

#####
# First Adaboost's aplication

adaboost = boosting(
  quality ~ ., 
  data      = data, 
  boos      = TRUE, 
  mfinal    = 100,
  coeflearn = 'Breiman',
  control   = rpart.control(maxdepth=20)
)

summary(adaboost)

p = predict(adaboost, data)
p

#####
# Second Adaboost's aplication

l = length(data[,1])
set.seed(seed)
sub = sample(1:l,0.9*l)

set.seed(seed)
adaboost = boosting(
  quality ~ .,
  data      = data[sub, ],
  mfinal    = 175,
  coeflearn = 'Breiman',
  control   = rpart.control(maxdepth=20)
)

# Predicions
set.seed(seed)
p = predict.boosting(
  adaboost,
  newdata = data[-sub, ]
)

p$confusion
p$error

# comparing error evolution in training and test set

evol.train = errorevol(adaboost, newdata = data[sub, ])
evol.test = errorevol(adaboost, newdata = data[-sub, ])

plot.errorevol(evol.test,evol.train)

#####
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

#####
# Verifying the first tree

t1 = adaboost$trees[[1]]
plot(t1)
text(t1, pretty=0)

#####
# Verifying the model

adaboost$weights
adaboost$importance
errorevol(adaboost,data)

