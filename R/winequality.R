# May 03, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using Adaboost in the Base wine quality

#####
# Definindo como a qualidade sera metrificada

#data$quality = as.factor(data$quality)
#data$quality = as.factor(ifelse(data$quality==8, 'Aprovado', 'Reprovado')) 
data$quality = as.factor(ifelse(data$quality>=6, 'Aprovado', 'Reprovado')) 

data$quality = as.factor(
  ifelse(
      data$quality >= 7, 'Aprovado', ifelse(
      data$quality >= 5, 'Media', 'Reprovado'
    )
  )
)

glimpse(data)

#####
# Aplicacao do adaboost

adaboost = boosting(
  quality ~ ., 
  data      = data, 
  boos      = TRUE, 
  mfinal    = 100,
  coeflearn = 'Breiman',
  control   = rpart.control(maxdepth=20)
)

summary(adaboost)

# Prevendo os valores e matriz de confusao
p = predict(adaboost, data)
p

#####
# Segunda aplicacao do adaboost

l = length(data[,1])
#sub = sample(1:l,2*l/3)
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

set.seed(seed)
p = predict.boosting(
  adaboost,
  newdata = data[-sub, ]
)

p = predict.boosting(
  models_by_tree[[4]][[6]],
  newdata = data[cv$test[[6]][["idx"]], ]
)

p$confusion
p$error


evol.train = errorevol(models_by_tree[[4]][[6]], newdata = data[cv$train[[6]][["idx"]], ])
evol.test = errorevol(models_by_tree[[4]][[6]], newdata = data[cv$test[[6]][["idx"]], ])

plot.errorevol(evol.test,evol.train)

#####
#comparing error evolution in training and test set
evol.train = errorevol(adaboost, newdata = data[sub, ])
evol.test = errorevol(adaboost, newdata = data[-sub, ])

plot.errorevol(evol.test,evol.train)

#####
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
# Verificando uma das arvores criadas
t1 = adaboost$trees[[1]]
plot(t1)
text(t1, pretty=0)

#####
# Verificando o metodo utilizado
adaboost$weights
adaboost$importance
errorevol(adaboost,data)

