# May 04, 2023
# CEFET - RJ
# Logistic
# Eric G. Lima

## Using Random Forest in the Data

set.seed(seed)

model = randomForest(quality ~ ., data=data, proximity=TRUE)
model

# Plotting the model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Aprovado", "Reprovado"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Aprovado"], 
          model$err.rate[,"Reprovado"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) +
  theme_linedraw() +
  labs(
    title = "Random Forest - Error Evolution",
    subtitle = "Using 500 Trees",
  )

#####
# Adjusting number of trees

model = randomForest(quality ~ ., data=data, ntree=1000, proximity=TRUE)
model

# Plotting the model

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times=3),
  Type=rep(c("OOB", "Aprovado", "Reprovado"), each=nrow(model$err.rate)),
  Error=c(model$err.rate[,"OOB"], 
          model$err.rate[,"Aprovado"], 
          model$err.rate[,"Reprovado"]))

ggplot(data=oob.error.data, aes(x=Trees, y=Error)) +
  geom_line(aes(color=Type)) +
  theme_linedraw() +
  labs(
    title = "Random Forest - Error Evolution",
    subtitle = "Using 1000 Trees",
  )

