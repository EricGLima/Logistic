library(adabag)
library(caret)
library(randomForest)
library(tree)

#####
data <- read.csv("Fornecedores.csv", header=TRUE, sep=";")

colnames(data) <- c(
  "demanda",
  "iso9000",                 # 1 = Entrega seguindo praticas da ISO, 0 = Entrega não seguindo praticas da ISO 
  "fornecedores",
  "media_atraso_h",
  "reclamacoes_no_pedido", 
  "aplica_Lean",             # 1 = Pedido usa praticas Lean,         0 = Pedido não usa praticas Lean  
  "reclamacoes_resolvidas", 
  "pedido_incompleto",       # 1 = Pedido incompleto,                0 = Pedido completo
  "nivel_falha"   
)


data[data == "?"] = NA

data$demanda                = as.numeric(as.character(data$demanda)) 
data$iso9000                = as.numeric(as.character(data$iso9000)) 
data$fornecedores           = as.numeric(as.character(data$fornecedores)) 
data$media_atraso_h         = as.numeric(as.character(data$media_atraso_h)) 
data$reclamacoes_no_pedido  = as.numeric(as.character(data$reclamacoes_no_pedido))
data$aplica_Lean            = as.numeric(as.character(data$aplica_Lean)) 
data$reclamacoes_resolvidas = as.numeric(as.character(data$reclamacoes_resolvidas)) 
data$pedido_incompleto      = as.numeric(as.character(data$pedido_incompleto)) 
data$nivel_falha            = as.numeric(as.character(data$nivel_falha)) 


data[data$iso9000 == 0,]$iso9000 = "Nao"
data[data$iso9000 == 1,]$iso9000 = "Sim"
data$iso9000 = as.factor(data$iso9000)


data[data$fornecedores == 1,]$fornecedores <- "Fornecedor x"
data[data$fornecedores == 2,]$fornecedores <- "Fornecedor y"
data[data$fornecedores == 3,]$fornecedores <- "Fornecedor z"
data[data$fornecedores == 4,]$fornecedores <- "Fornecedor t"
data$fornecedores = as.factor(data$fornecedores)


data[data$aplica_Lean == 0,]$aplica_Lean = "Nao"
data[data$aplica_Lean == 1,]$aplica_Lean = "Sim"
data$aplica_Lean = as.factor(data$aplica_Lean)


data[data$pedido_incompleto == 0,]$pedido_incompleto = "Pedido completo"
data[data$pedido_incompleto == 1,]$pedido_incompleto = "Pedido incompleto"
data$pedido_incompleto = as.factor(data$pedido_incompleto)


data[data$nivel_falha == 0,]$nivel_falha = "Excelente"
data[data$nivel_falha == 1,]$nivel_falha = "Necessita treinamento"
data[data$nivel_falha == 2,]$nivel_falha = "Nivel severo de falhas"
data[data$nivel_falha == 3,]$nivel_falha = "Nivel severo de falhas e seguranca"
data[data$nivel_falha == 4,]$nivel_falha = "Desqualificado"
data$nivel_falha = as.factor(data$nivel_falha)

data$nivel_falha = ifelse(data$nivel_falha == "Excelente", "Aprovado", "Reprovado")
data$nivel_falha = as.factor(data$nivel_falha)


#####
set.seed(10)

# Adaboost

adaboost = boosting(
  nivel_falha ~ data$demanda + data$reclamacoes_no_pedido, 
  data      = data, 
  boos      = TRUE, 
  mfinal    = 100,
  coeflearn = 'Breiman',
  control   = rpart.control(maxdepth=20)
)


adaboost = boosting(
  nivel_falha ~ ., 
  data      = data.imputed, 
  boos      = TRUE, 
  mfinal    = 100,
  coeflearn = 'Breiman',
  control   = rpart.control(maxdepth=20)
)


summary(adaboost)

#####
# Tree Package

adaboost$trees

t1 = adaboost$trees[[1]]

plot(t1)

text(t1, pretty=0)

adaboost$weights

adaboost$importance

errorevol(adaboost,data)

p = predict(adaboost, data)
p

#####
# Rndfrst

data.imputed = rfImpute(
  nivel_falha ~ .,
  data = data,
  iter = 6
)

model = randomForest(
  nivel_falha ~ ., 
  data      = data.imputed,
  proximity = TRUE,
  trees     = 100
)

model$confusion
model$votes

#####
# Adaboost-caret

index = createDataPartition(
  data$nivel_falha, 
  p    = 0.75,
  list = FALSE
)

train = data.imputed[index,]
test  = data.imputed[-index,]

model2 = train(
  nivel_falha ~.,
  method = "adaboost",
  data   = train,
  metric = "Accuracy"
)

pred2 = predict(model2, newdata = test)

confusionMatrix(pred2, test$nivel_falha)

summary(model2)
