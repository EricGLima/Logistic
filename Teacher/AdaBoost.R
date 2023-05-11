
install.packages('adabag', dependencies=TRUE)
install.packages('tree', dependencies=TRUE)

library(adabag)
library(randomForest)
library(caret)

data <- read.csv("D:\\Backup 11_10_2018\\CEFET\\Disciplinas CEFET\\Logística\\Aulas\\Aula 2 - Serviço ao cliente\\Fornecedores.csv", header=TRUE, sep=";")

colnames(data) <- c(
  "Demanda",
  "ISO9000",# 0 = Entrega não seguindo práticas da ISO , 1 = Entrega  seguindo práticas da ISO
  "Fornecedores", #O Fornecedor
  "Media_de_atraso_h",
  "Reclamacoes_no_pedido", # 
  "Aplica_Lean",  # 1 = Pedido usa práticas Lean, 0 = 1 = Pedido não usa práticas Lean  
  "Reclamacoes_resolvidas", 
  "Pedido_incompleto", # 1=Pedido incompleto, 0=Pedido completo
  "Nível_de_falha_do_fornecedor"   
)



data[data == "?"] <- NA
data$Demanda <- as.numeric(as.character(data$Demanda)) 
data$`ISO9000` <- as.numeric(as.character(data$`ISO9000`)) 
data$Fornecedores <- as.numeric(as.character(data$Fornecedores)) 
data$`Media_de_atraso_h` <- as.numeric(as.character(data$`Media_de_atraso_h`)) 
data$`Reclamacoes_no_pedido` <- as.numeric(as.character(data$`Reclamacoes_no_pedido`))
data$`Aplica_Lean` <- as.numeric(as.character(data$`Aplica_Lean`)) 
data$`Reclamacoes_resolvidas` <- as.numeric(as.character(data$`Reclamacoes_resolvidas`)) 
data$`Pedido_incompleto` <- as.numeric(as.character(data$`Pedido_incompleto`)) 
data$`Nível_de_falha_do_fornecedor` <- as.numeric(as.character(data$`Nível_de_falha_do_fornecedor`)) 


data[data$`ISO9000` == 0,]$`ISO9000` <- "Nao"
data[data$`ISO9000` == 1,]$`ISO9000` <- "Sim"
data$`ISO9000` <- as.factor(data$`ISO9000`)

data[data$Fornecedores == 1,]$Fornecedores <- "Fornecedor x"
data[data$Fornecedores == 2,]$Fornecedores <- "Fornecedor y"
data[data$Fornecedores == 3,]$Fornecedores <- "Fornecedor z"
data[data$Fornecedores == 4,]$Fornecedores <- "Fornecedor t"
data$Fornecedores <- as.factor(data$Fornecedores)


data$`Media_de_atraso_h` <- as.numeric(data$`Media_de_atraso_h`)

data$`Reclamacoes_no_pedido` <- as.numeric(data$`Reclamacoes_no_pedido`)

data[data$`Aplica_Lean` == 0,]$`Aplica_Lean` <- "Nao"
data[data$`Aplica_Lean` == 1,]$`Aplica_Lean` <- "Sim"
data$`Aplica_Lean` <- as.factor(data$`Aplica_Lean`)

data$`Reclamacoes_resolvidas` <- as.numeric(data$`Reclamacoes_resolvidas`)

data[data$`Pedido_incompleto` == 0,]$`Pedido_incompleto` <- "Pedido completo"
data[data$`Pedido_incompleto` == 1,]$`Pedido_incompleto` <- "Pedido incompleto"
data$`Pedido_incompleto` <- as.factor(data$`Pedido_incompleto`)

data[data$`Nível_de_falha_do_fornecedor` == 0,]$`Nível_de_falha_do_fornecedor` <- "Excelente"
data[data$`Nível_de_falha_do_fornecedor` == 1,]$`Nível_de_falha_do_fornecedor` <- "Necessita treinamento"
data[data$`Nível_de_falha_do_fornecedor` == 2,]$`Nível_de_falha_do_fornecedor` <- "Nível severo de falhas"
data[data$`Nível_de_falha_do_fornecedor` == 3,]$`Nível_de_falha_do_fornecedor` <- "Nível severo de falhas e segurança"
data[data$`Nível_de_falha_do_fornecedor` == 4,]$`Nível_de_falha_do_fornecedor` <- "Desqualificado"
data$`Nível_de_falha_do_fornecedor` <- as.factor(data$`Nível_de_falha_do_fornecedor`)

data$Nível_de_falha_do_fornecedor <- ifelse(test=data$`Nível_de_falha_do_fornecedor` == "Excelente", yes="Aprovado", no="Reprovado")
data$Nível_de_falha_do_fornecedor <- as.factor(data$Nível_de_falha_do_fornecedor)


set.seed(10)

#Adaboost
adaboost<-boosting(Nível_de_falha_do_fornecedor~data$Demanda + data$Reclamacoes_no_pedido, 
                   data=data, 
                   boos=TRUE, 
                   mfinal=100,coeflearn='Breiman',
                   control=rpart.control(maxdepth=20))


adaboost<-boosting(Nível_de_falha_do_fornecedor~., 
                   data=data.imputed, 
                   boos=TRUE, 
                   mfinal=100,coeflearn='Breiman',
                   control=rpart.control(maxdepth=20))


summary(adaboost)

adaboost$trees
t1<-adaboost$trees[[1]]
library(tree)
plot(t1)
text(t1,pretty=0)

adaboost$weights


adaboost$importance
errorevol(adaboost,data)
p<-predict(adaboost,data)
p


#Rndfrst
data.imputed <- rfImpute(Nível_de_falha_do_fornecedor ~ ., data = data, iter=6)
model <- randomForest(Nível_de_falha_do_fornecedor ~ ., 
                      data=data.imputed, proximity=TRUE, trees=100)
model$confusion
model$votes

#Adaboost-caret
index = createDataPartition(data$Nível_de_falha_do_fornecedor, 
                            p=0.75, list=FALSE)

train <- data.imputed[index,]
test <- data.imputed[-index,]

model2<- train(Nível_de_falha_do_fornecedor ~., method="adaboost",
               data=train, metric="Accuracy")

pred2<-predict(model2, newdata = test)
confusionMatrix(pred2, test$Nível_de_falha_do_fornecedor)

summary(model2)







