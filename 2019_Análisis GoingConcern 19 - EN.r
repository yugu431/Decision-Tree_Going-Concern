library(readxl)
library(ggplot2)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
library(pROC)
#IMPOTAMOS LOS DATOS
Entidades <- read_excel("EntidadesGC_Limpio_Completo.xlsx")

#MOSTRAMOS LOS DATOS
View(Entidades)
summary(Entidades)

#How many cases are there of each type
table(Entidades$`?Incluye GC?`)



#factor the variables
Entidades$`?Incluye GC?`<- factor(Entidades$`?Incluye GC?`)

Entidades$Sector_Score_Factor<- factor(Entidades$Sector_Score_Factor)

Entidades$`Total points_F`<- factor(Entidades$`Total points_F`)

Entidades$`Working capital / Assets_F...15`<- factor(Entidades$`Working capital / Assets_F...15`)

Entidades$`Retained earnings / Assets_F...17`<- factor(Entidades$`Retained earnings / Assets_F...17`)

Entidades$`Operating margin / Assets_F`<- factor(Entidades$`Operating margin / Assets_F`)


Entidades$`Cash flow form operations / Operating margin_F`<- factor(Entidades$`Cash flow form operations / Operating margin_F`)

Entidades$`Revenue / Assets_F`<- factor(Entidades$`Revenue / Assets_F`)

Entidades$`Intangibles / Accounts receivable + Cash_F`<- factor(Entidades$`Intangibles / Accounts receivable + Cash_F`)
Entidades$`Cash flow form operations / Operating margin_F`<- factor(Entidades$`Cash flow form operations / Operating margin_F`)

Entidades$`Relevant adquisition y the last 2 years score`<- factor(Entidades$`Relevant adquisition y the last 2 years score`)
Entidades$`Working capital / Assets_F...31`<- factor(Entidades$`Working capital / Assets_F...31`)

Entidades$`Retained earnings / Assets_F...33`<- factor(Entidades$`Retained earnings / Assets_F...33`)
Entidades$`Debt / Assets_F`<- factor(Entidades$`Debt / Assets_F`)
Entidades$`Clients /Assets_fF`<- factor(Entidades$`Clients /Assets_fF`)
Entidades$`Cash / Assets_F`<- factor(Entidades$`Cash / Assets_F`)
Entidades$`Revenue /Assets_f`<- factor(Entidades$`Revenue /Assets_f`)
Entidades$`Operating Margin / Revenue_F`<- factor(Entidades$`Operating Margin / Revenue_F`)
Entidades$`Cash flow from operations / Revenue_F`<- factor(Entidades$`Cash flow from operations / Revenue_F`)



#delete unnecessary data
Entidades$Oficina<- NULL
Entidades$Area<- NULL
Entidades$Socio<- NULL
Entidades$`Nombre Entidad`<- NULL
Entidades$`Socio Numerico`<- NULL
Entidades$`Area Numerica`<- NULL

Entidades$`Cash flow form operations / Operating margin`<- NULL

Entidades$`Cash flow form operations / Operating margin_F`<- NULL

Entidades$`Solvency risk score`<- NULL
Entidades$`solvency Risk`<- NULL


Entidades$`Partner score`<- NULL

Entidades$`Retained earnings / Assets` <- NULL
Entidades$`Retained earnings / Assets_F` <- NULL
Entidades$`Working capital / Assets`<- NULL
Entidades$`Working capital / Assets_F`<- NULL
Entidades$`Financial support score` <- NULL
Entidades$`Cliente code`<- NULL


#fix the seed 
set.seed(150) 

#split the population into training and testing
trainIndex<- createDataPartition(Entidades$`?Incluye GC?`,
                                 p= 0.8,
                                 list = FALSE,
                                 times = 1)


fTR<-Entidades[trainIndex,]
fTS<-Entidades[-trainIndex,]



#Since the population of Going concern = 'Si' is too small, we simulate data to be representative, 
#to be the same number as Going concern = 'No'
set.seed(100)
fTR_UP <- upSample(  x = Entidades[,-which(colnames(Entidades)=='?Incluye GC?')],   #Input variables
                     y = Entidades$`?Incluye GC?`)     #Output variable                

fTR_UP$`?Incluye GC?` <- fTR_UP$Class
fTR_UP$Class <- NULL
table(fTR_UP$`?Incluye GC?`)

fTR <- fTR_UP 
fTR_eval <- fTR
fTS_eval <- fTS

#Configuration of CROSS Validation, Make 10 partitions

ctrl <- trainControl(method = "cv",
                     number = 10,
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)



#We standardize the names of the variables
colnames(fTR)<- make.names(colnames(fTR))
colnames(fTS)<- make.names(colnames(fTS))






#To determine the degree of complexity of the tree (cp) we make an estimate through a sequence
set.seed(150) 
tree.fit = train(form = `X.Incluye.GC.` ~  .,          
                 data = fTR, 
                 method = "rpart", 
                 control = rpart.control(maxcompete = 0), 
                 parms = list(split = "gini"),   # impuriry measure
                 
                 #tuneGrid = data.frame(cp = 0.02),
                 tuneGrid = data.frame(cp = seq(0,0.02,0.0005)),
                 trControl = ctrl, 
                 metric = "ROC")   # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

tree.fit 

plot(tree.fit)





#We make the tree with CP = 0,010 the best without losing efficiency



tree.fit = train(form = `X.Incluye.GC.` ~  .,          
                 data = fTR, 
                 method = "rpart", 
                 control = rpart.control(maxcompete = 0), 
                 parms = list(split = "gini"),
                 tuneGrid = data.frame(cp = 0.015),
                # tuneGrid = data.frame(cp = seq(0,0.02,0.0005)),
                 trControl = ctrl, 
                 metric = "ROC") 


tree.fit

#Table with all possible combinations
tree.fit$finalModel
rules<-rpart.rules(tree.fit$finalModel)


#Simple Image of the tree
plot(tree.fit$finalModel, uniform = TRUE, margin = 0.1)
text(tree.fit$finalModel, use.n = TRUE, all = TRUE, cex = .8,xpd = TRUE)

#image of the tree with visual characteristics
rpart.plot(tree.fit$finalModel, type =1, fallen.leaves = FALSE,
           extra=1, tweak=1.2, box.palette = "Oranges", gap=0, space=0)

prp(tree.fit$finalModel,type = 2, extra=102,box.palette = "Greens")
plot(varImp(tree.fit,scale = FALSE))


#Predictions

#Trainning
fTR_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fTR) # predict probabilities
fTR_eval$tree_class <- predict(tree.fit, type="raw", newdata = fTR) # predict classes 

#TEST
fTS_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fTS) # predict probabilities
fTS_eval$tree_class <- predict(tree.fit, type="raw", newdata = fTS) # predict classes 


#confusion matrix Training data
confusionMatrix(data = fTR_eval$tree_class, #Predicted classes
                reference = fTR_eval$`?Incluye GC?`,# observations
                positive = "YES") #Class labeled as Positive

#confusion matrix test data
confusionMatrix(fTS_eval$tree_class, 
                fTS_eval$`?Incluye GC?`, 
                positive = "YES")


#Distribution Histogram
ggplot(fTS_eval)+geom_histogram(aes(x=tree_prob$YES,fill=`?Incluye GC?`),bins = 10)+facet_grid(.~`?Incluye GC?`)










#Evaluation of the model on the entire data set
fdata_eval <- Entidades
colnames(fdata_eval) <- make.names(colnames(fdata_eval))
fdata_eval$`?Incluye GC?` <- fdata_eval$X.Incluye.GC.
fdata_eval$tree_prob <- predict(tree.fit, type="prob", newdata = fdata_eval) # predict probabilities
fdata_eval$tree_class <- predict(tree.fit, type="raw", newdata = fdata_eval) # predict classes 

confusionMatrix(data = fdata_eval$tree_class, #Predicted classes
                reference = fdata_eval$`?Incluye GC?`,# observations
                positive = "YES") #Class labeled as Positive



ggplot(fdata_eval)+geom_histogram(aes(x=tree_prob$YES,fill=`?Incluye GC?`),bins = 10)+facet_grid(.~`?Incluye GC?`)

###Prediction list Going Concern= YES
ListaYES <- fdata_eval[fdata_eval$tree_class=='YES',]
ListaYES$ID <- rownames(ListaYES)
#Excel with prediccion Going Concern= YES

#Prediction list Going Concern= No
ListaNO <- fdata_eval[fdata_eval$tree_class=='NO',]
ListaNO$ID <- rownames(ListaNO)
write.table(ListaNO,'ListaNO.csv',sep = ';',row.names = F)


