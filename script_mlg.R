#===============================================================================
#        Script  com analises 
#===============================================================================


library(tidyverse)
library(readr)


dados <- read.csv("boston.csv")


hist(dados$target
     )
# vendo os tipos dos dados 


glimpse(dados)

 corrplot::corrplot(cor(dados),type = "upper")
cor(dados)


t<-cor(dados)


# 
# podemos perceber que temos  algumas variáveis altamente correlacionadas entre si, 
# como isso acontece o método de remoção se  dará da forma, as variáveis preditoras 
# correlacionadas com o maior número de  outras variáveis preditoras 


#temos 3 variáveis candidatas a remoção : INDUS,LSTAT,NOX,DIS



# ================================================================================
#                          seleção de   atributos 
# ================================================================================


library(randomForest)



# dividindo os dados em treino e teste 

importancia  = randomForest(dados$target~ ., data = dados)

col = importance(importancia)
options(scipen=999) 
varImpPlot(importancia)



# pelo método de seleção de atributos, ZN CHAS Rad e B não são muito significativas 
# para o modelo ,vamos remover essas variáveis 




novosd<-dados |> select(RM,LSTAT,NOX,PTRATIO,INDUS,DIS,CRIM,TAX,AGE,target)




corrplot::corrplot(cor(novosd),type = "upper")
cor(dados)





t<-glm(target~.,data = novosd)


summary(t)





require(MASS)
model.negative.binomial <- glm.nb(target~., data = novosd)
summary(model.negative.binomial)


install.packages('hnp')

library('hnp')
hnp(t, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T)


hnp(model.negative.binomial)

glm


mod1<- glm(log(target)~.,family =binomial(link = "logit") ,data = novosd)







dados<-read.csv("Behavior of the urban traffic of the city of Sao Paulo in Brazil.csv",sep = ";")


dados$Slowness.in.traffic....
glm()
