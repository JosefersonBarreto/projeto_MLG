
rm(list = ls())

dados<-read.table("dados.txt",sep = ",",header = TRUE)



#dados$price<-as.numeric(dados$price)


hist(dados$price)



model<-glm(dados$price~., family = poisson(),data = dados)


summary(model1)



library('hnp')
hnp(t, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T)


hnp(model)





model<-glm(dados$price~., family = gaussian(link = "identity"),data = dados)


summary(model)

# ===============================================================================
#                     analise exploratória           
# ===============================================================================

library(tidyverse)
library()



glimpse(dados)




#   vendo  a quantidade de fatores 



t<-dados %>%
  dplyr::select(where(is.factor)) 

  



#dados[3:9]<- sapply(dados[3:9],as.factor)




dados[3:9]<-dados[3:9] %>% mutate_if(is.character,as.factor)
  
dados[15:16]<-dados[15:16] %>% mutate_if(is.character,as.factor) 
  
 

dados[18]<-dados[18] %>% mutate_if(is.character,as.factor)  


dados[20]<-dados[20] %>% mutate_if(is.character,as.factor) 



dados<-dados %>% mutate_if(is.character,as.numeric) 







glimpse(dados)
        
# verificando valores ausentes 



sum(dados[!complete.cases(dados),])


#atribuir mediana a NAs
dados[is.na(dados$normalized.losses),]$normalized.losses = median(dados$normalized.losses,na.rm = T)

dados[is.na(dados$price),]$price = median(dados$price,na.rm = T)


t<-dados %>%
  dplyr::select(where(is.numeric))  
library(Hmisc)
hist.data.frame(t)

# aqui podemos ver todos histogramas e podemos ver que nossa variável observada 
#não segue uma distribuição normaal 


sum(is.na(t
))


cor(t)

# t %>%
#   filter_at(vars(starts_with("symboling")), all_vars(is.na(.)))
# 
# dados %>%
#   dplyr::select(where(!is.na))  




colnms <- colnames(t)

# filter
teste<-t %>%
  filter_at(vars(all_of(colnms)), any_vars(is.na(.)))


dados[is.na(dados$bore),]$bore = median(dados$bore,na.rm = T)
dados[is.na(dados$compression.ratio),]$compression.ratio = median(dados$compression.ratio,na.rm = T)
dados[is.na(dados$horsepower),]$horsepower= median(dados$horsepower,na.rm = T)
dados[is.na(dados$peak.rpm),]$peak.rpm= median(dados$peak.rpm,na.rm = T)



write.csv(dados,"carrosTrat.csv",row.names = F)



# verificando presença de multicolinearidade das variáveis numericas 


library(coorplot
        )
corrplot::corrplot(corr = cor(t),type = "upper")



library(caret)
t[-15] %>%
  cor() %>%
  findCorrelation( cutoff = .50, verbose = FALSE)


#verificando se temos colunas identicas 

t<-t %>%
  cor() %>%  findLinearCombos ()


caret::findLinearCombos(t)

#não temos colunas idententicas 
glimpse(t)  

novo<-t %>% 
  dplyr::select(symboling ,normalized.losses,compression.ratio,horsepower,peak.rpm,price )




ui<-cor(novo)
 
corrplot::corrplot(cor(novo))


mod<-glm(novo$price~.,family = Gamma(link=log),data = novo)
summary(mod)


mod<-glm(novo$price~.,family = inverse.gaussian(link = "1/mu^2"),data = novo)
summary(mod)

library('hnp')
hnp(mod, sim = 99,resid.type ='deviance',how.many.out=T ,
    conf = 0.95,scale = T)


hnp(mod)
