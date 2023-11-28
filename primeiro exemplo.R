

dados2<-read.csv("20_Victims_of_rape.csv")



teste<-glm(dados2$Victims_of_Rape_Total~. , data = dados2,family = poisson(link = "log"))



summary(teste)



model_poisson <- stan_glm(area ~ ., data = dados, family = poisson())



stan_glm1 <- stan_glm(area ~ temp, data = dados, family = poisson,
                      prior = normal(0, 2.5), seed = 12345)




dados <-  read.csv("C:/Users/joseferson/Documents/curso machine learnig - DSA/capitulo 9/python/dados/treino.csv")



dados<-read.csv("housing.csv",sep = "")

library(tidyverse)
glm(area~temp, family = Gamma(link = "log"), data = dados)



plot(dados$total_traders)


write.csv(dados,"dadostrat.csv",row.names = F)

model_gamma <- stan_glm(area ~., data = dados,
                        family = neg_binomial_2, seed = 12345)

glm(area~.,family = poisson(),data = dados)

glimpse(dados)
hist(round(dados$area),2)

dados$area<-log(dados$area+1)

dados |> filter(distinct(area))

t<-survreg(Surv(day,area)~temp+DMC+RH+FFMC+DC+wind+X+Y, data = dados,dist='exponential')
summary(t)


dados<-dados |> arrange(area)


v<-dados |> distinct(area)
v<-as.vector(v)

dados |> filter(area == c(v))
t<-dados |> filter(area !=  0)


hist(t$area)



boxplot(t$area)



model_gamma <- stan_glm(area ~., data = t,
                        family = neg_binomial_2, seed = 12345)



model_poisson <- glm(area ~ ., data = t, family = poisson())

autoplot(model_poisson)



t |>filter( area ==unique(area))
t$area<-(scale((t$area)))

t$area<-dados$area
model_poisson <- glm(area ~ ., data = t,family =gaussian(link = "identity"))

summary(model_poisson)


par(nforw)


hist(dados$total_comments)


par(mfrow=c(2,2))
plot(model_poisson,which=c(1:4))



teste<-survreg(Surv(day,area)~temp, data = t,dist='loglogistic')





data(airquality)
head(t)




library(corrplot) 
library(ggplot2)
library(hnp)
Banco de dados
data("airquality")
head(airquality);dim(airquality)
