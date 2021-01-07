#################################
####  EDA
#################################

# instalar pacotes
install.packages("tidyverse")
install.packages("epiDisplay")

# carregar pacotes
library(tidyverse)
library(epiDisplay)

# lendo dados
wage1 = read.table("./DadosMRP/wage1.txt")
head(wage1)
glimpse(wage1)

# nome das variáveis
wagenomes = c("wage", "educ", "exper", "tenure", 
              "nonwhite","female", "married", "numdep", 
              "smsa", "northcen","south", "west",
              "construc", "ndurman", "trcommpu", "trade", 
              "services", "profserv", "profocc", "clerocc",
              "servocc", "lwage", "expersq","tenursq")
colnames(wage1) = wagenomes
glimpse(wage1)

# estadísticas descritivas do salario (wage)

## educação
summary(wage1$educ)

## salario
summary(wage1$wage)

## educação
tab1(wage1$educ)

## salario x educ
boxplot(wage1$wage ~ wage1$educ, col= wage1$educ)

## salario por sexo

summary(wage1$wage[wage1$female == 1])
summary(wage1$wage[wage1$female == 0])
boxplot(wage1$wage ~ wage1$female)

## salario por estado civil
boxplot(wage1$wage ~ wage1$married)


## salario por cor
boxplot(wage1$wage ~ wage1$nonwhite)









