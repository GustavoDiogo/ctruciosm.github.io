####################################
### Lista 1 - Gabarito - Códigos ###
####################################


# Q2

library(MASS)
beta0 = 0.2
beta1 = 0.5

#mSigma = matrix(c(1,0,0,1),2)
#mSigma = matrix(c(1,0.3,0.3,1),2)
#mSigma = matrix(c(1,-0.5,-0.5,1),2)
mSigma = matrix(c(1,0.7,0.7,1),2)
vMu = c(0,0)
set.seed(1234)
dados_simulados = mvrnorm(2000, mu = vMu, Sigma = mSigma)

x = dados_simulados[,1]
u = dados_simulados[,2]
y = beta0 + beta1*x + u
lm(y~x)

# Q5

CEOSAL2 = read.table("./DadosMRP/CEOSAL2.txt")
colnames(CEOSAL2) = c("salary", "age", "college","grad",
                      "comten","ceoten","sales","profits","mktval",
                      "lsalary","lsales","lmktval","comtensq",
                      "ceotensq","profmarg")

# 3.a
summary(CEOSAL2$salary)
summary(CEOSAL2$ceoten)

mean(CEOSAL2$salary)
mean(CEOSAL2$ceoten)

# 3.b
sum(CEOSAL2$ceoten == 0)

# 3.c

lm(log(salary)~ceoten, data = CEOSAL2)

# Q6

WAGE1 = read.table("./DadosMRP/WAGE1.txt")[,c(1,3)]
colnames(WAGE1) = c("wage", "exper")
head(WAGE1)


# 6.1
summary(WAGE1)
sd(WAGE1$wage)
sd(WAGE1$exper)
apply(WAGE1,2,sd)
boxplot(WAGE1$wage)
hist(WAGE1$wage)

boxplot(WAGE1$exper)
hist(WAGE1$exper)
barplot(table(WAGE1$exper))

# 6.2

plot(WAGE1$exper,WAGE1$wage)

# 6.3

lm(wage~exper, data = WAGE1)

# 6.5

lm(log(wage)~exper, data = WAGE1)


# Q7

library(wooldridge)
data(catholic)

dim(catholic)


mean(catholic$math12)
mean(catholic$read12)


round(apply(catholic,2,mean),4)
round(apply(catholic,2,sd),4)

summary(catholic$hsgrad)
mean(catholic$hsgrad, na.rm = TRUE)
sd(catholic$hsgrad, na.rm = TRUE)

round(apply(catholic,2,mean, na.rm = TRUE),4)
round(apply(catholic,2,sd, na.rm = TRUE),2)


boxplot(catholic$math12)
boxplot(catholic$read12)

modelo = lm(math12~read12, data = catholic)
modelo
summary(modelo)

math12hat = fitted.values(modelo)
plot(catholic$math12,math12hat)



##################################
# Manipulação de Dados
##################################

library(dplyr)

CEOSAL2 = read.table("./DadosMRP/CEOSAL2.txt")
colnames(CEOSAL2) = c("salary", "age", "college","grad",
                      "comten","ceoten","sales","profits","mktval",
                      "lsalary","lsales","lmktval","comtensq",
                      "ceotensq","profmarg")

CEOSAL_AUX = CEOSAL2 %>% mutate(salary2 = log(salary)) %>%
              select(lsalary,salary2,ceoten) %>%
              filter(ceoten < 2) %>% na.omit()

head(CEOSAL_AUX)
dim(CEOSAL_AUX)
dim(CEOSAL2)







