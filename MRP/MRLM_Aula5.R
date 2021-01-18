#################################
####  MRLM - Aula 05
#################################
library(car)
library(corrplot)


### Carregando os dados
WAGE1 = read.table("./DadosMRP/wage1.txt")
colnames(WAGE1) = c("wage","educ","exper","tenure",
                    "nonwhite","female","married",
                    "numdep","smsa","northcen","south",
                    "west","construc","ndurman","trcommpu",
                    "trade","services","profserv","profocc",
                    "clerocc","servocc","lwage","expersq","tenursq")


### Ajustando o modelo
modelo1 = lm(log(wage) ~ educ, data = WAGE1)
modelo1


modelo3 = lm(log(wage) ~ educ + exper + tenure, data = WAGE1)
modelo3

modelof = lm(log(wage) ~ ., data = WAGE1)
modelof

### Mais informação do modelo

summary(modelo1)

summary(modelo3)

summary(modelof)


### Multicolinearidade

corrplot(cor(WAGE1))
vif(modelof)



