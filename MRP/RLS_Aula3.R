#################################
####  RLS
#################################


WAGE1 = read.table("./DadosMRP/wage1.txt")[,c(1,2)]
colnames(WAGE1) = c("wage", "educ")

# Fit1
modelo = lm(wage~educ, data = WAGE1)
modelo
summary(modelo)
uhat = residuals(modelo)
sqrt(var(uhat)*(length(uhat)-1)/(length(uhat)-2))


# Fit2
modelo2 = lm(log(wage)~educ, data = WAGE1)
modelo2
summary(modelo2)
uhat = residuals(modelo2)
sqrt(var(uhat)*(length(uhat)-1)/(length(uhat)-2))







