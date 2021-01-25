#################################
####  MRLM - Aula 08
#################################

library(wooldridge)
library(car)

# Case 1:
modelo = lm(wage~ exper + I(exper^2), data = wage1)
modelo
summary(modelo)

# Case 2:
modelo = lm(log(price)~log(nox) + log(dist) + 
              rooms + I(rooms^2) + stratio, 
            data = hprice2)
summary(modelo)

# Comparação de modelos

modelo1 = lm(salary~sales+roe, data = ceosal1)
modelo2 = lm(salary~sales+roe + indus+finance+consprod, data = ceosal1)

summary(modelo1)
summary(modelo2)
vif(modelo2)


# Intervalos
x0 = data.frame(sales = 7000, roe = 20, indus = 1, finance = 1, consprod = 1)
predict(modelo2,newdata = x0, interval = "prediction")

