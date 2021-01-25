#################################
####  MRLM - Aula 09
#################################

library(wooldridge)

# Case 1:

modelo = lm(wage~female + educ, data = wage1)
summary(modelo)


# Case 2:
modelo = lm(colGPA~PC+hsGPA+ACT, data = gpa1)
summary(modelo)


# Case 3:
modelo = lm(log(price)~ log(lotsize) + log(sqrft) + 
              bdrms + colonial, data = hprice1)
summary(modelo)


x0 = data.frame(lotsize = 9000, sqrft = 2500, bdrms = 4, colonial = 1)
predict(modelo,newdata = x0, interval = "prediction")


# Case 4:

modelo = lm(log(wage)~ married + female + married*female + 
              educ + exper +  I(exper^2) + tenure + I(tenure^2), 
            data = wage1)
summary(modelo)