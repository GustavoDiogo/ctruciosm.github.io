#################################
####  RLS
#################################

# carregar pacotes
library(ggplot2)

# Lendo dados
CEOSAL1 = read.table("./DadosMRP/ceosal1.txt")[,c(1,4)]
colnames(CEOSAL1) = c("salario", "roe")
head(CEOSAL1,3)

# Gráfico de dispersão
ggplot(CEOSAL1, aes(x = roe, y = salario)) + geom_point() 

# Fit
modelo = lm(salario~roe, data = CEOSAL1)
modelo
yhat = fitted(modelo)
R2 = cor(yhat,CEOSAL1$salario)^2
R2

ggplot(CEOSAL1, aes(x = roe, y = salario)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE)



