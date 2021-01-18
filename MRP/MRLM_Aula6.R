#################################
####  MRLM - Aula 06
#################################

### Carregando os dados
WAGE1 = read.table("./DadosMRP/wage1.txt")[,1:4]
colnames(WAGE1) = c("wage", "educ", "exper", "tenure")


### Ajustar o modelo
modelo = lm(log(wage) ~ educ+exper+tenure, data = WAGE1)
summary(modelo)$coefficients
summary(modelo)


### Teste F
modeloi = lm(log(wage) ~ educ+exper+tenure, data = WAGE1)
modelor = lm(log(wage) ~ exper, data = WAGE1)
anova(modelor, modeloi)



#################################

TWOYEAR = read.table("./DadosMRP/twoyear.txt")[,8:11]
colnames(TWOYEAR) = c("exper","jc","univ","lwage")

modelo = lm(lwage~., data = TWOYEAR)
modelo
summary(modelo)$coefficients[,"Std. Error"]^2
vcov(modelo)


### Testar beta_jc = beta_univ

betajc = coef(modelo)[3]; betauniv = coef(modelo)[4]
COV = vcov(modelo)
s2jc = COV[3,3]; s2univ = COV[4,4]; covjc_univ = COV[3,4]
Testet = (betajc-betauniv)/sqrt(s2jc+s2univ-2*covjc_univ)
abs(Testet)
n = nrow(TWOYEAR); k = 4
c(qt(0.975, df =  n-k-1), 1- pt(abs(Testet),df =  n-k-1))

### Testar beta_assess = 1, beta_lotsize = 0, beta_sqrft= 0, beta_bdrms = 0
HPRICE1 = read.table("./DadosMRP/hprice1.txt")
colnames(HPRICE1) = c("price", "assess", "bdrms", "lotsize", "sqrft",
                      "colonial", "lprice", "lassess","llotsize", "lsqrft")

modeloi = lm(log(price) ~ log(assess) + log(lotsize) + log(sqrft) + bdrms, data = HPRICE1)
modelor = lm(log(price) ~ offset(1*log(assess)), data = HPRICE1)
anova(modelor, modeloi)

