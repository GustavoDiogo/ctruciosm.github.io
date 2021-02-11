### Pacotes

library(wooldridge) # dados do livro
library(dplyr)      # manipulacao de dados
library(sandwich)   # cov robusta à heterocedasticidade
library(lmtest)     # testes de heterocedasticidade e estimacao robusta

# Dados
glimpse(hprice1)   # Rapida olhadinha
hprice = hprice1 %>% select(price,bdrms,lotsize,sqrft,colonial)
glimpse(hprice)

# EDA
summary(hprice)
apply(hprice,2,sd)  # 2: por coluna, 1: por linha
apply(hprice,2,mean)
boxplot(hprice$price)
boxplot(hprice$lotsize)
boxplot(hprice$sqrft)
table(hprice$colonial)
table(hprice$bdrms)

## Suspeitamos de um outlier na variavel lotsize

# Ajustamos o modelo

modelo = lm(price~., data = hprice)
summary(modelo)

## antes de interpretar precisamos verificar as hipoteses

# Verificando as hipoteses

yhat = fitted(modelo)
r = rstudent(modelo)

## Verificar outliers e forma funcional

plot(yhat,r)
plot(hprice$lotsize,r)
plot(hprice$sqrft,r)

### suspeitamos de  1 outliers mas não de forma funcional

summary(hprice$lotsize)

### removemos o possivel outliers e comecamos a modelagem de novo

hprice = hprice1 %>% select(price,bdrms,lotsize,sqrft,colonial) %>% 
  filter(lotsize<92681)

modelo2 = lm(price~.,data = hprice)
summary(modelo2)

# verificando as hipoteses de novo

yhat = fitted(modelo2)
r = rstudent(modelo2)

plot(yhat,r)
plot(hprice$lotsize,r)
plot(hprice$sqrft,r)

### aparentemente sem outliers e sem problemas na forma funcional

modelo2_full = lm(price~. + I(lotsize^2)+ I(sqrft^2), data = hprice)
anova(modelo2 ,modelo2_full)
## Tetse F, p-valor > 0.05 (embora  seja bem proximo), entao nao rejeitamos H0
## e concluimos que não precisamos incluir os termos quadraticos

modelo2_full = lm(price~. + I(yhat^2)+ I(yhat^3), data = hprice)
anova(modelo2 ,modelo2_full)



# verificando heterocedasticidade

plot(yhat,r)

bptest(modelo2,studentize = TRUE)

### H0: erros sao homocedastcos
### H1: erros nao sao homocedasticos (sao heterocedasticos)

## P-valor = 0.3977 > 0.05, entao nao rejeitamos H0 com um nível de significancia de 5%
## e concluimos que os erros sao homocedasticos

# verificar correlcao dos erros

ts.plot(r)
acf(r)

## todas as autocorrelacoes estao dentro das bandas, entao falamos que
## nao existe autocorrelacao dos erros

# verificando normalidade

dim(hprice)

qqnorm(r)
qqline(r)

# interpretando os parametros

summary(modelo2)

## Nosso modelo explica 76.44% da variabilidade de price.
## as variaveis lotsize, sqrft  sao estatisticamente significativas
## as avriaveis bdrms, colonial estao com p-valores bem proximos
## O tetse F que testa conjuntamente 
## H0: beta_{bdrms} = 0, beta_{lotsize} = 0, beta_{sqrft} = 0, beta_{colonial} = 0
## H1: H0 nao eh verdade
## P-valor 2.2e-16 < 0.05, entao rejeitamos H0 com um nivel de significancia de 0.05


## Coisas que podemos melhorar
#### 1) Parece que existe outro outliers
#### 2) Testar outras formas funcionais (interacao por exemplo)




















