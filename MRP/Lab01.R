######################################
### Laboratorio de R: 01
######################################

x = 1
y = 2

# Comparacoes
x < y
x > y
x == y

# operacoes basicas
x+y
x-y
x*y
x/y
y^2
y^3

# vetores
x = c(1,2,3,4,5)
y = c(6,7,8,9,10)

n_x = length(x)
n_y = length(y)

sum(x)
mean(x)
median(x)
min(x)
max(x)

x+y
x*y

plot(x,y)

# matrix
A = matrix(c(1,2,3,4),ncol=2)
A

# numeros aleatorios
x = rnorm(1000)
hist(x)
x = runif(1000)
hist(x)

# distribuicao normal
pnorm(0)
pnorm(2)
pnorm(5)

qnorm(0.05)
qnorm(0.025)

# comando IF
a = 8
b = 10
if(a < b){
  print("a é menor do que b")
}

a = 18
b = 10
if(a < b){
  x = rnorm(1000)
} else {
  x = runif(1000)
}
hist(x)

# comando FOR

x = c()
for(i in 1:10){
  x[i] = rnorm(1)
}
x
length(x)


x = c()
y = c()
for(i in 1:1000){
  x[i] = rnorm(1)
  y[i] = runif(1)
}

plot(x,y)

boxplot(x)
hist(x)


# Comando IF + FOR
x = c()
for(i in 1:100){
  if(i<= 50){
    x[i] = 0
  } else{
    x[i] = 1
  }
}
table(x)

# summary()

peso = rnorm(1000, mean = 65, sd = 2)

summary(peso)
boxplot(peso)
hist(peso)
sd(peso)

# Simulando dados de uma regressao linear

# y = b0 + b1x + u

n = 10000
u = rnorm(n)
b0 = 0.8
b1 = 0.67
x = rnorm(n, mean = 10)
y = b0 + b1*x + u
plot(x,y)
lm(y~x)

# algumas funcoes
x = rnorm(10)
abs_x = abs(x)
sqrt(abs(x))
log(abs(x))

abs_x2 = c()
for(i in 1:10){
  abs_x2[i] = abs(x[i])
}
abs_x2






















