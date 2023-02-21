data = read.table('data/birthweight.txt',header=TRUE)
x = data$birthweight

# significance level alpha
alpha = 0.05

##### EXERCISE 1A
# check normality
qqnorm(x); qqline(x)
shapiro.test(x)
help(shapiro.test)

mean = mean(x); mean
sd = sd(x); sd
n = length(x); n

# 96% bounded ci 
ci = c((mean - qt(0.98,n-1)*(sd/sqrt(n))), (mean + qt(0.98,n-1)*(sd/sqrt(n)))); ci

# length of sample size, s.t. E = 50
# show that qnorm(0.98)*(sd/sqrt(n)) =< 50

# n must be larger or equal to min_n
min_n = ((qnorm(0.98)^2)*(sd^2))/(50^2)

# 96% bootstrap ci
B = 1000
Tstar = numeric(B);
for (i in 1:B){
  Xstar = sample(x,replace=TRUE)
  Tstar[i] = mean(Xstar)
}
Tstar20 = quantile(Tstar,0.020)[[1]]
Tstar980 = quantile(Tstar,0.975)[[1]]
bootstrapci = c(2*mean-Tstar980,2*mean-Tstar20)

# comparison ci vs bootstrap ci
lenci = ci[2]-ci[1]
lenbsci = bootstrapci[2]-bootstrapci[1]
lenbsci<lenci #smaller ci, so more accurate



##### EXERCISE 1B/C

# perform one sided t test
t.test(x,mu=2800,alt="g")

# meaning of CI: a confidence interval for the mean appropriate to the specified alternative hypothesis.

# proposed sign test: T = #(x>2800)
nT = length(x[x>2800]); 

pttest = t.test(x,mu=2800,alt="g")[[3]]
psign = binom.test(nT,n,p=0.5)[[3]]

# compare -> should we simulate? or is one enough
pttest<psign


##### EXERCISE 1D

# estimate of p
p = length(x[x<2600])/n; p
B = (sqrt((p*(1-p))/n)); B
z = (p - 0.25) / B; z 
ci_p = c(0.25,(p+B*z)); ci_p

# suggests alpha/2 = 0.01, so alpha = 0.02
alpha = (pnorm(-z))*2; 1-alpha



##### EXERCISE 1E

# we test proportions
male = 34 + 61
female = 28 + 65

prop.test(c(34,28),c(male,female))

