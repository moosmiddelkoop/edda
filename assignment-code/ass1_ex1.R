data = read.table('data/birthweight.txt',header=TRUE)
x = data$birthweight

# significance level alpha
alpha = 0.05

##### EXERCISE 1A
# check normality
qqnorm(x); qqline(x)
hist(x)
shapiro.test(x)

mean = mean(x); mean
sd = sd(x); sd
n = length(x); n
# 96% bounded ci 
ci = c((mean - qt(0.98,n-1)*(sd/sqrt(n))), (mean + qt(0.98,n-1)*(sd/sqrt(n)))); ci

# length of sample size, s.t. E = 50
# show that qnorm(0.98)*(sd/sqrt(n)) =< 50

# n must be larger or equal to min_n
min_n = ((qnorm(0.98)^2)*(sd^2))/(50^2); min_n

# 96% bootstrap ci
B = 10000
Tstar = numeric(B);
for (i in 1:B){
  Xstar = sample(x,replace=TRUE)
  Tstar[i] = mean(Xstar)
}
Tstar20 = quantile(Tstar,0.020)[[1]]
Tstar980 = quantile(Tstar,0.980)[[1]]
bootstrapci = c(2*mean-Tstar980,2*mean-Tstar20); bootstrapci

# comparison ci vs bootstrap ci
lenci = ci[2]-ci[1]
lenbsci = bootstrapci[2]-bootstrapci[1]
lenbsci<lenci #smaller ci, so more accurate



##### EXERCISE 1B

# perform one sided t test
t.test(x,mu=2800,alt="g")

# meaning of CI: a confidence interval for the mean appropriate to the specified alternative hypothesis. TO DO

# proposed sign test: T = #(x>2800)
nT = length(x[x>2800]);

pttest = t.test(x,mu=2800,alt="g")[[3]]
psign = binom.test(nT,n,p=0.5)[[3]]

# not rejected!!
# compare -> should we simulate? or is one enough
pttest<psign

##### EXERCISE 1C

# WORDT NOG GEFIXT
psign_val = numeric(B)
pttest_val = numeric(B)

B = 1000


for (i in 1:B) {
    x1 = rnorm(n,mean=2800,sd=sd)
    pttest_val[i]=t.test(x1,mu=2800,alt="g")[[3]]; 
    psign_val[i]=binom.test(sum(x1>2800),n,p=0.5)[[3]]; 
  }
fracsign = sum(psign_val<0.05)/B; fracsign
fracttest = sum(pttest_val<0.05)/B; fracttest


## @plotto -> test at 'some' mu>2800. Here we test at mu = 2800 (we generate under H1, mu = 2800.) We can already
## see that frac. of rejections is larger for t tests (0.051 > 0.046). If we generate for mu > 2800, say, mu = 2900
## (@plotto verander dan de mean=2800 in line 70, bij x1 = rnorm(....)), then the frac. of rejections is even larger
## than for the sign test. From this we can conclude the power of the t test is larger than for the sign test.


# 
# better performance for ttest than for sign test: 0.993 vs. 0.873. Why is this?

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
# test for normality!! or just say we assume normality, so we can use this test

male = 34 + 61
female = 28 + 65

prop.test(c(34,28),c(male,female))

