data = read.table('data/cholesterol.txt',header=TRUE); data
before = data$Before
after = data$After8weeks


## EXERCISE 2A
summary(data)
shapiro.test(before)
shapiro.test(after)



reg = lm(after ~ before)
plot(before,after, pch=16, col="blue")
abline(reg)
summary(reg) #adj R^2 = 0.98 explained, correlated positively, but less than 1 so cholesterol level gets less. 

# but easier to see, if we remove before bias and only look at first differences:
diff = after - before
reg2 = lm(diff ~ before)
plot(before,diff, pch=16, col="red")
abline(reg2)
summary(reg2)
# notice that decrease in cholesterol level is higher for people who have higher cholesterol level before the experiment

## EXERCISE 2B

#use perm. test
meansamples = function(x,y) {mean(x-y)}
B=1000; Tstar = numeric(B)
for (i in 1:B){
  dietstar = t(apply(cbind(before,after),1,sample))
  Tstar[i] = meansamples(dietstar[,1],dietstar[,2])
}
myt = meansamples(before,after)
hist(Tstar)

pl = sum(Tstar<myt)/B
pr = sum(Tstar>myt)/B
p = 2*min(pl,pr)
p

#pearson corr. test
cor.test(before,after)
qqnorm(before,main="Q-Q Plot Before")
qqnorm(after,main="Q-Q Plot After")

#seems like normal, but if not: use spearman
cor.test(before,after,method="spearman")


## EXERCISE 2C

# function that computes variance of uniformly distr. variable
unif_var = function(a,b){
  return ((1/12)*((b-a)^2))
}

# E(X) = (a+b)/2 = mean(after)
# So point estimate of b is 2*E(X)-3
theta_est = 2*mean(after)-3

#build 95%-CI for theta_est
n = length(after)
ci_theta = c(theta_est - qnorm(0.975)*(sqrt(unif_var(3,theta_est))/sqrt(n)), theta_est + qnorm(0.975)*(sqrt(unif_var(3,theta_est))/sqrt(n)))
ci_theta


## EXERCISE 2D

#bootstrap test
t = max(after); t

vP = rep(0,9)
vT = rep(0,9)
for (theta in 3:12){
  B = 1000; tstar=numeric(B)
  
  for (i in 1:B){
    xstar=runif(n,3,theta)
    tstar[i] = max(xstar)
  }
  
  pl = sum(tstar<t)/B; pr = sum(tstar>t)/B
  p = 2*min(pl,pr); p
  
  vT[theta-2] = theta
  vP[theta-2] = p
  
}
res = rbind(vT,vP); res

# kolmogorov-smirnov can not be applied, explain.... (totally diff test)


## EXERCISE 2E
# sign test for the median with binomial distr.
s = sum(after<6); s
binom.test(s,n,p=0.5,alt="g") 

# fraction of cholesterol levels lower than 4.5 in after is at most 0.25
s2 = (sum(after<4.5))
binom.test(s2,n,p=0.25,alt="l")
