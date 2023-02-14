data = read.table('cholesterol.txt',header=TRUE); data
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

# -> lec 2,3


## EXERCISE 2C
after
(sqrt(length(after)) * (after - mean(after))) / sd(after)

unif_exp = function(a,b){
  return (b+a)/2
}
unif_var = function(a,b){
  return (1/12)*((b-a)^2)
}
