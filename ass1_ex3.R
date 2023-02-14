data = read.table('diet.txt',header=TRUE)

weight.lost = data$weight6weeks - data$preweight
data = cbind(data,weight.lost)

pre = data$preweight
post = data$weight6weeks
age = data$age

# test sign. difference -> check assumptions (normality etc.) otherwise other test is needed
t.test(pre,post)

# plot hist of loss per diet
for (i in 1:3){
   hist(data$weight.lost[data$diet == i])
}
mycol = rgb(0, 0, 255, max = 255, alpha = 20, names = "blue50")
mycol2 = rgb(0, 255, 0, max = 255, alpha = 100, names = "green50")


# male vs female loss
femalehist = hist(data$weight.lost[data$gender == 0], plot=FALSE)
malehist = hist(data$weight.lost[data$gender == 1], plot=FALSE)

plot(femalehist, col=mycol)
plot(malehist, col=mycol2, add=TRUE)


# lin reg of age and weight loss -> no relation!
reg = lm(weight.lost ~ age)
plot(age, weight.lost)
abline(reg)
summary(reg)



#ANOVA
boxplot(data); stripchart(data)
