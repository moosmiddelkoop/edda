data = read.table('data/diet.txt',header=TRUE)

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



## ANOVA (Ex. 3B)
boxplot(data); stripchart(data)
data$diet = as.factor(data$diet)
df = data[c("weight.lost","diet")]; df
is.factor(df$diet); is.numeric(df$diet)

# sum parametrization
contrasts(df$diet)=contr.sum

weightlostaov = lm(weight.lost~diet,data=df)
anova(weightlostaov)
summary(weightlostaov)
confint(weightlostaov)

# plot residuals to see if normality assumption is not violated -> good!
par(mfrow=c(1,2)); qqnorm(residuals(weightlostaov))
plot(fitted(weightlostaov),residuals(weightlostaov))


#TWO WAY ANOVA (ex. 3C)
data$gender = as.factor(data$gender)
df2 = data[c("weight.lost","diet","gender")]; df2

# sum parametr.?
contrasts(df2$diet)=contr.sum; contrasts(df2$gender)=contr.sum

twoway = lm(weight.lost~diet*gender,data=df2)
anova(twoway)
summary(twoway)
confint(twoway)

# plot residuals to see if normality assumption is not violated -> good!
par(mfrow=c(1,2)); qqnorm(residuals(twoway))
plot(fitted(twoway),residuals(twoway))


## EX 3D
head(data)
height = data$height

# show interaction plots to assume a priori that all interactions are 0
par(mfrow=c(1,2))
interaction.plot(height,diet,weight.lost)
interaction.plot(diet,height,weight.lost)

height_diet = lm(weight.lost ~ height+diet,data=data)
anova(height_diet)

# plot residuals of linear model
par(mfrow=c(1,2)); qqnorm(residuals(height_diet))
plot(fitted(height_diet),residuals(height_diet))


## EX 3E

# we first need to compute what the avg person looks like
av_height = mean(height); av_h
av_age = mean(age); av_age
av_weight = mean(pre); av_weight

# what is the best model? second? because height is taken into account