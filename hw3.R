# hw1
library(faraway)
library(caret)

write.csv(wbca, 'wbca.csv',row.names = FALSE)
write.csv(dicentric, 'dicentric.csv', row.names = FALSE)

# pr1 

head(wbca)

mod1 = glm(Class~ BNucl + Thick, family = binomial(), data = wbca)
mod2 = glm(Class~ ., family = binomial(), data = wbca)

# i)

phat1 = predict(mod1, new = wbca, type = 'response') #pr(Y = 1|X)

yhat1 <- rep(NA, length(phat1))
yhat1 <- ifelse(phat1 > 0.5, 1,0 )
train_acc1 = mean( (wbca$Class == yhat1))

phat2 = predict(mod2, new = wbca, type = 'response') #pr(Y = 1|X)

yhat2 <- rep(NA, length(phat2))
yhat2 <- ifelse(phat2 > 0.5, 1,0 )

train_acc2 = mean( (wbca$Class == yhat2))

cat(train_acc1,train_acc2) # parameter가 많아질 수록 정확도 증가


# ii)
folds <- createFolds(wbca$Class, k = 10)

mod1.test_acc  = rep(NA, length(folds))
mod2.test_acc  = rep(NA, length(folds))


for (i in 1:length(folds)){
  training_indices <- setdiff(1:nrow(wbca), folds[[i]])
  test_indices <- folds[[i]]
  
  mod1 = glm(Class~ BNucl + Thick, family = binomial(), data = wbca, subset = training_indices)
  mod2 = glm(Class~ ., family = binomial(), data = wbca, subset = training_indices)
  
  phat1 = predict(mod1, new = wbca[test_indices,], type = 'response') # reponse를 해야 link function 정의 필요 x
  phat2 = predict(mod2, new = wbca[test_indices,], type = 'response')
  
  yhat1 = ifelse(phat1 > 0.5, 1, 0)
  yhat2 = ifelse(phat2 > 0.5, 1, 0)
  
  
  mod1.test_acc[i] = mean(wbca$Class[test_indices] == yhat1)
  mod2.test_acc[i] = mean(wbca$Class[test_indices] == yhat2)
  
}
mod1.cv.acc <- mean(mod1.test_acc)
mod2.cv.acc <- mean(mod2.test_acc)

cat(mod1.cv.acc, mod2.cv.acc)



# pr2

head(dicentric)

# i)
folds <- createFolds(dicentric$ca, k = 10)

mod1.test_mse  = rep(NA, length(folds))
mod2.test_mse  = rep(NA, length(folds))


for (i in 1:length(folds)){
  training_indices <- setdiff(1:nrow(dicentric), folds[[i]])
  test_indices <- folds[[i]]
  
  mod1 = glm(ca~ log(cells) + log(doserate)*doseamt, family = poisson(), data = dicentric, subset = training_indices)
  mod2 = glm(ca~ offset(log(cells)) + log(doserate)*doseamt, family = poisson(), data = dicentric, subset = training_indices)
  
  yhat1 = predict(mod1, new = dicentric[test_indices,], type = 'response') # reponse를 해야 link function 정의 필요 x
  yhat2 = predict(mod2, new = dicentric[test_indices,], type = 'response')

  
  mod1.test_mse[i] = mean( (dicentric$ca[test_indices] - yhat1)^2 )
  mod2.test_mse[i] = mean( (dicentric$ca[test_indices] - yhat2)^2 )
  
}
mod1.cv.mse <- mean(mod1.test_mse)
mod2.cv.mse <- mean(mod2.test_mse)

cat(mod1.cv.mse, mod2.cv.mse)

mod1.test_mse

