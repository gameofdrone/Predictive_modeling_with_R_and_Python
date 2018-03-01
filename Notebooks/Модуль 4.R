# устанавливаем пакет randomForest
# install.packages("randomForest")

# загружаем пакет randomForest
library(randomForest)

# загружаем данные
data <- read.csv2("C:/Trees/RF_classification.csv")

# смотрим данные
data

# задаем стартовое значение генератора случайных
# чисел для воспроизводимости результатов
set.seed(152)

# строим случайный лес деревьев классификации
model<-randomForest(default ~., data, ntree=20, importance=TRUE, norm.votes=FALSE)

# выводим информацию об ошибке классификации
# по методу OOB
print(model)

# вычисляем прогнозы по методу OOB
oob_predictions <- predict(model, type="response")
results <-data.frame(data, result=oob_predictions)
results

# смотрим голоса деревьев, построенных
# по out-of-bag выборкам, для каждого класса
oob_votes <- predict(model, type="vote", norm.votes=FALSE)
oob_votes

# вычисляем вероятности классов по методу OOB
oob_probabilities <- predict(model, type="prob")
oob_probabilities

# смотрим процентные доли голосов деревьев, построенных
# по out-of-bag выборкам, для каждого класса
oob_votes <- predict(model, type="vote", norm.votes=TRUE)
oob_votes

# вычисляем прогнозы по обычному методу
predictions <- predict(model, data, type="response")
results <-data.frame(data, result=predictions)
results

# смотрим голоса деревьев, построенных по всем 
# бутстреп-выборкам, для каждого класса
votes <- predict(model, data, type="vote", norm.votes=FALSE)
votes

# вычисляем вероятности классов по методу OOB
probabilities <- predict(model, data, type="prob")
probabilities

# смотрим процентные доли голосов деревьев, построенных
# по всем бутстреп-выборкам, для каждого класса
votes <- predict(model, data, type="vote", norm.votes=TRUE)
votes


