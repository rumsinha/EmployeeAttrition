## Variable Importance with Boruta and Caret packages
library(Boruta)
set.seed(123)
boruta.train <- Boruta(Attrition~., data = hr.emp.data.df, doTrace = 2)
print(boruta.train)
boruta.df <- attStats(boruta.train)
print(boruta.df)

library(caret)
set.seed(123)
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
rfe.train <- rfe(hr.emp.data.df[,-2], hr.emp.data.df[,2], sizes= c(1,3:35), rfeControl=control)# takes 15-20 min time to complete
rfe.train  ## the top 5 variables OverTime, DailyRate, HourlyRate, MonthlyRate, JobRole
plot(rfe.train,type=c("o","g"))
rfe.train$fit
rfe.train$optVariables
