#read data
hr.emp.data.df <-
  read.csv("C:/my learning journey/practice/HR_Employee_Attrition_Data.csv")

#check dimension and names
dim(hr.emp.data.df)
nrow(hr.emp.data.df)
names(hr.emp.data.df)

#check for null value
sum(is.na(hr.emp.data.df)) # no null values

head(hr.emp.data.df)
tail(hr.emp.data.df)

#check for the data type
str(hr.emp.data.df) #the data types mix of numeric and factors

#dependent variable is Attrition having 2 levels Yes and No

# Education, EnvironmentSatisfaction, JobInvolvement, JobLevel, JobSatisfaction, NumCompaniesWorked, StockOptionLevel,
# TrainingTimesLastYear, WorkLifeBalance, PerformanceRating, RelationshipSatisfaction can be converted into factors

# five point summary
summary(hr.emp.data.df)

# converting the above variables into factors
hr.emp.data.df$Education <- as.factor(hr.emp.data.df$Education)
hr.emp.data.df$EnvironmentSatisfaction <-
  as.factor(hr.emp.data.df$EnvironmentSatisfaction)
hr.emp.data.df$JobInvolvement <-
  as.factor(hr.emp.data.df$JobInvolvement)
hr.emp.data.df$JobLevel <- as.factor(hr.emp.data.df$JobLevel)
hr.emp.data.df$JobSatisfaction <-
  as.factor(hr.emp.data.df$JobSatisfaction)
hr.emp.data.df$StockOptionLevel <-
  as.factor(hr.emp.data.df$StockOptionLevel)
hr.emp.data.df$WorkLifeBalance <-
  as.factor(hr.emp.data.df$WorkLifeBalance)
hr.emp.data.df$PerformanceRating <-
  as.factor(hr.emp.data.df$PerformanceRating)
hr.emp.data.df$RelationshipSatisfaction <-
  as.factor(hr.emp.data.df$RelationshipSatisfaction)

str(hr.emp.data.df)

summary(hr.emp.data.df)

library(ggplot2)

## Exploratory analysis of each variable on the dependent variable "Attrition"

#boxplot of age and attrition
ggplot(hr.emp.data.df, aes(x = Attrition, y = Age, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#significant age difference across 2 levels attrition level yes and attrition level no
aovstat <- aov(Age ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

#majority of employees leaving the org are around 30 Years
ggplot(hr.emp.data.df, aes(Age, fill = Attrition)) + geom_density() + facet_grid( ~
                                                                                    Attrition)

#Attrition contingency table
prop.table(table(hr.emp.data.df$Attrition)) # 84% no attrition in the data and 16% where attrition value is yes

#bar plot for Attrition attribute
ggplot(hr.emp.data.df, aes(x = Attrition, fill = Attrition)) + geom_bar(aes(color =
                                                                              Attrition))

#BusinessTravel
table(hr.emp.data.df$BusinessTravel)

#boxplot of age and BusinessTravel
ggplot(hr.emp.data.df,
       aes(x = BusinessTravel, y = Age, fill = BusinessTravel)) + geom_boxplot() +
  coord_flip()

##chi square Attrition and BusinessTravel
chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$BusinessTravel,
           correct = FALSE) #significant difference

#how attrition level differs across business travel, more attrition in the Travel groups
ggplot(hr.emp.data.df, aes(x = BusinessTravel, fill = Attrition)) + geom_bar()

#Department
table(hr.emp.data.df$Department)
table(hr.emp.data.df$Attrition, hr.emp.data.df$Department)

#low attrition in HR because of low population in HR group
ggplot(hr.emp.data.df, aes(x = Department, fill = Attrition)) + geom_bar(position = "dodge")

#chi square Attrition and Department
chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$Department,
           correct = FALSE) #significant difference

#DistanceFromHome...density plot
ggplot(hr.emp.data.df, aes(x = DistanceFromHome)) + geom_density()

#more attrition near home rather than away from home
ggplot(hr.emp.data.df, aes(x = DistanceFromHome, fill = Attrition)) + geom_density()


#Education..level 5 is doctor and very less attrition due to low representation
table(hr.emp.data.df$Attrition, hr.emp.data.df$Education)
ggplot(hr.emp.data.df, aes(x = Education, fill = Attrition)) + geom_bar(position = "dodge")

#EducationField..very less attrition due to low representation of HR group
table(hr.emp.data.df$Attrition, hr.emp.data.df$EducationField)
ggplot(hr.emp.data.df, aes(x = EducationField, fill = Attrition)) + geom_bar(position = "dodge")

#chi square analysis for attrition and education/education field
chisq.test(hr.emp.data.df$Attrition, hr.emp.data.df$Education, correct =
             FALSE) #not significant

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$EducationField,
           correct = FALSE) #significant

unique(hr.emp.data.df$EmployeeCount) # one value of 1

unique(hr.emp.data.df$EmployeeNumber) #Employee ID value
# we can conclude that EmployeeCount and EmployeeNumber are not significant variables and we can leave out these
# 2 while building the model

#EnvironmentSatisfaction
table(hr.emp.data.df$Attrition,
      hr.emp.data.df$EnvironmentSatisfaction)
ggplot(hr.emp.data.df,
       aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$EnvironmentSatisfaction,
           correct = FALSE) #significant

#Gender
table(hr.emp.data.df$Attrition, hr.emp.data.df$Gender)
ggplot(hr.emp.data.df, aes(x = Gender, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition, hr.emp.data.df$Gender, correct = FALSE) # not significant

#JobInvolvement... Ratings stand for 1 'Low' 2 'Medium' 3 'High' 4 'Very High'
## high attrition for low involved jobs
table(hr.emp.data.df$Attrition, hr.emp.data.df$JobInvolvement)
ggplot(hr.emp.data.df, aes(x = JobInvolvement, fill = Attrition)) + geom_bar(position = "dodge")

ggplot(hr.emp.data.df, aes(x = JobInvolvement, fill = Attrition)) + geom_bar(position =
                                                                               "dodge") + facet_grid( ~ BusinessTravel)

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$JobInvolvement,
           correct = FALSE) # significant

# as job level increase from 1 to 5, the attrition rate decreases
table(hr.emp.data.df$Attrition, hr.emp.data.df$JobLevel)
ggplot(hr.emp.data.df, aes(x = JobLevel, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition, hr.emp.data.df$JobLevel, correct =
             FALSE) # significant

#jobrole
table(hr.emp.data.df$Attrition, hr.emp.data.df$JobRole)
ggplot(hr.emp.data.df, aes(x = JobRole, fill = Attrition)) + geom_bar(position = "dodge")
#in one of the plots we created, the Sales Representative job role has significantly more attrition relative to other job roles.
chisq.test(hr.emp.data.df$Attrition, hr.emp.data.df$JobRole, correct = FALSE) # significant

#breakdown of distance from home by job role and attrition
ggplot(hr.emp.data.df,
       aes(x = JobRole, y = DistanceFromHome, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#breakdown of HourlyRate by job role and attrition
ggplot(hr.emp.data.df, aes(x = JobRole, y = HourlyRate, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#sales representatives that travel frequently have very high attrition rates
ggplot(hr.emp.data.df, aes(x = BusinessTravel, fill = Attrition)) + geom_bar(position =
                                                                               "dodge") + facet_grid( ~ JobRole) +
  theme(axis.text.x = element_text(
    angle = 45,
    hjust = 1,
    vjust = 0.5
  ))


#JobSatisfaction
table(hr.emp.data.df$Attrition, hr.emp.data.df$JobSatisfaction)
ggplot(hr.emp.data.df, aes(x = JobSatisfaction, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$JobSatisfaction,
           correct = FALSE) # significant

#MaritalStatus...attrition rate hihger in Single and lowest for divorced
table(hr.emp.data.df$Attrition, hr.emp.data.df$MaritalStatus)
ggplot(hr.emp.data.df, aes(x = MaritalStatus, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$MaritalStatus,
           correct = FALSE) # significant

#MonthlyIncome...high attrition corresponds to low monthly income
ggplot(data = hr.emp.data.df, aes(MonthlyIncome, fill = Attrition)) + geom_density()

#compare average monthly income by education and attrition
ggplot(hr.emp.data.df,
       aes(x = Education, y = MonthlyIncome, fill = Attrition)) + geom_boxplot() +
  coord_flip()

ggplot(hr.emp.data.df,
       aes(x = EducationField, y = MonthlyIncome, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#NumCompaniesWorked...attrition higher for people who worked in 1 or less then 1 company before
ggplot(data = hr.emp.data.df, aes(NumCompaniesWorked, fill = "red")) + geom_bar()
table(hr.emp.data.df$Attrition, hr.emp.data.df$NumCompaniesWorked)
ggplot(hr.emp.data.df, aes(x = NumCompaniesWorked, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$NumCompaniesWorked,
           correct = FALSE) # significant

#Over18 # only one value of Y
# since only one value hence not significant for the model

#OverTime
table(hr.emp.data.df$Attrition, hr.emp.data.df$OverTime)

#The next plot could also be very interesting. It shows how overtime (as a proxy for hourly vs. exempt employees) affects attrition.
#As can be seen above, hourly employees have significantly higher attrition rates as compared to exempt employees with no overtime.

ggplot(hr.emp.data.df, aes(x = OverTime, fill = Attrition)) + geom_bar(position = "dodge")

#There is high attrition among hourly sales representatives and lab technicians
ggplot(hr.emp.data.df, aes(x = OverTime, fill = Attrition)) + geom_bar(position =
                                                                         "dodge") + facet_grid( ~ JobRole)

#PercentSalaryHike
ggplot(data = hr.emp.data.df, aes(PercentSalaryHike, fill = "red")) + geom_bar()

ggplot(hr.emp.data.df,
       aes(x = Attrition, y = PercentSalaryHike, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#more attrition for less than 15% hike
ggplot(hr.emp.data.df, aes(PercentSalaryHike, Attrition)) + geom_point(size =
                                                                         4, alpha = 0.01)

#PerformanceRating...less attrition for rating 4 people
table(hr.emp.data.df$Attrition, hr.emp.data.df$PerformanceRating)
ggplot(hr.emp.data.df, aes(x = PerformanceRating, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$PerformanceRating,
           correct = FALSE) # significant

#RelationshipSatisfaction...proportion of attrition more in 1 and 2 level of Relationship Satisfaction
table(hr.emp.data.df$Attrition,
      hr.emp.data.df$RelationshipSatisfaction)
ggplot(hr.emp.data.df,
       aes(x = RelationshipSatisfaction, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$RelationshipSatisfaction,
           correct = FALSE) # significant

unique(hr.emp.data.df$StandardHours) # 80
# since only one value hence not significant for the model

# more attrition with no stockoptionlevel level
table(hr.emp.data.df$Attrition, hr.emp.data.df$StockOptionLevel)
ggplot(hr.emp.data.df, aes(x = StockOptionLevel, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$StockOptionLevel,
           correct = FALSE) # significant

#TotalWorkingYears...more attrition for less totalworkingyears <=10 years
ggplot(data = hr.emp.data.df, aes(TotalWorkingYears)) + geom_density()

ggplot(data = hr.emp.data.df, aes(TotalWorkingYears, fill = Attrition)) +
  geom_density()

#another plot that shows attrition across job levels
#attrition is a lot higher when job level = 1.
ggplot(hr.emp.data.df, aes(x = JobLevel, fill = Attrition)) + geom_bar(position = "dodge")

ggplot(hr.emp.data.df,
       aes(x = JobLevel, y = TotalWorkingYears, fill = Attrition)) + geom_boxplot() +
  coord_flip()

aovstat <- aov(TotalWorkingYears ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

#TrainingTimesLastYear...more attrition for level of 2 and 3
ggplot(data = hr.emp.data.df, aes(TrainingTimesLastYear, fill = Attrition)) +
  geom_bar()
aovstat <- aov(TrainingTimesLastYear ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

ggplot(hr.emp.data.df,
       aes(x = JobLevel, y = TrainingTimesLastYear, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#WorkLifeBalance...more attrition for 1/2/3 levels
table(hr.emp.data.df$Attrition, hr.emp.data.df$WorkLifeBalance)
ggplot(hr.emp.data.df, aes(x = WorkLifeBalance, fill = Attrition)) + geom_bar(position = "dodge")

chisq.test(hr.emp.data.df$Attrition,
           hr.emp.data.df$WorkLifeBalance,
           correct = FALSE) # significant

#work life balance and job role and attrition
ggplot(hr.emp.data.df, aes(x = WorkLifeBalance, fill = Attrition)) + geom_bar(position =
                                                                                "dodge") + facet_grid( ~ JobRole)

#YearsAtCompany...more attrition when <-10 years
ggplot(data = hr.emp.data.df, aes(YearsAtCompany, fill = Attrition)) + geom_density()
ggplot(data = hr.emp.data.df, aes(YearsAtCompany, fill = Attrition)) + geom_bar()
aovstat <- aov(YearsAtCompany ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

ggplot(hr.emp.data.df,
       aes(x = JobLevel, y = YearsAtCompany, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#YearsInCurrentRole...more attrition is <=5 years
ggplot(data = hr.emp.data.df, aes(YearsInCurrentRole, fill = Attrition)) +
  geom_density()
ggplot(data = hr.emp.data.df, aes(YearsInCurrentRole, fill = Attrition)) +
  geom_bar()
aovstat <- aov(YearsInCurrentRole ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

ggplot(hr.emp.data.df,
       aes(x = JobLevel, y = YearsInCurrentRole, fill = Attrition)) + geom_boxplot() +
  coord_flip()

ggplot(hr.emp.data.df,
       aes(x = EducationField, y = YearsInCurrentRole, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#YearsSinceLastPromotion...when less number of years then more attrition
ggplot(data = hr.emp.data.df, aes(YearsSinceLastPromotion, fill = Attrition)) +
  geom_density()
ggplot(data = hr.emp.data.df, aes(YearsSinceLastPromotion, fill = Attrition)) +
  geom_bar()
aovstat <-
  aov(YearsSinceLastPromotion ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

ggplot(hr.emp.data.df,
       aes(x = JobLevel, y = YearsSinceLastPromotion, fill = Attrition)) + geom_boxplot() +
  coord_flip()

ggplot(hr.emp.data.df,
       aes(x = EducationField, y = YearsSinceLastPromotion, fill = Attrition)) + geom_boxplot() +
  coord_flip()

#YearsWithCurrManager...more attrition when time with new manager is less
ggplot(data = hr.emp.data.df, aes(YearsWithCurrManager, fill = Attrition)) +
  geom_density()
ggplot(data = hr.emp.data.df, aes(YearsWithCurrManager, fill = Attrition)) +
  geom_bar()
aovstat <- aov(YearsWithCurrManager ~ Attrition, data = hr.emp.data.df)
summary(aovstat)
TukeyHSD(aovstat)

ggplot(hr.emp.data.df,
       aes(x = JobLevel, y = YearsWithCurrManager, fill = Attrition)) + geom_boxplot() +
  coord_flip()

ggplot(hr.emp.data.df,
       aes(x = EducationField, y = YearsWithCurrManager, fill = Attrition)) + geom_boxplot() +
  coord_flip()
