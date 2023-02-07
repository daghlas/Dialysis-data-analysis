#R SCRIPT
#Adverse effects of hemodialysis in patients with chronic kidney insufficiency

#loading packages
#install.packages()

#installing packages
library(tidyverse)
library(dplyr)

#importing dataset to R
library(readxl)
dialysis <- read_excel("C:/Users/USER/Desktop/Assignment on R/heamodialysis_dataset.xlsx")
#View(dialysis)
#rm(dialysis)

#No of observation
nrow(dialysis)

#No of variables
dim(dialysis)

#type of variable weight
str(dialysis$weight)

#type of variable sex
str(dialysis$sex)

#cleaning variable comorbidities
table(dialysis$comorbidities)
dialysis <- dialysis %>% mutate(comorbidities=ifelse(comorbidities=="yeah", "yes", comorbidities))
dialysis <- dialysis %>% mutate(comorbidities=ifelse(comorbidities=="none", "no", comorbidities))

#cleaning variable dialysis_perc
table(dialysis$diagnostic_cause)
dialysis <- dialysis %>% mutate(diagnostic_cause=ifelse(diagnostic_cause=="n/a", "undetermined", diagnostic_cause))

#missing values in weight variable, cleaning weight variable
dialysis$weight <- as.numeric(dialysis$weight)
#row position of missing values
which(is.na(dialysis$weight))
#cleaning weight
dialysis$weight[is.na(dialysis$weight)] <- 0
dialysis <- dialysis %>% mutate(weight=ifelse(weight<=50, 70, weight))

#creating new variable BMI
dialysis <- dialysis %>% mutate(dialysis, heightM=height*0.0254)
dialysis <- dialysis %>% mutate(dialysis, BMI=weight/(heightM*heightM))
#rm(heightM)

#average and median value
mean(dialysis$BMI)
median(dialysis$BMI)
summary(dialysis$BMI)

#creating new variable BMIcat
dialysis <- dialysis %>% mutate(
  BMIcat=ifelse(BMI<=20.4, "Under", ifelse(BMI>=20.5 & BMI<=27.9, "Normal", "Over"))
)

#persons still in heamodialysis
sum(dialysis$patient_final_situation == 'heamodialysis')
#men who are dead
sum(dialysis$sex == 'M' & dialysis$patient_final_situation == 'died')
#women over 45 yrs with comorboid conditions
sum(dialysis$sex == 'F' & dialysis$comorbidities == 'yes')
#percentage of men over 60 yrs with glomerulonephritis
t = sum(dialysis$sex == 'M' & dialysis$diagnostic_cause == 'glomerulonephritis' & dialysis$age >= 60)
perc = (t/55)*100
print(perc)

#cleaning the sex variable
table(dialysis$sex)
dialysis <- dialysis %>% mutate(sex=ifelse(sex=="Mm", "M", sex))
dialysis <- dialysis %>% mutate(sex=ifelse(sex=="m", "M", sex))
dialysis <- dialysis %>% mutate(sex=ifelse(sex=="f", "F", sex))
dialysis <- dialysis %>% mutate(sex=ifelse(sex=="Ff", "F", sex))
#generating variable gender from sex variable
dialysis <- dialysis %>% mutate(gender=ifelse(sex=="M", "Male", "Female"))

#pie chart, scatter, box and bar plots
#gender pie chart plot
x <- c(length(which(dialysis$gender == 'Male')),length(which(dialysis$gender == 'Female')))
y <- c('Male','Female')
pie(x,y)

#diagnostic_cause bar graph
br <- table(dialysis$diagnostic_cause)
barplot(br)

#age by weight scatter plot
scatter.smooth(dialysis$age, dialysis$weight)
#weight by comorbidities box plot
b <- table(dialysis$weight, dialysis$comorbidities)
boxplot(b)
#categorical BMI cat bar plot
br <- table(dialysis$BMIcat)
barplot(br)

View(dialysis)
