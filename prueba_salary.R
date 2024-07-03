## install_packages

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
library("tidyverse")
library("ggplot2")
library("dplyr")

## load data to r

employees_salary <- read.csv("employee_salaries.csv")

## Dimensions and structure of the data set
 
dim(employees_salary)

## the structure of the variables

str(employees_salary)
head(employees_salary)
colnames(employees_salary)

is.na(employees_salary)

## descriptive analysis

summary(employees_salary)

## 

ggplot(employees_salary, aes(x = Base.Salary, y = Department)) +
  geom_bar(stat = "identity")


ggplot(employees_salary, aes(x = Department , y = Base.Salary)) +
  geom_point(stat = "identity")

##

th <-  theme(axis.title = element_text(), axis.title.x = element_text()) 

ggplot(employees_salary, aes(Base.Salary)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "orange") + th +
  geom_density(alpha = 0.2, fill = "orange") + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(employees_salary$Base.Salary), 2), size = 2, linetype = 3)



ggplot(employees_salary, aes(Base.Salary)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "orange") + 
  geom_density(alpha = 0.2, fill = "orange") +
  th +
  ggtitle("Distribución De Salario",
          subtitle = expression("Con" ~'log'[10] ~ "de transformación en el eje X")) +
  #theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(employees_salary$Base.Salary), 2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("text", x = 1800, y = 0.75,label = paste("Media Precio = ", paste0(round(mean(employees_salary$Base.Salary), 2), "$")),
           color =  "#32CD32", size = 8)


### precio medio por departamento


employees_salary %>% group_by(Department) %>% summarise(Avg_price = mean(Base.Salary)) %>%
  ggplot(aes(x = Department, y = Avg_price, fill = Department))+geom_bar(stat="identity") + 
  labs(title = "Diagrama de barras",
       subtitle = "Precio medio por Departamento",
       x = "Departamento",
       y = "Precio medio")

## precio medio por posision 
employees_salary %>% group_by(Position.Title) %>% summarise(Avg_price = mean(Base.Salary)) %>%
  ggplot(aes(x = Position.Title, y = Avg_price, fill = Position.Title))+geom_bar(stat="identity") + 
  labs(title = "Diagrama de barras",
       subtitle = "Precio medio por Departamento",
       x = "Departamento",
       y = "Precio medio")


## 
ggplot(employees_salary, aes(x = fct_infreq(Department), fill= Employee.Status))+
geom_bar()+
  labs(title = "Tipo de propiedad por vecindario ",
       x = "Vecindario", y = "Numero de listados") +
  theme(legend.position = "right")