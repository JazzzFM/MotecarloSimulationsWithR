library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
#Class-Function for mean, median, std, etc.
stat <- function(df){
  df[is.na(df)] <- 0.0
  
  values <- list(long = length(df), media = mean(df), mediana = median(df), moda = mode(df), varianza = var(df), std = sqrt(var(df)))

  values
  
}
# Load Titanic data for analysis. Open in spreadsheet view.
titanic <- read.csv("C:/Users/Jorge/Documents/Programacion/Proyectos/Titanic_Probabilidad/Datasets/Titanic/train.csv", 
                    stringsAsFactors = FALSE)
View(titanic)

colnames(titanic)
# Set up factors.
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)


Age_info <- stat(titanic$Age)
Fare_info <- stat(titanic$Age)
Age_info$std
Fare_info$media
Fare_info$std



#Survival rates by sex

p<-prop.table(table(titanic$Sex,titanic$Survived),2)

ggplot(titanic, aes(x = Sex, fill = Survived)) +
  scale_fill_discrete(name = "Sexo", labels = c("Mujer", "Hombre"))+
  theme_bw() +
  geom_bar() +
  labs(x = 'Sexo',y = "Número de pasajeros",
       title = "Sobrevivencia por sexo")

ggplot(as.data.frame(p), aes(x=Var2,y=Freq,fill=Var1)) +
  scale_fill_discrete(name = "Sexo", labels = c("Mujer", "Hombre"))+
  theme_bw() +
  geom_col() +
  labs(x = '0=Muertos,1=Vivos',y = "Porcentaje de pasajeros",
  title = "Porcentaje de sobrevivencia por sexo")

# Survival rates by Passenger class 

p<-prop.table(table(titanic$Pclass,titanic$Survived),2)

ggplot(titanic, aes(x = Pclass, fill = Survived)) + 
  theme_bw() +
  geom_bar() +
  labs(x = 'Sexo',y = "Número de pasajeros",
       title = "Sobrevivencia por clase de pasajero")

ggplot(as.data.frame(p), aes(x=Var2,y=Freq,fill=Var1)) +
  scale_fill_discrete(name = "Clase", labels = c("Alta", "Media", "Baja"))+
  theme_bw() +
  geom_col() +
  labs(x = '0=Muertos,1=Vivos',y = "Porcentaje de pasajeros",
       title = "Porcentaje de sobrevivencia por clase de pasajero")



#Survival rate by class of ticket and gender
ggplot(titanic, aes(x = Sex, fill = Survived)) + 
  scale_fill_discrete(name = "Sobrevivencia", labels = c("Muertos", "Vivos"))+
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(x="Sexo", y = "Número de pasajeros",
       title = "Sobrevivencia por clase de pasajero y sexo")



#Histogram distribution by Age

ggplot(titanic, aes(x = Age)) +
  geom_histogram(alpha=0.5,binwidth = 5,fill=("#003666")) +
  labs(y = "Número de pasajeros",
       x = "Edad",
       title = "Distribución por edad")
#Please overlap this two graphs
ggplot(titanic, aes(x = Age)) +
  geom_density(alpha = 0.2, fill = "#ce181e") +
  labs(y = "Número de pasajeros",
       x = "Edad",
       title = "Distribución por edad")

#Survival rate by age, class and sex

ggplot(titanic, aes(x = Age, fill = Survived)) +
  scale_fill_discrete(name = "Sobrevivencia", labels = c("Muertos", "Vivos"))+
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) +
  labs(y = "Edad",
       x = "Sobrevivencia",
       title = "Sobrevivencia por edad, clase de pasajero y sexo")

# If you prefer histograms, no problem!
ggplot(titanic, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")
