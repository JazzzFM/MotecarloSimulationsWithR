library(tidyverse)
library(ggplot2)
library(shiny)

df <- read.csv("C:/Users/Jorge/Documents/Programacion/Proyectos/Titanic_Probabilidad/Datasets/Titanic/train.csv", 
                 row.names = 1)
names(df)

head(df)



ggplot(df, aes(x=Age, color=Sex)) +
  geom_histogram(fill="white")

ggplot(df, aes(x=Age, color=Sex)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")

#AGE RESPECT SEX

ggplot(df, aes(x=Age, color=Sex, fill=Sex)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=df, aes(xintercept=mean(df[["Age"]]), color=Sex),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Weight histogram plot",x="Age", y = "Density")+
  theme_classic()

#AGE RESPECT SURVIVAL 
##ERROR##

ggplot(df, aes(x=Age, color=Survived, fill=Survived)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  geom_vline(data=df, aes(xintercept=mean(df[["Age"]]), color=Survived),
             linetype="dashed")+
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  labs(title="Weight histogram plot",x="Age", y = "Density")+
  theme_classic()