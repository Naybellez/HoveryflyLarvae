#Single larva, 3 conditions

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(showtext)
library(ragg)
showtext_auto()
font_add_google("EB Garamond",family = "gara")

df_condi = read.csv("F:\\HoverFly\\BORIS_csv\\Muscid\\EnviroCondition.csv")
head(df_condi)

df_new_condi = df_condi %>%
  select(Behavior, Normalised_occurences...5min., Before.After, Condition, Observation)%>%
  group_by(Behavior, Condition)%>%
  summarise(
    Norm_occ = (Normalised_occurences...5min.),
    mean_B_oc = mean(Normalised_occurences...5min.),
    SE = std_err(Normalised_occurences...5min.),
    stdDev = sd(Normalised_occurences...5min.))%>%
  print.data.frame(.)

df_new_condi = df_new_condi%>%
  group_by(Condition, Behavior)%>%
  print.data.frame(.)

print.data.frame(df_new_condi)


df_LagC = df_new_condi%>%
  select(Behavior, Condition, mean_B_oc, stdDev)%>%
  print.data.frame(.)
df_LagC = df_LagC[!duplicated(df_joined_behav_1),]

ggplot(data= df_new_condi, aes(
  x= factor(Condition, levels = c("Cl", "Mos", "H")),
                               y=  Norm_occ,
                               fill = factor(Condition)
                              ))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Lagoon Condition" ,subtitle= "Behaviour Occurrence for Single larva in 3 Condition", caption= "Boxplot of behaviour occurance of single larva in 3 condition types:
       (1) Dirty water with Mosquito larvae; (2) Horizontal Lagoon with clear water and grass clumps; (3) Clear Water", x = "Condition type", y= "Behaviour Occurrence (Count / 5 larva minutes)",
       fill = "Lagoon Condition",
       scale_fill_manual(values = c("#d8b365", "#f5f5f5", "#5ab4ac")))+
  theme(text= element_text( family ='gara'))+
  theme(axis.text = element_text(size=7))

