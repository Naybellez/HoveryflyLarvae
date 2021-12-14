#Time Trials


library(tidyverse)
library(ggplot2)
library(ggpubr)
library(showtext)
library(ragg)
showtext_auto()
font_add_google("EB Garamond",family = "gara")


df_Time = read.csv("C:\\Users\\Nay\\OneDrive\\Desktop\\DRAFTS\\Time Trials.csv")

df_t = df_Time %>%
  select(Condition, X.Speed..cm.s.)%>%
  group_by(Condition)%>%
  summarise(meann = mean(X.Speed..cm.s.),
            SE = std_err(X.Speed..cm.s.),
            stdDev = sd (X.Speed..cm.s.))%>%
  print.data.frame(.)

df_tt = left_join(df_Time, df_t)

library (tidyr)

ggplot(df_tt, aes(x = Width..cm.,  y= Trial.Average.Time..s.))+
  geom_jitter(position=position_jitter())+
  facet_grid(~Condition)+
  labs( title = "Time Trials", 
        subtitle = "Average Time Taken for Larva to Move Out of View",
        x = "Distance Travelled (cm)", 
        y = " Time (s)")+
  theme(text= element_text( family ='gara'))
  
  

 df_to = f_tt %>%
  select(Condition, Trial.1.Time..s., Trial.2.Time..s., Trial.3.Time..s.)%>%
  pivot_longer(df_Time, cols= c("Trial.1.Time..s.", "Trial.2.Time..s.", "Trial.3.Time..s."), names_to = "Var", values_to ="Val")
 
 ggplot(df_to, aes (x = Var, y = X.Speed..cm.s))+
   geom_boxplot()
  
   
