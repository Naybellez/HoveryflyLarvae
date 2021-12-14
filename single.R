library(tidyverse)
library(ggplot2)

df_DLC =read.csv("C:\\Users\\Nay\\OneDrive\\Desktop\\CSV\\Justmid.csv")

summary(df_DLC)
head(df_DLC)

#dlc = lm(ï..Midbody_x~ Midbody_y, data=df_DLC)
plot(df_DLC$ï..Midbody_x)
ggplot(df_DLC, aes(x=Midbody_x, y= Midbody_y, col=ï..scorer))+geom_jitter(alpha=0.09)

colnames(df_DLC)

plot(dlc)


