library(tidyverse)
library(ggplot2)
#import the tables
#create single df - for single larvae, get averages (mode, medium, mean) for behaviour frequency and duration
#create df for single and group larvae - get averages (mode, medium, mean) for group by size and compare to single larvae.

#import tables for single larvae
#first 5 min
df_fSingle = read.csv("F:\\HoverFly\\BORIS_csv\\first 5 min\\all_in_one.csv")
df_sSingle = read.csv("F:\\HoverFly\\BORIS_csv\\second 5 min\\all_in_one.csv")

tail(df_fSingle)
tail(df_sSingle)

plot(Behavior ~ Total_duration_(s), data=df_fSingle)

dev.off()
head(df_fSingle)

#f
df_fSingle$Observation = factor(df_fSingle$Obsersvation)
ggplot(df_fSingle, aes(x=Total_number_of_occurences, y=Total_duration_.s.))+
  geom_jitter(position=position_jitter(width=1), alpha=0.4, aes(color=Observation))+
  facet_grid(.~Behavior)

dev.off()

ggplot(df_fSingle, aes(x=Total_number_of_occurences, y=Duration_std_dev))+
  geom_jitter(position=position_jitter(width=1), alpha=0.4, aes(color=Observation))+
  facet_grid(.~Behavior)

head(df_fSingle)

####
#mean mode and medium for entire s or f group

#/ by observations (13, not each entry)

###needs work
df_fSingle%%
  select(df_fSingle$Observation, df_fSingle$Total_number_of_occurences, df_fSingle$Total_duration_.s., df_fSingle$Behavior)%%
  filter(Obsersvation)%%
  summarise(mean_duration = round(mean(Total_duration_.s.), digits =2))

#plots
#s
df_sSingle$Observation = factor(df_sSingle$Observation)
ggplot(df_sSingle, aes(x=Total_number_of_occurences, y=Total_duration_.s.))+
  geom_jitter(position=position_jitter(width=1), alpha=0.4, aes(color=Observation))+
  facet_grid(.~Behavior)

df_sSingle$Observation = factor(df_sSingle$Observation)
ggplot(df_sSingle, aes(x=Total_number_of_occurences, y=Duration_std_dev))+
  geom_jitter(position=position_jitter(width=1), alpha=0.4, aes(color=Observation))+
  facet_grid(.~Behavior)


#-what behaviours are exhibited, how frequently, for how long.

#model with behaviour, 
head(df_fSingle)
#frequency = number of occurances/ 2min(120s)

#group by Observation

df_fSingle
obsf = df_fSingle%>%
  group_by(Observation)%>%
  select(- Modifiers)
obsf

obsS = df_sSingle%>%
  group_by(Observation)%>%
  select(-Modifiers)

########################################
glimpse(df_fSingle)
#to see categorical values, factors are assigned
levels(factor(df_fSingle$Behavior))
#verifying continuous variables
continuous = select_if(df_fSingle, is.numeric)
summary(continuous)

#linear model

#Binomial =  when two possible outcomes- Boolean
#Poisson  =  counting the number of__. COUNT DATA.
#Gaussian =  when outcome is expressed as number that can have a fractional value. Normal Distribution.

lm_f_Single = lm(Total_duration_.s. ~ Total_number_of_occurences +Observation + Duration_std_dev + Behavior ,data= df_fSingle)


par(mfrow = c(2,2))
plot(lm_f_Single)

anova(lm_f_Single)


#####grouped in BORIS
#created csv of all observations for first 5 and second 5 mins

df_Sin1_G = read.csv("F:\\HoverFly\\BORIS_csv\\first 5 min\\GTB.csv")
#16 obs
df_Sin2_G = read.csv("F:\\HoverFly\\BORIS_csv\\second 5 min\\GTB.csv")
#14 obs


#remove modifiers col
head(df_Sin2_G)
df_S_f = df_Sin1_G%>%
  select(-Modifiers)

df_S_s = df_Sin2_G%>%
  select(-Modifiers)

#16 observations
#frequency = event per second
#16 events at 120s
t =16*120
#freq of behaviour = behaviour occurance / t

#frequency table
freq_t = table(df_S_f)
freq_t
hist(df_S_f)

#averaging of total data
#df_S_f%>%
#  select(Behavior, Total.number.of.occurences, Total.duration..s.)%>%
#  summarise(mean_time = Total.duration..s./ t)%>%
def_new = df_S_f%>%
  for(i in 1:nrow(df_S_f)){
    summarise(std_duration = sd(df_S_f$Total.duration..s., na.rm = TRUE))
  }
def_new
#NULL   obj returned when expression or function results in undefined value
head(df_S_f)
#14 observations
#freq = occurances /14

#GRAPHS

ggplot(df_sSingle, aes(x=Total_number_of_occurences, y=Total_duration_.s.))+
  geom_jitter(position=position_jitter(width=1), alpha=0.4, aes(color=Observation))+
  facet_grid(.~Behavior)


df_maths = read.csv("F:\\HoverFly\\BORIS_csv\\smaller behaviour names.csv")
head(df_maths)

ggplot(df_maths, aes(x=F.S, y= frequency))+geom_boxplot()+facet_grid(~ï..Behaviour)

ggplot(df_maths, aes(x=ï..Behaviour, y=frequency))+geom_boxplot()+facet_grid(~ F.S)

ggplot(df_maths, aes(x=ï..Behaviour, y=frequency))+geom_boxplot()+scale_x_discrete("F.S")
#
cont_freqLM = lm(frequency ~ F.S + factor(ï..Behaviour), data=df_maths)
anova(cont_freqLM)
#i don't know why it doesn't list each type of behaviour



#Group Behaviour
df_G_f = read.csv("F:\\HoverFly\\BORIS_csv\\Group3\\FH.csv")
# 10 obs
df_G_s =  read.csv("F:\\HoverFly\\BORIS_csv\\Group3\\SH.csv")
#10 obs
df_G_fs = read.csv("F:\\HoverFly\\BORIS_csv\\Group3\\SH&FH.csv")


df_G_f = df_G_f%>%
  select(-Modifiers)
df_G_s = df_G_s%>%
  select(-Modifiers)

head(df_G_f)
ggplot(df_G_f, aes(x=Behavior, y= Duration.mean..s.))+
  geom_boxplot()+
  scale_x_discrete(limits=c("StlS", "StlB", "Sg ", "SRM", "LRM", "CW", "CD", "Fl", "Si", "W", "EBT", "RBT"))



ggplot(df_G_s, aes(x=Behavior, y= Duration.mean..s.))+
  geom_boxplot()+
  scale_x_discrete(limits=c("StlS", "StlB", "Sg ", "SRM", "LRM", "CW", "CD", "Fl", "Si", "W", "EBT", "RBT"))

head(df_G_fs)
ggplot(df_G_fs, aes(x=Behavior, y= Duration.mean..s.))+
  geom_boxplot()+
  facet_grid(~F.S)

ggplot(df_G_fs, aes(x=F.S, y= Duration.mean..s.))+
  geom_boxplot()+
  facet_grid(~Behavior)

g3_lm = lm(Duration.mean..s.~ F.S +X..of.total.length + Behavior + Total.number.of.occurences +inter.event.intervals.mean..s.  ,data= df_G_fs)
par(mfrow =c(2,2))
plot(g3_lm)
#scale_x_discrete(limits=c("StlS", "StlB", "Sg ", "SRM", "LRM", "CW", "CD", "Fl", "Si", "W", "EBT", "RBT"))
g3_lm
anova(g3_lm)
#unrealistic that all are significant- must be because all are relient on each other

#How do i compare if there is significant difference for results between F and S?


#############################################################

############################################################

#DLC
par(mfrow=c(1,1))

df_Single_V1 = read.csv("F:\\HoverFly\\Video\\Group Vs Individual\\9.7.21\\1\\single\\1 min edits\\single.9.7.21_V1.0DLC_resnet50_Test_Hoverfly_1Jun26shuffle1_5000.csv")
head(df_Single_V1)

lm_Single_v1 = lm(,data=df_Single_v1)
