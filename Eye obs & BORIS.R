#Behaviour stats              21.8.21
# Mean  of behaviour occurance
  # for first 5 min, for second 5 min (before and after stimulus)
        #& per T (if time)
#Std dev for behaviour occurance
#range of behaviour occurance


rm(list=ls())
#Import behaviour data
#Group 3

#install.packages("devtools")
#install.packages("ggpubr")
#install.packages("PairedData")
#install.packages("showtext")
#install.packages("ragg")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(showtext)
library(ragg)
showtext_auto()
font_add_google("EB Garamond",family = "gara")
font_add_google("Special Elite", family = "special")
#font-family: 'EB Garamond', serif;

df_behav_1 = read.csv("F:\\HoverFly\\BORIS_csv\\Group3\\SHFH.csv")
#correct
head(df_behav_1)

#Remove Modifiers col
df_behav_1 = df_behav_1%>%
  select(-Modifiers)

# STD ERR FUNCTION
std_err <- function(x) {
  SE <- sd(x)/sqrt(length(x))
  return (SE)
}

#Making Mean of behaviour occurnace
head(df_behav_1)
df_new_behav_1 = df_behav_1 %>%
  select(Behavior,Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(
    Total.number.of.occurences = (Total.number.of.occurences/3),
    mean_B_oc = mean(Total.number.of.occurences),
    SE = std_err(Total.number.of.occurences),
    stdDev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)

#need to group by behaviour and add up all the tot.num of all that behaviour- seporated by before or after.
  
#head(df_new_behav_1)
#print.data.frame(df_new_behav_1)
df_TTF = df_behav_1%>%
  select(-Total.number.of.occurences)
df_joined_behav_1 = left_join(df_TTF, df_new_behav_1)

#print.data.frame(df_joined_behav_1)
#we now have repeats, remove repeats
#df_joined_behav_1 = df_joined_behav_1[!duplicated(df_joined_behav_1),]%>%
#  print.data.frame(.)

#Success!

#Plot that BOI
#dev.off()


#ggplot(df_joined_behav_1, aes(x= Behavior, y = Total.number.of.occurences))
#didnt plot anything. t's ok, getting closer.
#ggplot(df_joined_behav_1, aes(x=Behavior, y = Total.number.of.occurences))+geom_boxplot()
#box plot fucking done m8


#next steps
#-multiple larvae - same thing but scale_x_discrete or facet_grid  / larvae
#compare first 5 min to second 5 min. do this for mean, stdDev, stderr of all before and all after. #for each exp, ie no stimulus, water stimulus and muscid stimulus. 
#for single larvae, 3 group, 5 group, 10/15 group.

#Single LArvae, GVI
#get Mean Std dev for all single first 5 min

#df_behSing = read.csv("F:\\HoverFly\\BORIS_csv\\SINGLE\\GTB_F.S.csv")
df_behSingAll = read.csv("F:\\HoverFly\\BORIS_csv\\SINGLE\\all_in_one_F.S.csv")
###correct
head(df_behSing)


#df_new_bSing = df_behSingAll %>%
 #   select(Behavior,Total.number.of.occurences, Before.After)%>%
 # group_by(Behavior, Before.After)%>%
 # summarise(
#    mean_B_oc = mean(Total.number.of.occurences),
#            SE = std_err(Total.number.of.occurences),
#            stdDev = sd(Total.number.of.occurences),
#            Before.After=Before.After)%>%
  print.data.frame(.)
df_new_bSing = df_behSingAll %>%
  select(Behavior,Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(
    Total.number.of.occurences = (Total.number.of.occurences),
              mean_B_oc = mean(Total.number.of.occurences),
               SE = std_err(Total.number.of.occurences),
               stdDev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)

#head(df_behSingAll)
#print.data.frame(df_new_bSing)
df_BRF = df_behSingAll%>%
  select(-Total.number.of.occurences)
df_joined_bSing = left_join(df_BRF, df_new_bSing)
head(df_joined_bSing)
#print.data.frame(df_joined_bSing)

#############################################
#Group_All

DF_GROUP_ALL = read.csv("F:\\HoverFly\\BORIS_csv\\GROUP\\Group_All_S&F.csv")
#correct
head(DF_GROUP_ALL)
#lagoon$Weather = gsub('Su', 'SS', lagoon$Weather)

  DF_GROUP_ALL$Subject = gsub("No focal subject", "Single larvae", DF_GROUP_ALL$Subject)

#df_Group_all = DF_GROUP_ALL %>%
#  select(Behavior, Total.number.of.occurences, Before.After)%>%
#  group_by(Behavior, Before.After)%>%
#  summarise(mean_B_oc = (mean(Total.number.of.occurences/5)),
#            SE = std_err(Total.number.of.occurences/5),
#            stdDev = sd(Total.number.of.occurences/5)
#            )%>%
#  print.data.frame(.)
#df_join_G_ALL = left_join(DF_GROUP_ALL, df_Group_all)%>%
#  print.data.frame(.)
df_Group_all = DF_GROUP_ALL %>%
  select(Behavior, Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(Total.number.of.occurences = (Total.number.of.occurences/5),
            mean_B_oc = mean(Total.number.of.occurences),
            SE = std_err(Total.number.of.occurences),
            stdDev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)



 DF_GROUP_ALLL =DF_GROUP_ALL%>%
   select(-Total.number.of.occurences)
 
 df_join_G_ALL = left_join(DF_GROUP_ALLL, df_Group_all)
  
#head(df_join_G_ALL)


##########################################################
#MUSCID SINGLE


DF_MUSC_SIN = read.csv("F:\\HoverFly\\BORIS_csv\\Muscid\\USEME\\M_Single.csv")  
#



 
df_musc_sin = DF_MUSC_SIN %>%
  select(Behavior, Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(mean_B_oc = mean(Total.number.of.occurences),
            SE = std_err(Total.number.of.occurences),
            stdDev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)
print.data.frame(df_musc_sin)


df_join_mu_Sin = left_join(DF_MUSC_SIN, df_musc_sin)

#print.data.frame(df_join_mu_Sin)

##############################################
#Musc group

DF_MUSC_GROUP = read.csv("F:\\HoverFly\\BORIS_csv\\Muscid\\USEME\\M_G5.csv")


#

df_musc_group = DF_MUSC_GROUP %>%
  select(Behavior, Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(Total.number.of.occurences = (Total.number.of.occurences/3))%>%
  print.data.frame(.)
df_musc_group

df_musc_group
df_Moo = DF_MUSC_GROUP%>%
  select(-ï..Subject)
df_join_mu_GG = left_join(df_Moo, df_Moo)
df_join_mu_GG
print.data.frame(df_join_mu_GG)

df_M_twoo = read.csv("F:\\HoverFly\\BORIS_csv\\Muscid\\USEME\\M_G2.csv")
df_mootoo = df_M_twoo %>%
  select(Behavior, Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(Total.number.of.occurences = (Total.number.of.occurences/2))%>%
  print.data.frame(.)
df_M2 = df_M_twoo%>%
  select(-ï..Subject)
df_M2
df_Mmmtwo = left_join(df_mootoo, df_M2)

df_join_mu_Gc = full_join(df_join_mu_GG, df_Mmmtwo)
df_join_mu_G = df_join_mu_Gc %>%
  group_by(Behavior, Before.After, Total.number.of.occurences)%>%
  summarise(mean_B_oc = mean(Total.number.of.occurences),
            SE = std_err(Total.number.of.occurences),
            stdDev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)

print.data.frame(df_join_mu_G)

#print.data.frame(df_join_mu_G)
##############################################

df_WATER_DIST = read.csv("F:\\HoverFly\\BORIS_csv\\EyeWaterDisturbance\\WaterDisturbance_control.csv")


head(df_WATER_DIST)
df_water_dist = df_WATER_DIST %>%
  select(Behavior, Total.number.of.occurences, Before.After)%>%
  group_by(Behavior, Before.After)%>%
  summarise(mean_B_oc = mean(Total.number.of.occurences),
            SE = std_err(Total.number.of.occurences),
            stdDev = sd(Total.number.of.occurences),
            )%>%
  print.data.frame(.)
df_join_water = left_join(df_WATER_DIST, df_water_dist)
print.data.frame(df_join_water)
 #head(df_join_water)

###############################################

##############################################

#WD & GvI I
head(df_joined_bSing)

df_Si = df_joined_bSing%>%
  group_by(Behavior, Before.After)%>%
  summarise(Condition = "GvI_I")

print.data.frame(df_mg)
df_Sinn = full_join(df_joined_bSing, df_Si)
print.data.frame(df_mgg)

df_Wa = df_join_water %>%
  group_by(Behavior, Before.After)%>%
  summarise(Condition = "WD")
df_wat = full_join(df_join_water, df_Wa)

df_WD_I = full_join(df_Sinn, df_wat)

ggplot(df_WD_I, aes( x= Condition, y = Total.number.of.occurences)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(factor(Before.After, levels = c("B", "A")) ~ Behavior)+
  labs(title = "Control Comparison",
       subtite = "Group Vs Individual and Water Disturbance conditions",
       x ="Condition",
       y = "Behaviour Occurrence (Count/ 2 larva minutes)",
       caption = "Boxplot displying Behaviour occurrences for Group Vs Individual (GvI) I and Water Disturbance conditions for both Before (B) and After (A) samples. outliers are shown in red cirlces
       CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing")+
  theme(text = element_text("gara"))














##########################################
#Now to analyse!!!!!
#Group 3
#df_joined_behav_1

#head(df_joined_behav_1)


  ggplot(df_joined_behav_1, aes(x = Behavior, y= Total.number.of.occurences)) + geom_boxplot()+
  facet_grid(~Before.After)
  
glm_b_1 = glm(Total.number.of.occurences ~ Before.After + Behavior, family=poisson, data = df_joined_behav_1)
 #summary(glm_b_1)

#head(df_joined_behav_1)

Before.After.Stimulus1 = factor(df_joined_behav_1$Before.After, levels=c("B", "A"))

ggplot(data= glm_b_1, aes(x=Before.After.Stimulus1, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour: Group of 3 Larvae" ,subtitle= "Mean Behaviour Occurrence", caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing", x = "Before & After The Halfway Mark", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))
  
   #Box & Whisker plot displaying the median, upper, lower and interquatile range
#and the total range of behaviour occurance displayed by larvae in clear-water conditions taken from two 2 minute
#video samples from the first 5 minutes and the second 5 minutes of a 10 minute observation


ggplot(df_joined_behav_1, aes(x=Before.After.Stimulus1, y=mean_B_oc))+
  geom_pointrange(aes(ymin=mean_B_oc - SE,
                      ymax = mean_B_oc + SE))+
 scale_x_discrete(limits=c("B", "A"))+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour: Group of 3 Larvae" ,subtitle= "Mean Behaviour Occurrence", 
  x= "Before and After Stimulus",
  y = "Behaviour Occurrence (Count / 2 larva minutes)",
  caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing")+
  theme(text= element_text( family ='gara'))
  

 #this is the jitter one, got extra dots
#ggplot(df_behav_1, aes(x=Before.After, y=Total.number.of.occurences))+
#  geom_jitter(position=position_jitter(), alpha=0.4)+
#  geom_pointrange(data= df_joined_behav_1, aes(ymin=mean_B_oc - SE,
#                      ymax = mean_B_oc + SE))+
#  scale_x_discrete(limits=c("B", "A"))+
 # facet_grid(~Behavior) +
#  labs(title="Group Vs Individual Behaviour: Group of 3 Larvae" ,subtitle= "Mean Behaviour Occurance", caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
#       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing")
  #The mean behaviour occurance displayed by lone larva in clear-water conditions taken from two 2 minute
#video samples from the first 5 minutes and the second 5 minutes of a 10 minute observation", y= "Mean Behaviour Occurance

####SOOOO CLOSE####

print.data.frame(df_joined_behav_1)
#Victory


#Single # Single # Single # Single
#df_joined_bSing

glm_Sin = glm(Total.number.of.occurences ~ Before.After + Behavior, family=poisson, data = df_joined_bSing)
summary(glm_Sin)

head(df_joined_bSing)

Before.After.StimulusS = factor(df_joined_bSing$Before.After, levels=c("B", "A"))
head(Before.After.Stimulus)

ggplot(data= glm_Sin, aes(x= Before.After.StimulusS, y= Total.number.of.occurences))+ geom_boxplot(outlier.colour = "red", outlier.shape = 1)+  
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour: Single larva" ,subtitle= "Mean Behaviour Occurrence", caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing", x = "Before & After The Halfway Mark", y= "Behaviour Occurrence  (Count / 2 larva minutes)") +
theme(text= element_text( family ='gara'))
 
   head(df_joined_bSing)
tail(df_joined_bSing)
ggplot(df_joined_bSing, aes(x=Before.After.StimulusS, y=mean_B_oc))+
  geom_pointrange(aes(ymin=mean_B_oc - SE,
                      ymax = mean_B_oc + SE))+
  scale_x_discrete(limits=c("B", "A"))+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour: Single larva" ,subtitle= "Mean Behaviour Occurrence", caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After The Halfway Mark", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))

 #Group Vs Individual Behaviour: Single larva" ,subtitle= "Mean Behaviour Occurance", caption= "The mean behaviour occurance displayed by lone larva in clear-water conditions taken from two 2 minute
#video samples from the first 5 minutes and the second 5 minutes of a 10 minute observation.", x = "Before & After The Halfway Mark

#b and a the same...

#What is different to the previous one?
print.data.frame(df_joined_behav_1)
print.data.frame(df_joined_bSing)

#Group# GROUP # GROUP # GROUP # GROUP
#df_join_G_ALL
glm_Group = glm(Total.number.of.occurences ~ Before.After + Behavior, family=poisson, data = df_join_G_ALL)
summary(glm_Group)

head(glm_Group)
print.data.frame(df_join_G_ALL)

Before.After.Stimulus_G = factor(df_join_G_ALL$Before.After, levels=c("B", "A"))

ID = factor(df_join_G_ALL$Subject, levels=c("Single Larvae","L2", "L3", "L4", "L5", "No focal subject "))

ggplot(data= glm_Group, aes(x=Before.After.Stimulus_G, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour: Group (5)" ,subtitle= "Mean Behaviour Occurrence",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After The Halfway Mark", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))
####
ggplot(df_join_G_ALL, aes(x=Before.After, y=mean_B_oc))+
  geom_pointrange(aes(ymin=mean_B_oc - SE,
                      ymax = mean_B_oc + SE))+
  scale_x_discrete(limits=c("B", "A"))+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour: Group (5)" ,subtitle=  "Mean Behaviour Occurrence",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After The Halfway Mark", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))

####
####
#plot not plotting
#Error: Aesthetics must be either length 1 or the same as the data (132): x
####
####

#Muscid attack single
#df_join_mu_Sin
glm_Mus_Sin = glm(Total.number.of.occurences ~ Before.After + Behavior, family=poisson, data = df_join_mu_Sin)
 summary(glm_Mus_Sin)

head(glm_Mus_Sin)
head(df_join_mu_Sin)

Before.After.Stimulusm = factor(df_join_mu_Sin$Before.After, levels=c("B", "A"))

ggplot(data= glm_Mus_Sin, aes(x=Before.After.Stimulusm, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack: Individual Larva" ,subtitle= "Mean Behaviour Occurrence",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After Stimulus", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))
##
print.data.frame(df_join_mu_Sin)
##
 ggplot(data= df_join_mu_Sin, aes(x=Before.After.Stimulusm, y= mean_B_oc))+
  geom_pointrange(aes(ymin=mean_B_oc - SE,
                      ymax = mean_B_oc + SE))+
   scale_x_discrete(limits=c("B", "A"))+
   facet_grid(~Behavior)+
  labs(title="Muscid Attack: Individual Larva" ,subtitle= "Mean Behaviour Occurrence",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement;
  RBT: Retract Breathing Tube; S: Searching; Si: Sink;
  SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After Stimulus", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
   theme(text= element_text( family ='gara'))


#Muscid attack group
#df_join_mu_G
print.data.frame(df_join_mu_G)

glm_Mus_Grou = glm(Total.number.of.occurences ~ Before.After + Behavior, family=poisson, data = df_join_mu_G)

Before.After.StimulusMG = factor(df_join_mu_G$Before.After, levels=c("B", "A"))
#Box&Whisker
ggplot(data= glm_Mus_Grou, aes(x=Before.After.StimulusMG, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack: Group Larvae" ,subtitle= "Mean Behaviour Occurrence",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement;
  RBT: Retract Breathing Tube; S: Searching; Si: Sink;
  SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After Stimulus", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))

 #Point range
ggplot(df_join_mu_G, aes(x=Before.After.StimulusMG, y=mean_B_oc))+
  geom_pointrange(aes(ymin=mean_B_oc - SE, 
                      ymax = mean_B_oc + SE))+
  scale_x_discrete(limits=c("B", "A"))+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack: Group larvae" ,subtitle=  "Mean Behaviour Occurrence",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After Stimulus", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))



#WATER DISTURBANCE CONTROL!!!!!!
#df_join_water

#glm
glm_Water = glm(Total.number.of.occurences ~ Before.After + Behavior, family=poisson, data = df_join_water)
summary(glm_Water)

#Set Before and After the right way around
Before.After.StimulusW = factor(df_join_water$Before.After, levels=c("B", "A"))

#box plot

ggplot(data= glm_Water, aes(x=Before.After.StimulusW, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack: Water Disturbance" ,subtitle= "Control Group",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After Stimulus", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))


#point range
head(df_join_water)

ggplot(data= df_join_water, aes(x=Before.After.StimulusW, y= mean_B_oc))+
  geom_pointrange(aes(ymin=mean_B_oc - SE,
                      ymax = mean_B_oc + SE))+
  scale_x_discrete(limits=c("B", "A"))+
    facet_grid(~Behavior)+
  labs(title="Muscid Attack: Water Disturbance" ,subtitle= "Control Group",caption= "CD : Change Direction; CW: Climb Wall; EBT: Extend Breathing Tube; Fl: Float; LRM: Long Range Movement; RBT: Retract Breathing Tube;
       S: Searching; Si: Sink; SRM: Short Range Movement; StB: Still bottom; StS: Still Surface; W: Writhing
       ", x = "Before & After Stimulus", y= "Behaviour Occurrence (Count / 2 larva minutes)")+
  theme(text= element_text( family ='gara'))

 #
############

###########
###############
############
####################


#use models to compare mean behaviour before and after

#mean_B_oc 
#SE 
#stdDev

#G3
#df_joined_behav_1
tail(df_joined_behav_1)
G3.lm = lm(mean_B_oc ~ Before.After + Before.After*Behavior
  ,data=df_joined_behav_1)
summary(G3.lm)
plot(G3.lm) 

Before.After.StimulusG3 = factor(df_joined_behav_1$Before.After, levels=c("B", "A"))

df_std_1= df_joined_behav_1 %>%
  group_by(Behavior, Before.After, mean_B_oc)%>%
  summarise(stev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)

df_B1_mean = read.csv("C:\\Users\\Nay\\R\\LagoonSurvey\\G3_mean_stDev.csv")


#write.csv(df_std_1, "C:\\users\\Nay\\R\\LagoonSurvey\\G3_mean_stDev.csv")
#pair data
G3Before = subset(df_B1_mean, Before.After=="B", Behavior, drop=TRUE)
G3After = subset(df_joined_behav_1, Before.After=="A", Behavior, drop = TRUE)
#plot paired data
library(PairedData)
#G3pd = paired(G3Before, G3After)
#differing sizes
#plot(G2pd)


#compure difference
G3Diff = with(df_B1_mean,
              mean_B_oc[Before.After == "B"] - mean_B_oc[Before.After=="A"])
#Different lengths...

res = t.test(mean_B_oc ~ Before.After, data = df_B1_mean, paired=TRUE)
#not same length...
res

ggplot(df_std_1, aes(x=Before.After, y = stev))+
  geom_jitter()+
  facet_grid(~Behavior)

par(mfrow=c(2,2))
t.test(G3.lm$mean_B_oc)

plot(G3.lm)


#Single
#df_joined_bSing

#Paired T-Test
#df_new_bSing
head(df_joined_bSing)
df_S_T= df_joined_bSing %>%
  group_by(Behavior, Before.After, mean_B_oc)%>%
  summarise(stev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)

#write.csv(df_S_T, "C:\\Users\\Nay\\R\\LagoonSurvey\\Sin_Ttest.csv")
df_S_T 


resS = t.test(mean_B_oc ~ Before.After, data = df_S_T , paired=TRUE)
resS

#GroupAll
#df_join_G_ALL
df_G_T= df_join_G_ALL %>%
  group_by(Behavior, Before.After, mean_B_oc)%>%
  summarise(stev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)

resG = t.test(mean_B_oc ~ Before.After, data = df_G_T , paired=TRUE)
resG
  
  
 #Muscid Single
#df_join_mu_Sin


df_MS_T= df_join_mu_Sin %>%
  group_by(Behavior, Before.After, mean_B_oc)%>%
  summarise(stev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)


write.csv(df_MS_T, "C:\\Users\\Nay\\R\\LagoonSurvey\\df_MS_T.csv")

read.csv("C:\\Users\\Nay\\R\\LagoonSurvey\\df_MS_T.csv")
resMS = t.test(mean_B_oc ~ Before.After, data = df_MS_T , paired=TRUE)
resMS

###Lost Cause

#Muscid group
#df_join_mu_G
df_join_mu_G

df_MG_T= df_join_mu_G %>%
  group_by(Behavior, Before.After, mean_B_oc)%>%
  summarise(stev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)


write.csv(df_MS_T, "C:\\Users\\Nay\\R\\LagoonSurvey\\df_MG_T.csv")

df_MG_T= read.csv("C:\\Users\\Nay\\R\\LagoonSurvey\\df_MG_T.csv")
resMG = t.test(mean_B_oc ~ Before.After, data = df_MG_T , paired=TRUE)
resMG
#Lost Cause


#Water dist
#df_join_water
df_MW= df_join_water %>%
  group_by(Behavior, Before.After, mean_B_oc)%>%
  summarise(stev = sd(Total.number.of.occurences))%>%
  print.data.frame(.)


write.csv(df_MW, "C:\\Users\\Nay\\R\\LagoonSurvey\\df_MG_T.csv")

read.csv("C:\\Users\\Nay\\R\\LagoonSurvey\\df_MW_T.csv")
resMW = t.test(mean_B_oc ~ Before.After, data = df_MW , paired=TRUE)
resMW



###############
##################
#########################################################


#Make df of all Before results from all dfs- but labelled so know which is which
#need to join df
#Adding Condition element to all df
#G3
head(df_joined_behav_1)
df_b = df_joined_behav_1%>%
  group_by(Before.After)%>%
  summarise(Condition = "G3",
            Total.number.of.occurences = Total.number.of.occurences)
df_bb = left_join(df_joined_behav_1, df_b)
#df_bb[!duplicated(df_bb),]
head(df_bb)

#Single
df_s = df_joined_bSing%>%
  group_by(Behavior, Before.After)%>%
  summarise(Condition = "I")
head(df_s)
df_ss = left_join(df_joined_bSing, df_s)
head(df_ss)
#Group All
##
df_g = df_join_G_ALL%>%
  group_by(Behavior, Before.After)%>%
  summarise(Condition = "G5")
df_gg = left_join(df_join_G_ALL, df_g)


#Muscid Single
df_ms = df_join_mu_Sin%>%
  group_by(Behavior, Before.After)%>%
  summarise(Condition = "MS")
df_mss = left_join(df_join_mu_Sin, df_ms)

#Musid Group
df_ms
df_mg = df_join_mu_G%>%
  group_by(Behavior, Before.After)%>%
  summarise(Condition = "MG")
print.data.frame(df_mg)
df_mgg = full_join(df_join_mu_G, df_mg)
print.data.frame(df_mgg)

#Water Disturbance
df_mw = df_join_water%>%
  group_by(Behavior,  Before.After)%>%
  summarise(Condition = "WD")
df_mww = left_join(df_join_water, df_mw)

#compare mww to GvI I
df_WSS = left_join(df_ss, df_mww)
df_WSS

ggplot(df_WSS, aes(x = factor(Before.After, levels="B", "A"), y = Total.number.of.occurences, fill = Condition))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  stat_summary(fun = mean, geom="point", colour="white", shape=1,  size=0.8)+
  facet_grid(~Behavior)+
  labs(title = "Control Comparison",
       subtitle = " Boxplot to Compare The Control Conditions",
       x = "Behaviour",
       y = "Behaviour Occurrence (Count / 2 larva minutes)",
       caption = "Boxplots displaying the control conditions")+
  theme(text= element_text( family ='gara'))+
  theme(axis.text =element_text(size=7))

###Plot of WD Before & After
ggplot(df_mww, aes(x= factor(df_mww$Before.After, levels=c("B", "A")), y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack",
       subtitle = "Water Disturbance; BEFORE & AFTER Stimulus",
       x = "Behaviour",
       y = "Behaviour Occurrence (Count / 2 larva minutes)",
       caption = "Box & Whisker plot to explore statistical differences in behaviour occurrence before and after stimulus.")+
  theme(text= element_text(family ='gara'))

#Subset Before.After = B (Before)
#G3
df_B1 = df_bb %>%
  subset(Before.After == "B")%>%
  select(Behavior, Before.After, Total.number.of.occurences,Condition)
print.data.frame(df_B1)

##
#Single
df_S = df_ss%>%
  select_all()%>%
  subset(Before.After == "B")%>%
  select(Behavior, Before.After, Total.number.of.occurences,  Condition)
head(df_S)

#Group
df_G = df_gg %>%
  subset(Before.After == "B")%>%
  select(Behavior, Before.After, Total.number.of.occurences, Condition)
head(df_G)


##
#Muscid Single
df_MS = df_mss%>%
  subset(Before.After == "B")%>%
  select(Behavior, Before.After, Total.number.of.occurences, Condition)
head(df_MS)

df_mgg
#Muscid Group
df_MG = df_mgg %>%
  subset(Before.After == "B")%>%
  select(Behavior, Before.After, Total.number.of.occurences, Condition)
head(df_MG)
print.data.frame(df_MG)
##
#Water Disturbance
df_M_W = df_mww %>%
  subset(Before.After == "B")%>%
  select(Behavior, Before.After, Total.number.of.occurences,  Condition)
df_M_W


#Join all Before df's together
 df_B = full_join(df_B1, df_S, all.x=TRUE)
head(df_B)
 #not including df_S
 df_Be = full_join(df_B, df_G, all.x=TRUE)
 tail(df_Be)
 
 df_Bef = full_join(df_Be, df_MS, all.x=TRUE)
 df_Befo = full_join(df_Bef, df_MG, all.x=TRUE)
 df_Before = full_join(df_Befo, df_M_W, all.x=TRUE)

 print.data.frame(df_Before)
tail(df_Before)


#YUSSSS!!!!


#Make df of all After results from all dfs- but labeled so know which is which
#G3
df_B2 = df_bb %>%
  subset(Before.After == "A")%>%
  select(Behavior, Before.After, Total.number.of.occurences,  Condition)
print.data.frame(df_B2)

##
#Single
df_z = df_ss%>%
  select_all()%>%
  subset(Before.After == "A")%>%
  select(Behavior, Before.After, Total.number.of.occurences, Condition)
head(df_z)

#Group
df_H = df_gg %>%
  subset(Before.After == "A")%>%
  select(Behavior, Before.After, Total.number.of.occurences,  Condition)
head(df_H)


##
#Muscid Single
df_SM = df_mss%>%
  subset(Before.After == "A")%>%
  select(Behavior, Before.After, Total.number.of.occurences, mean_B_oc, SE, stdDev, Condition)
head(df_SM)

##
#Muscid Group
print.data.frame(df_mgg)
df_GM = df_mgg %>%
  select(Behavior, Before.After, Total.number.of.occurences,Condition)%>%
  subset(Before.After == "A")
  

print.data.frame(df_GM)

##
#Water Disturbance
df_WM = df_mww %>%
  subset(Before.After == "A")%>%
  select(Behavior, Before.After, Total.number.of.occurences, Condition)
df_WM



#Join all Before df's together
df_A = full_join(df_B2, df_z, all.x=TRUE)
head(df_A)
#not including df_S
df_Af = full_join(df_A, df_H, all.x=TRUE)
tail(df_Af)

df_Aft = full_join(df_Af, df_SM, all.x=TRUE)
df_Afte = full_join(df_Aft, df_GM, all.x=TRUE)
df_After = full_join(df_Afte, df_WM, all.x=TRUE)
head(df_After)





ggplot(df_After, aes(x= factor(Condition, levels =c("I", "WD","MS", "G3", "G5", "MG")), y = Total.number.of.occurences, fill=Condition))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  stat_summary(fun = mean, geom="point", colour="white", shape=1,  size=0.8)+
  facet_grid(~Behavior)+
  labs(title = "After",
       subtitle = " Plot of all AFTER Results",
       x = "Condition",
       y = "Behaviour Occurrence (Count / 2 larva minutes)",
       caption = "Boxplots displaying all AFTER grouped results for comparison")+
  theme(text= element_text( family ='gara'))+
  theme(axis.text =element_text(size=4))
#############
###################################
#Before GrouoVIndividal

df_gvi_B = full_join(df_B1, df_S)
df_GvI_B = full_join(df_gvi_B, df_G)

############

#After
df_gvi_A = full_join(df_B2, df_z) 
df_GvI_A = full_join(df_gvi_A, df_H)

#Before and After togeth
df_ba = full_join(df_GvI_B, df_GvI_A)%>%
  group_by(Behavior)
print.data.frame(df_ba)

#BOX PLOT TIME

#Plot df_Before. x = condiition, y= mean_b_oc. facet by behaviour

#Need Control on the left, then G3, then G
ConditionType = factor(df_GvI_B$Condition, levels=c("I", "G3", "G5"))
levels(ConditionType)
class(ConditionType)
  #G3 is a group of three individuals; G is all groups of sizes, 2&5
showtext_auto()

ggplot(df_GvI_B, aes(x= ConditionType, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  #geom_jitter(alpha=0.2, colour="orange", shape = 7)+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour",
       subtitle = "Comparison of the means and standard deviations of behaviours exhibited in three different conditions BEFORE Stimulus",
       x = " Condition",
       y = "Behaviour Occurrence (Count / 2 larva minutes)",
       caption = "Box & Whisker plot to explore statistical differences in behaviour occurrence in three condition types:
       I = Individual larva; G3 = Group of three larvae; G = Group of average 5 larvae")+
  theme(text= element_text( family ='gara'))
                             
  
    ####
#After
ConditionTYpe = factor(df_GvI_A$Condition, levels=c("I", "G3", "G5"))
levels(ConditionType)

class(ConditionTYpe)
print.data.frame(df_GvI_A)
#
ggplot(df_GvI_A, aes(x= ConditionTYpe, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  #geom_jitter(alpha = 0.2, colour = "orange", size = 0.2, shape = 4)+
  facet_grid(~Behavior)+
  labs(title="Group Vs Individual Behaviour",
       subtitle = "Comparison of the means and standard deviations of behaviours exhibited in three different conditions AFTER Stimulus",
       x = " Condition",
       y = "Behaviour occurrence (Count / 2 larva minutes)",
       caption = "Box & Whisker plot to explore statistical differences in behaviour occurrence in three condition types:
       I = Individual larva; G3 = Group of three larvae; G = Group of average 5 larvae")+
  theme(text= element_text( family ='gara'))+
  theme(axis.text = element_text(size=6))
#1/2count larva-1 minutes-1

 #point_range
#ggplot(df_GvI_A, aes(x= ConditionTYpe, y= Total.number.of.occurences))+
#  geom_pointrange(aes(ymin=mean_B_oc - SE,
#                      ymax = mean_B_oc + SE))+
#  facet_grid(~Behavior)+
#  labs(title="Group Vs Indivudal Behaviour",
#       subtitle = "Comparison of the means and standard deviations of behaviours exhibited in three different conditions AFTER Stimulus",#
 #      x = " Condition",
#       y = "Behaviour occurence",
#       caption = "Box & Whisker plot to explore statistical differences in #behaviour occurance in three condition types:
 #      Si = Single larva; G3 = Group of three larae; G = larger group of size 5, 10 & 15")

###########

#Before and After all conditions boxplot  
#
#tail(df_ba)
#(df_ba$Total.number.of.occurences)
#CONDITionTYpe = factor(df_ba$Condition, levels = c("Si", "G3", "G"))

#ggplot(df_ba, aes(x= CONDITionTYpe, y= Total.number.of.occurences))+
#  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  #scale_x_discrete(limits="B", "A")+
#  facet_grid(~Behavior)+
#  labs(title="Group Vs Indivudal Behaviour",
#       subtitle = "Comparison of the means and standard deviations of behaviours exhibited in three different conditions BEFORE & AFTER Stimulus",
#       x = " Condition",
#       y = "Behaviour occurence (Count/ 2 larva-minutes)",
#       caption = "Box & Whisker plot to explore statistical differences in behaviour occurance in three condition types:
#       Si = Single larva; G3 = Group of three larae; G = Group of 5 larvae")


##########

#######MUSCID

df_m_a = full_join(df_SM, df_GM)
print.data.frame(df_m_a)
#wrong letters
df_WM
df_M_A = full_join(df_m_a, df_WM)
print.data.frame(df_M_A)

 ##############
df_m_b = full_join(df_MS, df_MG)
df_M_B = full_join(df_m_b, df_M_W)
##################
df_M_BA = full_join(df_M_B, df_M_A)
###

#Boxplotz
#Before

#Set levels
CONditiontype = factor(df_M_A$Condition, levels = c("WD", "MS", "MG"))
print(df_M_B$Condition)
print(CONditiontype)
#plotting
ggplot(df_M_A, aes(x= CONditiontype, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  #geom_jitter(alpha = 0.2, colour = "orange", shape = 7)+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack",
       subtitle = "Comparison of the means and standard deviations of behaviours exhibited in three different conditions AFTER Stimulus",
       x = " Condition", 
       y = "Behaviour occurrence (Count / 2 larva minutes)",
       caption = "Box & Whisker plot to explore statistical differences in behaviour occurrence in three condition types:
       WD = Water Disturbance; MS = Muscid & Single Larva; MG = Muscid & Group (3) of larvae")+
  theme(text= element_text(family="gara"))+
  theme(axis.text = element_text(size=6))


 #######BEFORE
#
df_M_A
CoNdItIoNtYpE = factor(df_M_B$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_B, aes(x= CoNdItIoNtYpE, y= Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
 # geom_jitter(alpha= 0.2, colour = "orange", shape = 7)+
  facet_grid(~Behavior)+
  labs(title="Muscid Attack",
       subtitle = "Comparison of the means and standard deviations of behaviours exhibited in three different conditions BEFORE Stimulus",
       x = " Condition", 
       y = "Behaviour occurrence (Count / 2 larva minutes)",
       caption = "Box & Whisker plot to explore statistical differences in behaviour occurrence in three condition types:
       WD = Water Disturbance: Control; MS = Muscid and Single Larva; MG = Muscid & Group (3) of larvae")+ 
  theme(text = element_text(family="gara"))+
  theme(axis.text = element_text(size=6, family ='gara'))

  ####
#########################################
#########################################################
#######################################
#####
#CW
COUNditionTipe = factor(df_M_CW$Condition, levels = c("WD", "MS", "MG"))
df_M_CW$Condition
df_M_BA
df_M_CW
Boufor = factor(df_M_CW$Before.After, levels = c("B", "A"))
df_M_CW = df_M_BA %>%
  subset(df_M_BA$Behavior == "CW")

ggplot(df_M_CW, aes(x= COUNditionTipe, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_CW$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Climb Wall; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
#####
#CD
df_M_CD = df_M_BA %>%
  subset(df_M_BA$Behavior == "CD")
BCD = factor(df_M_CD$Before.After, levels = c("B", "A"))
CoNdItIoNCD = factor(df_M_CD$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_CD, aes(x= CoNdItIoNCD, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_CD$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Change Direction; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
##########
#EBT
df_M_EBT = df_M_BA %>%
  subset(df_M_BA$Behavior == "EBT")
BBT = factor(df_M_EBT$Before.After, levels = c("B", "A"))
CoNdItIoNEBT = factor(df_M_EBT$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_EBT, aes(x= CoNdItIoNEBT, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_EBT$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Extend Breathing Tube; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
####
#Fl
df_M_Fl = df_M_BA %>%
  subset(df_M_BA$Behavior == "Fl")
BFl = factor(df_M_Fl$Before.After, levels = c("B", "A"))
CoNdItIoNFl = factor(df_M_Fl$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_Fl, aes(x= CoNdItIoNFl, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_Fl$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Float; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
###
#LRM
df_M_LRM = df_M_BA %>%
  subset(df_M_BA$Behavior == "LRM")
BLRM = factor(df_M_LRM$Before.After, levels = c("B", "A"))
CoNdItIoNLRM = factor(df_M_LRM$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_LRM, aes(x= CoNdItIoNLRM, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_LRM$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Long Range Movement; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
###
#RBT
df_M_RBT = df_M_BA %>%
  subset(df_M_BA$Behavior == "RBT")
BRBT = factor(df_M_RBT$Before.After, levels = c("B", "A"))
CoNdItIoNRBT = factor(df_M_RBT$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_RBT, aes(x= CoNdItIoNRBT, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_RBT$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Retract Breathing Tube; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
####
# S
df_M_Ss = df_M_BA %>%
  subset(df_M_BA$Behavior == "S")
BSs= factor(df_M_Ss$Before.After, levels = c("B", "A"))
CoNdItIoNSs = factor(df_M_Ss$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_Ss, aes(x= CoNdItIoNSs, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_Ss$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Searching; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
#####
#Si
df_M_Si = df_M_BA %>%
  subset(df_M_BA$Behavior == "Si")
BSi= factor(df_M_Si$Before.After, levels = c("B", "A"))
CoNdItIoNSi = factor(df_M_Si$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_Si, aes(x= CoNdItIoNSi, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_Si$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Sink; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
######
#SRM
df_M_SRM = df_M_BA %>%
  subset(df_M_BA$Behavior == "SRM")
BSRM = factor(df_M_SRM$Before.After, levels = c("B", "A"))
CoNdItIoNSRM = factor(df_M_SRM$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_SRM, aes(x= CoNdItIoNSRM, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_SRM$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Short Range Movement; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
######
#StB
df_M_STB = df_M_BA %>%
  subset(df_M_BA$Behavior == "StB")
BSTB = factor(df_M_STB$Before.After, levels = c("B", "A"))
CoNdItIoNSTB = factor(df_M_STB$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_STB, aes(x= CoNdItIoNSTB, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_STB$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Still at Bottom third of lagoon; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
##################
#StS
df_M_STS = df_M_BA %>%
  subset(df_M_BA$Behavior == "StS")
BSTS = factor(df_M_STS$Before.After, levels = c("B", "A"))
CoNdItIoNSTS = factor(df_M_STS$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_STS, aes(x= CoNdItIoNSTS, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_STS$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Still at Surface third of lagoon; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))
#######
#W
df_M_W = df_M_BA %>%
  subset(df_M_BA$Behavior == "W")
BW= factor(df_M_W$Before.After, levels = c("B", "A"))
CoNdItIoNW = factor(df_M_W$Condition, levels = c("WD", "MS", "MG"))

ggplot(df_M_W, aes(x= CoNdItIoNW, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_M_W$Before.After, levels = c("B", "A")))+
  labs(title= "Muscid Attack", 
       subtitle = "Writhing; Before and After Stimulus",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: WD; MS; MG, Before and After Stimulus")+
  theme( text = element_text(family = "gara"))

##############
######################################################
#####################################################
#########################################################################
########################################################################
####################################################
#####################################################
################### df_ba
#CW
df_GVICW = df_ba %>%
  subset(df_ba$Behavior == "CW")
COUNditionnCW = factor(df_GVICW$Condition, levels = c("I", "G3", "G5"))
BCW = factor(df_GVICW$Before.After, levels = c("B", "A"))


ggplot(df_GVICW, aes(x= COUNditionnCW, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVICW$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Climb Wall",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
#####
#CD
df_GVICD = df_ba %>%
  subset(df_ba$Behavior == "CD")
COUNditionnCD = factor(df_GVICD$Condition, levels = c("I", "G3", "G5"))
BCD = factor(df_GVICW$Before.After, levels = c("B", "A"))

ggplot(df_GVICD, aes(x= COUNditionnCD, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVICD$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Change Direction",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#EBT
df_GVIEBT = df_ba %>%
  subset(df_ba$Behavior == "EBT")
COUNditionnEBT = factor(df_GVIEBT$Condition, levels = c("I", "G3", "G5"))
BEBT = factor(df_GVIEBT$Before.After, levels = c("B", "A"))

ggplot(df_GVIEBT, aes(x= COUNditionnEBT, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVIEBT$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Extend Breathing Tube",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#Fl
df_GVIFL = df_ba %>%
  subset(df_ba$Behavior == "Fl")
COUNditionnFL = factor(df_GVIFL$Condition, levels = c("I", "G3", "G5"))
BFL = factor(df_GVIFL$Before.After, levels = c("B", "A"))

ggplot(df_GVIFL, aes(x= COUNditionnFL, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVIFL$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Float",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))

###############
#LRM
df_GVILRM = df_ba %>%
  subset(df_ba$Behavior == "LRM")
COUNditionnLRM = factor(df_GVILRM$Condition, levels = c("I", "G3", "G5"))
BLRM = factor(df_GVILRM$Before.After, levels = c("B", "A"))

ggplot(df_GVILRM, aes(x= COUNditionnLRM, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVILRM$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Long Range Movement",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#RBT
df_GVIRBT = df_ba %>%
  subset(df_ba$Behavior == "RBT")
COUNditionnRBT = factor(df_GVIRBT$Condition, levels = c("I", "G3", "G5"))
BRBT = factor(df_GVIRBT$Before.After, levels = c("B", "A"))

ggplot(df_GVIRBT, aes(x= COUNditionnRBT, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVIRBT$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Retracting Breathing Tube",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#S
df_GVIS = df_ba %>%
  subset(df_ba$Behavior == "S")
COUNditionnS = factor(df_GVIS$Condition, levels = c("I", "G3", "G5"))
BS = factor(df_GVIS$Before.After, levels = c("B", "A"))

ggplot(df_GVIS, aes(x= COUNditionnS, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVIS$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Searching",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))

###############
#Si
df_GVISi = df_ba %>%
  subset(df_ba$Behavior == "Si")
COUNditionnSi = factor(df_GVISi$Condition, levels = c("I", "G3", "G5"))
BSi = factor(df_GVISi$Before.After, levels = c("B", "A"))

ggplot(df_GVISi, aes(x= COUNditionnSi, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVISi$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Sinke",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#SRM
df_GVISRM = df_ba %>%
  subset(df_ba$Behavior == "SRM")
COUNditionnSRM = factor(df_GVISRM$Condition, levels = c("I", "G3", "G5"))
BSRM = factor(df_GVISRM$Before.After, levels = c("B", "A"))

ggplot(df_GVISRM, aes(x= COUNditionnSRM, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVISRM$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Short Range Movement",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#StB
df_GVISTB = df_ba %>%
  subset(df_ba$Behavior == "StB")
COUNditionnSTB = factor(df_GVISTB$Condition, levels = c("I", "G3", "G5"))
BSTB = factor(df_GVISTB$Before.After, levels = c("B", "A"))

ggplot(df_GVISTB, aes(x= COUNditionnSTB, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVISTB$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Still at Bottom Third",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
###############
#StS
df_GVISTS = df_ba %>%
  subset(df_ba$Behavior == "StS")
COUNditionnSTS = factor(df_GVISTS$Condition, levels = c("I", "G3", "G5"))
BSTS = factor(df_GVISTS$Before.After, levels = c("B", "A"))

ggplot(df_GVISTS, aes(x= COUNditionnSTS, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVISTS$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Still at Surface Third",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))

###############
#W
df_GVIW = df_ba %>%
  subset(df_ba$Behavior == "W")
COUNditionnW = factor(df_GVIW$Condition, levels = c("I", "G3", "G5"))
BW = factor(df_GVIW$Before.After, levels = c("B", "A"))

ggplot(df_GVIW, aes(x= COUNditionnW, y = Total.number.of.occurences))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  facet_grid(~ factor(df_GVIW$Before.After, levels = c("B", "A")))+
  labs(title= "Group Vs Individual Behaviour", 
       subtitle = "Writhing",
       x = "Coundition",
       y ="Number of Occurrences (Count / 2 larva minutes)",
       caption = "Box & Whisker plot displaying the mean, range and standard deviation for three codnition types: I; G3; G5, Before & After")+
  theme( text = element_text(family = "gara"))
