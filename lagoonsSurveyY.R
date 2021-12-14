library(tidyverse)
library(ggplot2)
library(showtext)

font_add_google("EB Garamond",family = "gara")
showtext_auto()
rm(list=ls())

Lagoon = read.csv("C:\\Users\\Nay\\R\\LagoonSurvey\\LagoonSurvey.csv", na.strings = c("", "NA"))
lagoon = Lagoon[c(1:145), c(1:15)];
names(lagoon)
summary(lagoon)
head(lagoon)
tail(lagoon)
str(lagoon) #type of variable
#naa = is.na(Lagoon)
#naa

#lagoon = na.omit(Lagoon)
lagoon
#change data to correct data type
lagoon$Species.ID = as.factor(lagoon$Species.ID)
lagoon$Ethno.Code = as.factor(lagoon$Ethno.Code)
lagoon$Wet..Dry = as.factor(lagoon$Wet..Dry)
lagoon$Weather = as.factor(lagoon$Weather)
lagoon$Notes = as.factor(lagoon$Notes)
lagoon$Lagoon.ID = as.factor(lagoon$Lagoon.ID)
lagoon$Time..24h. = as.numeric(lagoon$Time..24h.)
lagoon$PH = as.numeric(lagoon$PH)
lagoon$Temperature...C.= as.numeric(lagoon$Temperature...C.)

#Species richness = MNS
#group by lagoonID
lagoon[lagoon$Lagoon.ID &lagoon$MNS]
  
#diagnostic plots
plot(lagoon$MNS~lagoon$PH)

ggplot(lagoon, aes(Date, MNI))+geom_jitter()+geom_smooth(method=lm)

#trying to group time of day into morning or afternoon
#plot mns~time with noon and morn times different colours
lagoon%%
    Day = if (lagoon$Time..24h. <= 12.00 & lagoon$Time..24h.>= 0.00){
      c('morn')
    } else{
      c('eve')
    } 
    
#ggplot(lagoon, aes(Lagoon.ID, MNS, color = Day)) +
 # geom_line()+
  #labs(title = "colourd time of day test",
  #     x = 'Lagoon',
   #    y = 'Species Richness'

#lagoon%%Day = function(Time..24h.){
#    for (t in lagoon$Time..24h.){
#      if (lagoon$Time..24h. <= "12.00" & lagoon$Time..24h.>= "0.00"){
#        result = c('morn')
 #     } else{
#        result = c('eve')
#    }
#      return(result)
#    }
#  }

#Day
#following R course
demo(lm.gm)

par()
rm(list = ls())
PAROLD = par()
par(mfrow = c (2,2))

hist(Lagoon$MNS)
hist(Lagoon$MNI)
hist(lagoon$Time..24h.)
hist(lagoon$Temperature...C.)

par(mfrow = c(1,1))
par(PAROLD)

hist(Lagoon$MNS, breaks=15)
#Remember purpose of experiement
#whether factor affects species richness
#lagoon composition
#lagoon location type
#temperature
#ph
plot(MNS~Lagoon.ID, data=lagoon)
plot(MNS~Temperature...C., data=lagoon)
plot(MNS~PH, data=lagoon)
#plotting effects of more than one predictor. adding colour
plot(MNS~Lagoon.ID, col=as.numeric(PH), data=Lagoon)

plot(MNS~Lagoon.ID, col=as.numeric(Temperature...C.), data=Lagoon)

plot(MNI~Lagoon.ID, col=as.numeric(Temperature...C.), data=Lagoon)
##

##ggplot
ggplot(Lagoon, aes(x=PH))+geom_histogram()

lagooon = data.frame(lagoon)
lagooon <- data.frame(matrix(unlist(lagoon), nrow=145, byrow=TRUE),stringsAsFactors=FALSE)

typeof(lagooon)

lagoon$Temperature...C. = as.numeric(lagoon$Temperature...C.)


  #filter(lagoon$Temperature...C. > 0 & lagoon$PH > 0)
  
#Na.omit(mutate_all(lagoon, ~ifelse(.%in% c("N/A", "null", "") NA,.)))
#new_df = na.omit(lagoon)
 
#ggplot(new_df, aes(x=PH))+ geom_histogram(binwidth = 0.07)
#new_df shows very different data to lagoon
ggplot(lagoon, aes(x=PH))+ geom_histogram(binwidth = 0.07)

#df_lgn = lagoon%%
#  filter(lagoon$Temperature...C. < 5000)
#typeof(lagoon$Temperature...C.)

#########################
#####################
type = factor(lagoon$Location.type, levels = c("Hedge", "Open", "Shaded"))
ggplot(lagoon, aes(x=Temperature...C., y=MNS))+
  geom_jitter(position= position_jitter(width=1), alpha = 0.4, aes(colour = factor(type)))+
  geom_smooth(method=lm)+
  labs(title ="Lagoon Survey",
       subtitle = "The Effect of Temperature of Species Richness",
       x = "Temperature (°C)",
       y = "Species Richness",
       caption = "Jitter plot with line of best fit displaying the effect of temperature on Species richness.
       colour displays the effect location plays on Species Richness
       ",
       factor = "Location Type"
  )+
  theme()
 #################
#################
#PH slight LBF, Temp more so

#playing with other variables

ggplot(lagoon, aes(x= Weather, y=MNS))+ geom_boxplot()+geom_smooth(method=lm)

ggplot(lagoon, aes(x=Species.ID, y=MNI))+geom_boxplot()

head(lagoon)
ggplot(lagoon, aes(x=MNI, Y=MNS))+facet_grid(lagoon$Weather)

typeof(lagoon$Lagoon.ID)
###############
#Faceting
##########
#facet_grid
#facet_wrap


ggplot(lagoon, aes(x=Temperature...C., y=MNS))+ geom_jitter(position= position_jitter(width=1), alpha = 0.4, aes(colour = factor(Lagoon.ID)))+stat_smooth(method=lm)+facet_grid(.~Lagoon.ID)

ggplot(lagoon, aes(x=Temperature...C., y=MNI))+ geom_jitter(position= position_jitter(width=1), alpha = 0.4, aes(colour = factor(Lagoon.ID)))+stat_smooth(method=lm)+facet_grid(.~Lagoon.ID)

#Two Facet
ggplot(lagoon, aes(x=Temperature...C., y=MNS))+
  geom_jitter(position= position_jitter(width=1),
              alpha = 0.4)+
              stat_smooth(method="lm")+
                facet_grid(Weather ~ Lagoon.ID, as.table = FALSE)
###useful if using the right variables


#3.5 grouped data and violins
dev.off()
ggplot(lagoon, aes(x=Morn.Noon, y=MNS))+
  geom_boxplot()+
  scale_x_discrete(limits=c("Morn", "Noon"))

ggplot(lagoon, aes(x=Morn.Noon, y=MNS))+
  geom_jitter(width=1, alpha=0.4)

std_err=function(x){
  SE = sd(x)/sqrt(length(x))
  return(SE)
}
lagoon_sum = lagoon%%
  group_by(Morn.Noon)%%
  summarise(MNS_mean = mean(MNS),
            MNS_SE = std_err(MNS))

typeof(lagoon$Morn.Noon)

#Linear Models
temp.mod = lm(MNS ~ Temperature...C., data=lagoon)
temp.i.mod = lm(MNI ~ Temperature...C., data =lagoon)
weat.i.mod = lm(MNI~Weather, data=lagoon)
weat.s.mod = lm(MNS~Weather, data=lagoon)
ph.i.mod = lm(MNI~PH, data=lagoon)
ph.s.mod = lm(MNS~PH, data=lagoon)
day.i.mod = lm(MNI~Morn.Noon, data=lagoon)
day.s.mod = lm(MNS~Morn.Noon, data=lagoon)
comp.i.mod = lm(MNI~Lag.composition, data=lagoon)
comp.s.mod= lm(MNS~Lag.composition, data=lagoon)
loc.i.mod = lm(MNI~Location.type, data=lagoon)
loc.s.mod= lm(MNS~Location.type, data=lagoon)

#diagnostic plots
par(mfrow=c(2,2))
plot(temp.mod)
plot(temp.i.mod)
plot(weat.i.mod)
plot(weat.s.mod)
plot(ph.i.mod)
plot(ph.s.mod)
plot(comp.i.mod)
plot(comp.s.mod)
plot(day.i.mod)
plot(day.s.mod)

#Table of Coefficients Include in paper
summary(temp.mod)
summary(temp.i.mod)
summary(weat.i.mod)
summary(weat.s.mod)
summary(ph.i.mod)
summary(ph.s.mod)
summary(day.i.mod)
summary(day.s.mod)
summary(comp.i.mod)
summary(comp.s.mod)
summary(loc.i.mod)
summary(loc.s.mod)
#Anova table. don't include in paper
summary.aov(temp.mod)

install.packages('car')
library('car')
#mulicollinearity 
#vif()
#multi variable lm
#MNS   Species richness

#replacing Sh and Su into SS to make them one variable
lagoon$Weather = gsub('Sp', 'SS', lagoon$Weather)
lagoon$Weather = gsub('Su', 'SS', lagoon$Weather)
#only rain is significant so..
#change weather to rain or sun
lagoon$Weather = gsub('Cl', 'SS', lagoon$Weather)
lagoon$Weather = gsub('Sh', 'SS', lagoon$Weather)
#remove NA values
drop_na(lagoon$Weather)
head(lagoon)
lagoon = na.omit(lagoon)
levels(Lagoon$Weather)
ggplot(lagoon, aes(x=MNI, y=MNS))+ scale_x_discrete(limits=c("R", "SS"))

#multi.s.mod= lm(MNS~Temperature...C.+Weather+PH+Morn.Noon+Lag.composition+Location.type, data=lagoon)
#vif(multi.s.mod)
#summary(multi.s.mod)
#logLik(multi.s.mod) #-253.0934 df=13
#remove factors that plots imply do not effect MNS
#mod.2= update(multi.s.mod, ~. -Location.type) #MNS~Temperature...C.+Weather+PH+Morn.Noon+Lag.composition
#logLik(mod.2) #-253.5963 df=11
#NOT AN ANOVA. THis is using anova function. give two models will give partial f-test
#anova(multi.s.mod, mod.2)


#mod.3 = update(mod.2, ~. -Lag.composition) #MNS~Temperature...C.+Weather+PH+Morn.Noon
#anova(mod.2, mod.3)
#logLik(mod.3) # -255.4428 df=8

#AIC(multi.s.mod, mod.2, mod.3)
#mod.3 is best

#mod.4 = update(mod.3, ~. -Morn.Noon) #MNS~Temperature...C.+Weather+PH
#anova(mod.3, mod.4)
#logLik(mod.4) #-256.2916
#summary(mod.4)
#AIC(mod.3, mod.4)
#mod.4 slightly better = 526.5831

#mod.3.2 = update(mod.2, ~. -Morn.Noon)
#anova(mod.2, mod.3.2)
#mod.5= update(mod.4, ~. -PH)
#anova(mod.4) #MNS~Temperature...C.+Weather
#anova(mod.4, mod.5)
#biggest jump in F value. should keep PH

#logLik(mod.5) #-260.0097 df=6
#mod.6 = update(mod.5, ~. - Weather) #MNS~Temperature...C.
#anova(mod.5, mod.6)
#logLik(mod.6) # -289.9825 df=3

#mod.7 = update(mod.4, ~. - Weather) #MNS~Temperature...C.+PH
#anova(mod.7, mod.6)
#AIC(mod.7, mod.6)
#mod.7 better = 583.6742

#logLik(mod.7) # -287.8371 df=4
#summary(mod.7)
#summary(mod.6)
###gotten messy
#mod.4, 
#mod.9 = update(mod.2, ~. -Morn.Noon) #MNS~Temperature...C.+Weather+PH+Lag#.composition
#logLik(mod.9) #-254.1971 df=10
#mod.10 =update(mod.9, ~. -Weather) #MNS~Temperature...C.+PH+Lag.composition
#anova(mod.9, mod.4)
#logLik(mod.10) #-283.9129 df=7

#mod.11 = lm(MNS ~ Temperature...C. +  PH, data=lagoon) 
#summary(mod.11)
#anova(mod.11, mod.4)
#plot(mod.11)
#logLik(mod.11) #-283.0721 df=5
#which model fits data best

#mod.4.1 = update(mod.4, ~. + Temperature...C.*PH)
#anova(mod.4, mod.4.1)
#AIC(mod.4, mod.4.1)
#mod.4 better = 526.5831

#mod.4.2 = update(mod.4, ~. + Temperature...C.*Weather)
#anova(mod.4, mod.4.2)
#logLik(mod.4.2) #-254.1891 (df=10)
#logLik(mod.4.1) #-255.7141 (df=8)
#AIC(mod.4, mod.4.2)
#mod.4 better

#Compare models

#AIC(mod.2, mod.3, mod.4, mod.4.1, mod.4.2, mod.5)
#3 & 4 are best
#AIC(mod.3, mod.4, mod.4.1, mod.4.2)
#3 & 4 are best
#So far, mod.4 is best. (MNS~Temperature...C.+Weather+PH)

#mod.a = update(mod.4, ~. -PH)
#AIC(mod.4, mod.a)
#mod.4 best
#mod.b = update(mod.4, ~. - Temperature...C.)
#AIC(mod.4, mod.b)
#mod.4 best
#Cant remove weather, not same # of observations (difference of one)
#mod.c = update(mod.4, ~. -Weather)
#AICc(mod.4, mod.b)

#mod.444 = lm(MNS~Temperature...C. + Weather + PH, data=lagoon, na.action= na.exclude)
#mod.noW = update(mod.444, ~. - Weather, na.action = na.exclude)
#AIC(mod.444, mod.noW)
#####################
#Count data. need to use gml and poisson
#hist(lagoon$MNS)
#plot(MNS~MNI, data =lagoon, pch=as.numeric(PH))

glm.1 =glm(MNS ~MNI +Temperature...C. + PH + MNI*Temperature...C. + MNI*PH, family=poisson, data=lagoon)
summary(glm.1)

#So, PH does not seem significant in glm
#as before make big model and reduce down
#big model:
big.glm = glm(MNS ~ MNI + Temperature...C.+ PH+ Weather+ Lag.composition + Location.type + Morn.Noon, family =poisson, data=lagoon)
summary(big.glm)
bi.glm = update(big.glm, ~. - Location.type) #MNS~ MNI +Temp + PH+Lag.comp
summary(bi.glm)
b.glm = update(bi.glm, ~. - Lag.composition)#MNS~ MNI+ Temp + PH
summary(b.glm)
AIC(bi.glm, b.glm)
c.glm= update(b.glm, ~. - Temperature...C.) #MNS~ MNI +PH
summary(c.glm)
 
ggplot(b.glm, aes(x=Temperature...C., y=MNS, col=Weather))+geom_jitter()


#################################
#THIS IS THE SMALLEST MODEL
d.glm= update(c.glm, ~. - PH) #MNS ~ MNI +weather + morn.noon
summary(d.glm)
anova(d.glm)
anova(d.glm, test="Chi")
###############################


#ggplot(lagoon, aes(x=Morn.Noon, y=MNS))+
#geom_boxplot()+
#  scale_x_discrete(limits=c("Morn", "Noon"))
#geom_jitter(position= position_jitter(width=1), alpha = 0.4, aes(colour = factor(Location.type)

ggplot(d.glm, aes(x=Morn.Noon, y=MNS))+
  geom_jitter(position=position_jitter(width=1), alpha= 0.4, aes(colour=factor(Weather)))+
  scale_x_discrete(limits=c("Morn", "Noon"))

dev.off()
ggplot(d.glm, aes(x=MNI, y=MNS))+
  geom_jitter(position=position_jitter(width=1), alpha=0.4, aes(colour=Weather))+
  facet_grid(.~Morn.Noon)+
  geom_smooth()

#Remove Morn.Noon
small_glm = update(d.glm, ~. - Morn.Noon)
summary(small_glm)
par(mfrow=c(2,2))
plot(small_glm)
head(small_glm)
ggplot(lagoon, aes(x=MNI, y= MNS))

dev.off()

#e.glm = update(b.glm, ~. -PH) # MNS ~ MNI +Temp
#summary(e.glm)
ff.glm = glm(MNS ~ MNI + Weather + Morn.Noon, family =poisson, data=lagoon)
summary(ff.glm)

jj.glm=glm(MNS~Date +MNI +Weather + Morn.Noon, family=poisson, data= lagoon)

jg.glm=lmer(MNS~Date +MNI +(1|Weather) + (1|Morn.Noon), family=poisson, data= lagoon)

ggplot(jj.glm, aes(x=Date, y=MNI, col=MNS))+geom_jitter(alpha=0.4)

anova(ff.glm, test="Chi")
par(mfrow = c (2,2))
plot(ff.glm)

ggplot(ff.glm, aes(x=Morn.Noon, y=MNS))+geom_boxplot()

ggplot(ff.glm, aes(x=Weather, y=MNS))+geom_boxplot()

ggplot(ff.glm , aes(x=MNI, y=MNS, col=Weather))+geom_jitter() +geom_smooth()




#Plot Species Richness against number of individuals. split by morn.noon. again split by weather

tail(lagoon)
#Box and whisker
ggplot(d.glm, aes(x=MNI, y=MNS))+
  geom_boxplot()+
  facet_grid(~Morn.Noon)+
  labs(title = "Lagoon Survey", 
       subtitle = "Species Richness Against Number of Individuals",
       x= "Number of Individuals",
       y = "Species Richness",
       caption = "Box&Whisker plot showing the effect that periods of the day has on Species Richness")

#geom point_range
#need a mean and a stderr
# STD ERR FUNCTION
std_err <- function(x) {
  SE <- sd(x)/sqrt(length(x))
  return (SE)
}
head(lagoon)
df_test= lagoon%>%
  select(Morn.Noon, MNS)%>%
  group_by(Morn.Noon)%>%
  summarise(meanRich= mean(MNS)
        )%>%
  print.data.frame(.)

morn.noon = factor(lagoon$Morn.Noon, levels=c("M", "N"))

df_lagoon = lagoon%>%
  select(Morn.Noon, MNS, MNI)%>%
  group_by(Morn.Noon)%>%
  summarise(meanRich = mean(MNS),
          StdErRich = std_err(MNS),
          stdDevRich = sd(MNS),
          meanInd = mean(MNI),
          stdErInd = std_err(MNI),
          stdDevInd = sd(MNI))%>%
  print.data.frame(.)
df_laugi = left_join(lagoon, df_lagoon)%>%
  na.omit(Morn.Noon)%>%
  select(MNI, MNS, Morn.Noon, meanRich, StdErRich, stdDevRich, meanInd, stdErInd, stdDevInd)
head(df_laugi)
head(this.glm)
this.glm = glm(meanRich~meanInd + stdDevRich + stdDevInd + Morn.Noon, data= df_laugi)
summary(this.glm)

ggplot(this.glm, aes(x=meanRich, y=meanInd))+
  geom_jitter(alpha=0.4)+
  facet_grid(~Morn.Noon)+
  labs( title = "Lagoon Survey",
        subtitle ="Comparison of the Mean Species Richness Against Mean Number of Individuals for Morning and Afternoon Observations",
        x = "Mean Species Richness",
        y = "Mean Number of Individuals",
        caption = " A jitter plot displaying the difference of species richness against number of individuals for Morning and Afternoon observations")+
  theme(text = element_text(family="gara"))

print.data.frame(df_laugi)

#For weather
df_weather = left_join(lagoon, df_lagoon)%>%
  na.omit(Weather)%>%
  select(MNI, MNS, Morn.Noon, meanRich, StdErRich, stdDevRich, meanInd, stdErInd, stdDevInd, Weather)
head(df_weather)

weather.glm = glm(meanRich~meanInd + stdDevRich + stdDevInd + Morn.Noon + Weather, data= df_weather)

ggplot(weather.glm, aes(x=meanRich, y=meanInd))+
  geom_jitter(alpha=0.4)+
  geom_line()+
  facet_grid(~Weather)+
  labs( title = "Lagoon Survey",
        subtitle ="Comparison of the Mean Species Richness Against Mean Number of Individuals for Weather conditions",
        x = "Mean Species Richness",
        y = "Mean Number of Individuals",
        caption = " A jitter plot displaying the difference of species richness against number of individuals for weather conditions")



#Richness over time?
#df_time = left_join(df_lagoon, lagoon)
#head(df_time)
#timeGLM=  glm(meanRich~meanInd + stdDevRich + stdDevInd + Morn.Noon + Date, data= df_time)

#ggplot(df_time, aes(x=Date, y=meanRich))+
#  geom_jitter(alpha=0.4)+
#  labs(title= "Lagoon Survey",
#       subtitle= "Richness Over Time",
#       x = "Date",
#       y = "Species Richness",
#       caption ="Plot showing species Richness over time")
############ need to make Date smaller. by month?
######
#ggplot(df_laugi, aes(x=MNS, y= MNI, na.rm=TRUE))+
#  geom_jitter(alpha=0.2)+
#  geom_pointrange(aes(ymin=meanRich - StdErRich,
#                      ymax = meanRich + StdErRich))+
#  facet_grid(~Morn.Noon)+
#  labs(title ="Lagoon Survey",
#       subtitle="Species Richness Against Number of Individuals",
#       x = "Number of Individuals",
#       y="Species Richness",
#       caption="Point range graph disaplying the mean species richness of surveyed #lagoons split by morning and afternoon observations to show the effect of day period on species richness")
#just need to remove NA
#YUSS
#This does not look right... too many points
#####
###
####
#stick with boxplot
df_newBIG = left_join(lagoon, df_lagoon)
head(df_newBIG)
newBIG.glm = glm(MNS ~ MNI + Temperature...C.+ PH+ Weather + meanRich+ StdErRich+ stdDevRich+ meanInd+ stdErInd+ stdDevInd + Lagoon.ID+ Lag.composition+ Location.type, family =poisson, data=df_newBIG)

#by weather condition
head(newBIG.glm)
ggplot(newBIG.glm, aes(x=meanInd, y = meanRich))+
  geom_jitter(alpha = 0.4)+
  geom_line()+
  facet_grid(~Weather)+
  labs( title = "Lagoon Survey",
        subtitle = "Mean Species Richness Against Mean number of Individuals for all Weather Conditions",
        x = " Mean Number of Individuals",
        y = "Mean Species Richness",
        caption = "Jitter plot comparing the effect of weather conditions on the species richness of lagoons")

#by temperature (range?)

#by lagoon
ggplot(newBIG.glm, aes(x=meanInd, y = meanRich))+
  geom_jitter(alpha = 0.4)+
  geom_line()+
  facet_grid(~Lagoon.ID)+
  labs( title = "Lagoon Survey",
        subtitle = "Mean Species Richness Against Mean number of Individuals split by Lagoon",
        x = " Mean Number of Individuals",
        y = "Mean Species Richness",
        caption = "Jitter plot comparing the differences of species richness at lagoons")

#by LAgoon composition
ggplot(newBIG.glm, aes(x=meanInd, y = meanRich))+
  geom_jitter(alpha = 0.4)+
  geom_line()+
  facet_grid(~Lag.composition)+
  labs( title = "Lagoon Survey",
        subtitle = "Mean Species Richness Against Mean number of Individuals split by Lagoon Composition",
        x = " Mean Number of Individuals",
        y = "Mean Species Richness",
        caption = "Jitter plot comparing the differences of species richness for difference lagoon composition")

#by location

ggplot(newBIG.glm, aes(x=meanInd, y = meanRich))+
  geom_jitter(alpha = 0.4)+
  geom_line()+
  facet_grid(~Location.type)+
  labs( title = "Lagoon Survey",
        subtitle = "Mean Species Richness Against Mean number of Individuals split by Lagoon Location type",
        x = " Mean Number of Individuals",
        y = "Mean Species Richness",
        caption = "Jitter plot comparing the differences of species richness at different lagoon location types")

#by PH  ##same issue as temp. need tp be a range
#ggplot(newBIG.glm, aes(x=meanInd, y = meanRich))+
#  geom_jitter(alpha = 0.4)+
#  geom_line()+
#  facet_grid(~PH)+
#  labs( title = "Lagoon Survey",
#        subtitle = "Mean Species Richness Against Mean number of Individuals for PH Conditions"
#        x = " Mean Number of Individuals",
#        y = "Mean Species Richness",
#        caption = "Jitter plot comparing the effect of PH on the species richness of lagoons")
