#DLC positions
library(ggplot2)
#Single
#Single .0
df_Sin_cam6_V1.0 = read.csv("F:\\HoverFly\\dlc_csvss\\single.9.7.cam6_Vid1.csv")
head(df_Sin_cam6_V1.0)
par(mfrow = c (2,2))
sin6V1mod = lm(Head.FrontBody_x ~ Head.FrontBody_y,data=df_Sin_cam6_V1.0)
plot(sin6V1mod)

head(df_Sin_cam6_V1.0)


df_Sin_6V1.0_Mid = df_Sin_cam6_V1.0%>%
  select(Midbody_x, Midbody_y, frame)

head(df_Sin_6V1.0_Mid)
ggplot(df_Sin_cam6_V1.0, aes(x=Midbody_x, y= Midbody_y, color=frame))+
  geom_jitter(alpha=0.4)
  geom_text(aes(label=frame), hjust=0, vjust=0)

Sin_6_v1Midbody = pnorm(df_Sin_cam6_V1.0$Midbody_x, df_Sin_cam6_V1.0$Midbody_y)
Sin_6_v1Head = pnorm(df_Sin_cam6_V1.0$Head.FrontBody_x, df_Sin_cam6_V1.0$Head.FrontBody_y)

##########################
###########
#This graph does not match the video.
#############
############################
df_Sin_cam6_V1.5 = read.csv("F:\\HoverFly\\dlc_csvss\\single.9.7.cam6_Vid1_V1-0002.csv")


ggplot(df_Sin_cam6_V1.5, aes(x=Midbody_x, y= Midbody_y, color=frame))+
  geom_jitter(alpha=0.04)
#This is upsidedown


df_Sin_9721V1 = read.csv("F:\\HoverFly\\dlc_csvss\\Single_9.7_V1.csv")
ggplot(df_Sin_9721V1, aes(x=Midbody_x, y= Midbody_y, color=frame))+
  geom_jitter(alpha=0.04)
#nope
df_Sin_9721V1.5 = read.csv("F:\\HoverFly\\dlc_csvss\\single.9.7_V1.5.csv")
ggplot(df_Sin_9721V1.5, aes(x=Midbody_x, y= Midbody_y, color=frame))+
  geom_jitter(alpha=0.04)+
  geom_line()
#Nope

######################






#Group3
df_G3_38_V1 = read.csv("F:\\HoverFly\\dlc_csvss\\FromLucy\\Group_3_38_V1.0DLC_resnet50_Test_Hoverfly_1Jun26shuffle1_5000.csv")
ggplot(df_G3_38_V1, aes(x=Midbody_x, y= Midbody_y, color=frame))+
  geom_jitter(alpha=0.04)


#Group
#Group.0

#Group.5


#All body points

All_bod1 = read.csv("F:\\HoverFly\\dlc_csvss\\FromLucy\\formatted_for_R\\Group_3_38_V1.0DLC_resnet50_Test_Hoverfly_1Jun26shuffle1_5000.csv")

head(All_bod1)
ggplot(All_bod1, aes(x = Head.FrontBody.x, y = Head.FrontBody.y))+
  geom_jitter(alpha=0.4, color="red")+
  geom_jitter(aes(x=Midbody.x, y =Midbody.y), color="green")+
  geom_jitter(aes(x=Tail_base.x, y=Tail_base.y), color="blue")

All_bod2 = read.csv("F:\\HoverFly\\dlc_csvss\\FromLucy\\formatted_for_R\\Group_3_38_V1.5DLC_resnet50_Test_Hoverfly_1Jun26shuffle1_5000.csv") 
head(All_bod2)
ggplot(All_bod2, aes(x = Head.FrontBody.x, y = Head.FrontBody.y))+
  geom_jitter(alpha=0.4, color="red")+
  geom_jitter(aes(x=Midbody.x, y =Midbody.y), color="green")+
  geom_jitter(aes(x=Tail_base.x, y=Tail_base.y), color="blue")
