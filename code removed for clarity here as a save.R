# hiding messy code; may want it later?

#NICE1

#Making STD dev of behaviour 

#The standard deviation (SD) measures the amount of variability, or dispersion, from the individual data values to the mean

#  standard error of the mean (SEM) measures how far the sample mean (average) of the data is likely to be from the true population mean

#Lets do a SE first



#df_behav_1 %>%
#  group_by(Behavior)%>%
#  summarise(SE = std_err(Total.number.of.occurences),
#            sd(Total.number.of.occurences),
#            mean_B = mean(Total.number.of.occurences))%>%
#  print.data.frame(.)
#did it

#head(df_behav_1)  

#STD
#df_behav_1 %>%
#  group_by(Behavior)%>%
# summarise(sd(Total.number.of.occurences))%>%
#  print.data.frame(.)

#Can I create new DF with all the numbers i just worked out? 

#df_NEW_Bitches = df_behav_1 %>%
#  group_by(Behavior)%>%
#  summarise(mean_B = mean(Total.number.of.occurences),
#            SE = std_err(Total.number.of.occurences),
#            std = sd(Total.number.of.occurences))%>%
#  print.data.frame(.)

#Join new Df on to previous DF for ultimate df

################################################

df_allSSingle = read.csv("F:\\HoverFly\\BORIS_csv\\second 5 min\\all_in_one.csv")
df_S_GTB    = read.csv("F:\\HoverFly\\BORIS_csv\\second 5 min\\GTB.csv")

#new df of std err dev and means
print(df_allSSingle)
df_allSSingle_ = df_allSSingle %>%
  group_by(Behavior)%>%
  summarise(mean_B = mean(Total_number_of_occurences),
            SE = std_err(Total_number_of_occurences),
            std = sd(Total_number_of_occurences),
  )%>%
  print.data.frame(.)

#getting occurance and frequency from Boris grouped csv
df_S_GTB_ = df_S_GTB %>%
  select(Behavior, Total.number.of.occurences, frequency)%>%
  group_by(Behavior)%>%
  summarise(Total_occ = Total.number.of.occurences,
            S_freq = frequency)%>%
  print.data.frame(.)

#Cool, Weel done. next is to work out how to make R compare these.
#need to make a linear model comparing the means/behaviour for first half and second half.
#Q - can linear model be made from two df?
#Ans Dunno but can rename variables AND join df - will do this
head(df_S_GTB_)


#the renaming process - remember that this can be skipped if names done correctly to begin with
df_aSS = df_allSSingle_%>%
  group_by(Behavior)%>%
  summarise(Smean_B = mean_B,
            S_SE_B = SE,
            S_std_B = std)%>%
  print.data.frame(.)
head(df_allSSingle_)
head(df_aSS)

#mport(sjmisc)joinging up those two df to make one with everything we want to work with
df_SS_GTB = left_join(df_aSS, df_S_GTB_)%>%
  print.data.frame(.)
#check where we are
head(df_SS_GTB)
head(df_aFS_GTB)
#All good, lets merge those two so we can make LM and plot some data!
#Rotate both df before join...
install.packages(sjmisc)

rotate_df(df_aFS_GTB,cn=TRUE)%>%
  print.data.frame(.)


df_FS_Sing = left_join(df_aFS_GTB, df_SS_GTB)%>%
  print.data.frame(.)
head(df_FS_Sing)
tail(df_FS_Sing)
#Where the fuck is Smean etc !"!"!"!"!
#i think we need to do another name for total occurances 


ggplot(df_FS_Sing, aes(x=Behavior, y= Total_number_of_occurences))
#next to make lm

#lm_FS_single = lm(Fmean_B ~ Smean_B, data=df_FS_Sing)
#summary(lm_FS_single)
df_allFSingle = read.csv("F:\\HoverFly\\BORIS_csv\\first 5 min\\all_in_one.csv")
df_F_GTB      = read.csv("F:\\HoverFly\\BORIS_csv\\first 5 min\\GTB.csv")

#remove Modifiers col
df_allFSingle = df_allFSingle %>%
  select(-Modifiers)%>%
  print.data.frame(.)
head(df_allFSingle)

#creating mean, std err and std dev from total njumber of occurances grouped by behavior
df_allFSingle_ = df_allFSingle %>%
  group_by(Behavior)%>%
  summarise(mean_B = mean(Total_number_of_occurences),
            SE = std_err(Total_number_of_occurences),
            std = sd(Total_number_of_occurences),
  )%>%
  print.data.frame(.)
#getting numb occurance and frequency from BORIS' grouped csv
df_F_GTB_ = df_F_GTB %>%
  select(Behavior, Total.number.of.occurences, frequency)%>%
  group_by(Behavior)%>%
  summarise(Total_occ = Total.number.of.occurences,
            F_Freq = frequency)%>%
  print.data.frame(.)

#renaming/ making new cols in new df of mean std dev and se (could've named them bettwe to begin with)
df_aFS = df_allFSingle_ %>%
  group_by(Behavior)%>%
  summarise(Fmean_B = mean_B,
            F_SE_B = SE,
            F_std_B = std
  )%>%
  print.data.frame(.)

#joinging the just renamed df and the BORIS grouped df to make one df with all info i need
df_aFS_GTB = left_join(df_aFS, df_F_GTB_)
print(df_aFS_GTB)


#Do same for other half

