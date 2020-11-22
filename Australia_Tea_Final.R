#Australian Teabag analysis#
#Haydn Thomas#
#24th May 2017#
#Sleeping Willow is down under#

#Detach packages####
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}

detachAllPackages()

####Open packages####
library(lme4)
library(nlme)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(DMwR)
require(gridExtra)
require(tidyr)
require(reshape2)
require(grid)
require(effects)
library(lmerTest) # p-values

#Some custom functions
`%notin%` <- function(x,y) !(x %in% y)
se <- function(x) sqrt(var(x, na.rm=T)/length(x))

#Import data####
tea<-read.csv(file="scripts/users/hthomas/data/Tea/Aus_tea.csv") #I've emailed you this file
tea$Tea_init<-as.numeric(as.character(tea$Tea_init)) #Convert to numeric
tea$Tea_final<-as.numeric(as.character(tea$Tea_final)) #Convert to numeric
tea$Loss<-1-(tea$Tea_final / tea$Tea_init) #Recalculate Loss w. more decimal places
tea$Days<-as.numeric(as.character(tea$Days)) #Convert to numeric
tea<-tea[!is.na(tea$Loss),] #Remove NA losses
tea<-tea[!is.na(tea$Days),] #Remove NA days
tea$Burial<-as.Date(tea$Burial, format = "%d/%m/%Y") #Convert to date
tea$Recovery<-as.Date(tea$Recovery, format = "%d/%m/%Y") #Convert to date
tea$latlon<-paste(tea$Lat,"_",tea$Lon,sep="") #Create unique coordinate 

#Calculate Tea Bag Index values####

#Some fixed values
Hg<-0.842 
Hr<-0.552

tea$Tea_ID<-as.character(tea$Tea_ID) #Create unique tea ID
tea$Plot<-as.character(tea$Plot) #Create unique plot ID

tea$ID1<-sapply(strsplit(tea$Tea_ID, "_"), "[", 1) #Extract tea information
tea$ID2<-sapply(strsplit(tea$Tea_ID, "_"), "[", 2) #Extract tea information
tea$ID3<-sapply(strsplit(tea$Tea_ID, "_"), "[", 3) #Extract tea information
tea$rep<-sapply(strsplit(tea$Plot, "_"), "[", 5) #Extract plot replicate

#Reshape data
collect<-tea %>% 
  group_by(Site,Plot,Season,Tea_Type) %>% 
  select(ID1, ID2,rep,Site,Plot,Days,Tea_Type,Loss,Season) %>%
  spread(Tea_Type, Loss) %>%
  arrange(Site)

#Calculate TBI on individual basis
collect$S<-1-(collect$Green/Hg) #Calculate S
collect$ar<-Hr*(1-collect$S)
collect$k<-log(collect$ar/((1-collect$Rooibos)-(1-collect$ar)))/collect$Days #Calculate k

# #Calculate values for sites with grouped tea bags (where e.g. one teabag was lost)
# site_TBI<-collect %>%
#   group_by(Site,Plot,Season) %>%
#   summarise(green_mean = mean(Green,na.rm=T),
#             red_mean = mean(Rooibos,na.rm=T),
#             green_se = se(Green),
#             red_se = se(Rooibos))
# site_TBI$Days<-collect$Days[match(site_TBI$Plot,collect$Plot)]
# 
# #Plot Means
# site_TBI$S_mean<-1-(site_TBI$green_mean/Hg)
# site_TBI$ar_mean<-Hr*(1-site_TBI$S_mean)
# site_TBI$k_mean<-log(site_TBI$ar_mean/((1-site_TBI$red_mean)-(1-site_TBI$ar_mean)))/site_TBI$Days
# site_TBI$isplot<-"Plot Mean"
# 
# #Plot Standard errors
# site_TBI$S_mean_plusse<-1-((site_TBI$green_mean-site_TBI$green_se)/Hg)
# site_TBI$S_mean_minusse<-1-((site_TBI$green_mean+site_TBI$green_se)/Hg)
# 
# site_TBI$ar_mean_plusse<-Hr*(1-site_TBI$S_mean_plusse)
# site_TBI$ar_mean_minusse<-Hr*(1-site_TBI$S_mean_minusse)
# 
# site_TBI$k_mean_plusse<-log(site_TBI$ar_mean_minusse/((1-(site_TBI$red_mean+site_TBI$red_se))-(1-site_TBI$ar_mean_minusse)))/site_TBI$Days
# site_TBI$k_mean_minusse<-log(site_TBI$ar_mean_plusse/((1-(site_TBI$red_mean-site_TBI$red_se))-(1-site_TBI$ar_mean_plusse)))/site_TBI$Days
# 
# #Combine all together
# both_TBI<-select(site_TBI,Site,Plot,S_mean,k_mean,S_mean_plusse,S_mean_minusse,k_mean_plusse,k_mean_minusse,isplot)

#Add back to tea dataframe
tea<-collect

#Add snowpatch information####
tea$snow<-as.factor(ifelse(grepl("late",tea$Plot),"Late",
                           ifelse(grepl("mid",tea$Plot),"Mid","Early")))

tea$snow<- factor(tea$snow,levels(tea$snow)[c(1,3,2)]) #Convert to factor

#Back to long form
tea_green<-select(tea,-Rooibos)
names(tea_green)[8]<-"Loss"
tea_green$Tea_Type<-"Green"

tea_red<-select(tea,-Green)
names(tea_red)[8]<-"Loss"
tea_red$Tea_Type<-"Rooibos"

tea<-rbind(tea_green, tea_red)


#Add snow days
tea$Snow_Days<-ifelse(tea$snow=="Late", 179,ifelse(tea$snow=="Mid",169,153))

#Add season+tea
tea$Season_Tea<-paste(tea$Season,tea$Tea_Type)


#Basic analysis####

#View tea differences - this is just the density plots between red and green tea####

ggplot(tea, aes(x=Loss*100))+
  geom_density(alpha=0.25, adjust=3, aes(y = ..density..*150,fill=factor(Tea_Type)))+
  geom_histogram(alpha=0.9,bins=20,position = 'stack', aes(y = ..density..*100,fill=factor(snow)),colour="black")+
  scale_x_continuous(limits = c(0,100))+
  scale_fill_manual(values = c("grey40","darkgreen",  "white","grey75","red"),
                    name=NULL,
                    breaks = c("Green", "Rooibos", "Early", "Mid","Late"),
                    labels = c("Green tea", "Rooibos tea", "Early snowmelt", "Mid snowmelt", "Late snowmelt"))+
  scale_colour_manual(values = c("darkgreen", "red"),name="Tea Type")+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(y="Proportion of teabags (%)",x="% Mass Loss")+
  ylim(0,15)

#Comparison of snow-patch melt time####

#Calculate significances

##Winter
w_g<-pairwise.t.test(tea[tea$Tea_Type=="Green"&tea$Season=="Winter",]$Loss, tea[tea$Tea_Type=="Green"&tea$Season=="Winter",]$snow, p.adj = "bonf")
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Winter",]$Loss, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Winter",]$snow, p.adj = "bonf")
w<-pairwise.t.test(tea[tea$Season=="Winter",]$Loss,tea[tea$Season=="Winter",]$Tea_Type,p.adj = "bonf")

#Significance bar locations
df1 <- data.frame(a = c(0.75, 0.75,1.0,1.25,1.25), b = c(81, 82, 82, 82, 81))
df2 <- data.frame(a = c(0.75, 0.75,0.9,1,1), b = c(75, 76, 76, 76, 75))
df3 <- data.frame(a = c(1, 1,1.1,1.25,1.25), b = c(67, 68, 68, 68, 67))

df4 <- data.frame(a = c(1.75, 1.75,2,2.25,2.25), b = c(42, 43, 43, 43, 42))
df5 <- data.frame(a = c(1.75, 1.75,1.9,2,2), b = c(36, 37, 37, 37, 36))
df6 <- data.frame(a = c(2, 2,2.1,2.25,2.25), b = c(33, 34, 34, 34, 33))

df7 <- data.frame(a = c(1,1,1.5,2,2), b = c(98, 99, 99, 99, 98))

#Draw boxplot
(winter<-ggplot(tea[tea$Season=="Winter",], aes(Tea_Type,Loss*100))+
  geom_boxplot(aes(fill=snow))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Tea Type",y="% Mass Loss")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("b) Across melt zones: Winter + Spring \n    incubation")+
  geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1, y = 83, label = ifelse(w_g$p.value[2,1]<0.001,"***",ifelse(w_g$p.value[2,1]<0.01,"**",ifelse(w_g$p.value[2,1]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 0.875, y = 77, label = ifelse(w_g$p.value[1,1]<0.001,"***",ifelse(w_g$p.value[1,1]<0.01,"**",ifelse(w_g$p.value[1,1]<0.05,"*","ns"))), size = 5) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 1.125, y = 71, label = ifelse(w_g$p.value[2,2]<0.001,"***",ifelse(w_g$p.value[2,2]<0.01,"**",ifelse(w_g$p.value[2,2]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2, y = 44, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df5, aes(x = a, y = b)) + annotate("text", x = 1.875, y = 38, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 5) +
  geom_line(data = df6, aes(x = a, y = b)) + annotate("text", x = 2.125, y = 35, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df7, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 100, label = ifelse(w$p.value[1,1]<0.001,"***",ifelse(w$p.value[1,1]<0.01,"**",ifelse(w$p.value[1,1]<0.05,"*","ns"))), size = 5)+
  ylim(0,100))

##Summer
w_g<-pairwise.t.test(tea[tea$Tea_Type=="Green"&tea$Season=="Summer",]$Loss, tea[tea$Tea_Type=="Green"&tea$Season=="Summer",]$snow, p.adj = "bonf")
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Summer",]$Loss, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Summer",]$snow, p.adj = "bonf")
w<-pairwise.t.test(tea[tea$Season=="Summer",]$Loss,tea[tea$Season=="Summer",]$Tea_Type,p.adj = "bonf")

#Significance locations
df1 <- data.frame(a = c(0.75, 0.75,1.0,1.25,1.25), b = c(82, 83, 83, 83, 82))
df2 <- data.frame(a = c(0.75, 0.75,0.9,1,1), b = c(74, 75, 75, 75, 74))
df3 <- data.frame(a = c(1, 1,1.1,1.25,1.25), b = c(76, 77, 77, 77, 76))

df4 <- data.frame(a = c(1.75, 1.75,2,2.25,2.25), b = c(49, 50, 50, 50, 49))
df5 <- data.frame(a = c(1.75, 1.75,1.9,2,2), b = c(43, 44, 44, 44, 43))
df6 <- data.frame(a = c(2, 2,2.1,2.25,2.25), b = c(40, 41, 41, 41, 40))

df7 <- data.frame(a = c(1,1,1.5,2,2), b = c(98, 99, 99, 99, 98))

(Summer<-ggplot(tea[tea$Season=="Summer",], aes(Tea_Type,Loss*100))+
  geom_boxplot(aes(fill=snow))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Tea Type",y="% Mass Loss")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("c) Across melt zones: Summer \n    incubation")+
  geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1, y = 86, label = ifelse(w_g$p.value[2,1]<0.001,"***",ifelse(w_g$p.value[2,1]<0.01,"**",ifelse(w_g$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 0.875, y = 78, label = ifelse(w_g$p.value[1,1]<0.001,"***",ifelse(w_g$p.value[1,1]<0.01,"**",ifelse(w_g$p.value[1,1]<0.05,"*","ns"))), size = 3.5) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 1.125, y = 80, label = ifelse(w_g$p.value[2,2]<0.001,"***",ifelse(w_g$p.value[2,2]<0.01,"**",ifelse(w_g$p.value[2,2]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2, y = 53, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df5, aes(x = a, y = b)) + annotate("text", x = 1.875, y = 47, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 3.5) +
  geom_line(data = df6, aes(x = a, y = b)) + annotate("text", x = 2.125, y = 44, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df7, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 100, label = ifelse(w$p.value[1,1]<0.001,"***",ifelse(w$p.value[1,1]<0.01,"**",ifelse(w$p.value[1,1]<0.05,"*","ns"))), size = 5)+
  ylim(0,100))

##Year

w_g<-pairwise.t.test(tea[tea$Tea_Type=="Green"&tea$Season=="Year",]$Loss, tea[tea$Tea_Type=="Green"&tea$Season=="Year",]$snow, p.adj = "bonf")
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Year",]$Loss, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Year",]$snow, p.adj = "bonf")
w<-pairwise.t.test(tea[tea$Season=="Year",]$Loss,tea[tea$Season=="Year",]$Tea_Type,p.adj = "bonf")

#Significance locations
df1 <- data.frame(a = c(0.75, 0.75,1.0,1.25,1.25), b = c(89.5, 90.5, 90.5, 90.5, 89.5))
df2 <- data.frame(a = c(0.75, 0.75,0.9,1,1), b = c(83.5, 84.5, 84.5, 84.5, 83.5))
df3 <- data.frame(a = c(1, 1,1.1,1.25,1.25), b = c(81, 82, 82, 82, 81))

df4 <- data.frame(a = c(1.75, 1.75,2,2.25,2.25), b = c(63, 64, 64, 64, 63))
df5 <- data.frame(a = c(1.75, 1.75,1.9,2,2), b = c(57, 58, 58, 58, 57))
df6 <- data.frame(a = c(2, 2,2.1,2.25,2.25), b = c(47, 48, 48, 48, 47))

df7 <- data.frame(a = c(1,1,1.5,2,2), b = c(98, 99, 99, 99, 98))

Year<-ggplot(tea[tea$Season=="Year",], aes(Tea_Type,Loss*100))+
  geom_boxplot(aes(fill=snow))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Tea Type",y="% Mass Loss")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("a) Across melt zones: One year \n    incubation")+
  geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1, y = 93.5, label = ifelse(w_g$p.value[2,1]<0.001,"***",ifelse(w_g$p.value[2,1]<0.01,"**",ifelse(w_g$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 0.875, y = 87.5, label = ifelse(w_g$p.value[1,1]<0.001,"***",ifelse(w_g$p.value[1,1]<0.01,"**",ifelse(w_g$p.value[1,1]<0.05,"*","ns"))), size = 3.5) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 1.125, y = 85, label = ifelse(w_g$p.value[2,2]<0.001,"***",ifelse(w_g$p.value[2,2]<0.01,"**",ifelse(w_g$p.value[2,2]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2, y = 67, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df5, aes(x = a, y = b)) + annotate("text", x = 1.875, y = 59, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 5) +
  geom_line(data = df6, aes(x = a, y = b)) + annotate("text", x = 2.125, y = 51, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df7, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 100, label = ifelse(w$p.value[1,1]<0.001,"***",ifelse(w$p.value[1,1]<0.01,"**",ifelse(w$p.value[1,1]<0.05,"*","ns"))), size = 5)+
  ylim(0,100)

#Comparison of seasons across melt zones####
tea$Season<- factor(tea$Season,levels = c("Year","Summer","Winter"))

#Significances
w_g<-pairwise.t.test(tea[tea$Tea_Type=="Green",]$Loss, tea[tea$Tea_Type=="Green",]$Season, p.adj = "bonf")
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos",]$Loss, tea[tea$Tea_Type=="Rooibos",]$Season, p.adj = "bonf")
w<-pairwise.t.test(tea$Loss,tea$Tea_Type,p.adj = "bonf")

#Significance bars
df1 <- data.frame(a = c(0.75, 0.75,1.0,1.25,1.25), b = c(89, 90, 90, 90, 89))
df2 <- data.frame(a = c(0.75, 0.75,0.9,1,1), b = c(84, 85, 85, 85, 84))
df3 <- data.frame(a = c(1, 1,1.1,1.25,1.25), b = c(82, 83, 83, 83, 82))

df4 <- data.frame(a = c(1.75, 1.75,2,2.25,2.25), b = c(64, 65, 65, 65, 64))
df5 <- data.frame(a = c(1.75, 1.75,1.9,2,2), b = c(58, 59, 59, 59, 58))
df6 <- data.frame(a = c(2, 2,2.1,2.25,2.25), b = c(47, 48, 48, 48, 47))

df7 <- data.frame(a = c(1,1,1.5,2,2), b = c(98, 99, 99, 99, 98))

#Draw boxplot
season_diffs<-ggplot(tea, aes(Tea_Type,Loss*100))+
  geom_boxplot(aes(fill=Season))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Tea Type",y="% Mass Loss")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("d) Across seasons\n")+
  geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1, y = 91, label = ifelse(w_g$p.value[2,1]<0.001,"***",ifelse(w_g$p.value[2,1]<0.01,"**",ifelse(w_g$p.value[2,1]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 0.875, y = 86, label = ifelse(w_g$p.value[1,1]<0.001,"***",ifelse(w_g$p.value[1,1]<0.01,"**",ifelse(w_g$p.value[1,1]<0.05,"*","ns"))), size = 5) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 1.125, y = 84, label = ifelse(w_g$p.value[2,2]<0.001,"***",ifelse(w_g$p.value[2,2]<0.01,"**",ifelse(w_g$p.value[2,2]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df4, aes(x = a, y = b)) + annotate("text", x = 2, y = 68, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df5, aes(x = a, y = b)) + annotate("text", x = 1.875, y = 60, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 5) +
  geom_line(data = df6, aes(x = a, y = b)) + annotate("text", x = 2.125, y = 51, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5)+
  geom_line(data = df7, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 100, label = ifelse(w$p.value[1,1]<0.001,"***",ifelse(w$p.value[1,1]<0.01,"**",ifelse(w$p.value[1,1]<0.05,"*","ns"))), size = 5)+
  ylim(0,100)

#Overll figure:
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/patch_diffs.pdf", width = 7.5, height = 6)
snowpatch_diffs<-grid.arrange(Year,winter,Summer,season_diffs,ncol=2)
dev.off()

#TBI k####
#Calculate significances

##Winter
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Winter",]$k, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Winter",]$snow, p.adj = "bonf")

#Significance bar locations
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.0175, 0.018, 0.018, 0.018, 0.0175))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.0155, 0.016, 0.016, 0.016, 0.0155))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.0215, 0.022, 0.022, 0.022, 0.0215))

#Draw boxplot
(winter_k<-ggplot(tea[tea$Season=="Winter",], aes(snow,k))+
  geom_boxplot(aes(fill=snow))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Melt zone",y="TBI k (decomposition rate)")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("b) Across melt zones: Winter + Spring \n    incubation")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.019, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 3.5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.017, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.023, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
    ylim(0,0.025))

##Summer
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Summer",]$k, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Summer",]$snow, p.adj = "bonf")

#Significance locations
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.0175, 0.018, 0.018, 0.018, 0.0175))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.0155, 0.016, 0.016, 0.016, 0.0155))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.0215, 0.022, 0.022, 0.022, 0.0215))

(Summer_k<-ggplot(tea[tea$Season=="Summer",], aes(snow,k))+
    geom_boxplot(aes(fill=snow))+
    theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x="Melt zone",y="TBI k (decomposition rate)")+
    scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
    ggtitle("c) Across melt zones: Summer \n    incubation")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.019, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 3.5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.017, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.023, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
    ylim(0,0.025))

##Year
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Year",]$k, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Year",]$snow, p.adj = "bonf")

#Significance locations
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.0175, 0.018, 0.018, 0.018, 0.0175))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.0155, 0.016, 0.016, 0.016, 0.0155))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.0215, 0.022, 0.022, 0.022, 0.0215))

(Year_k<-ggplot(tea[tea$Season=="Year",], aes(snow,k))+
  geom_boxplot(aes(fill=snow))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Melt zone",y="TBI k (decomposition rate)")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("a) Across melt zones: One year \n    incubation")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.019, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 3.5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.017, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.023, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
    ylim(0,0.025))

#Comparison of seasons across melt zones####
tea$Season<- factor(tea$Season,levels = c("Year","Summer","Winter"))

#Significances
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos",]$k, tea[tea$Tea_Type=="Rooibos",]$Season, p.adj = "bonf")

#Significance bars
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.0175, 0.018, 0.018, 0.018, 0.0175))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.0155, 0.016, 0.016, 0.016, 0.0155))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.0215, 0.022, 0.022, 0.022, 0.0215))

#Draw boxplot
(season_diffs_k<-ggplot(tea, aes(Season,k))+
  geom_boxplot(aes(fill=Season))+
  theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  labs(x="Melt zone",y="TBI k (decomposition rate)")+
  scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
  ggtitle("d) Across seasons\n")+
  geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.019, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 5)+
  geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.017, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 5) +
  geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.023, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
  ylim(0,0.025))

#Overll figure:
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/patch_diffs_k.pdf", width = 7.5, height = 6)
snowpatch_diffs<-grid.arrange(Year_k,winter_k,Summer_k,season_diffs_k,ncol=2)
dev.off()

#TBI S####
#Calculate significances

##Winter
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Winter",]$S, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Winter",]$snow, p.adj = "bonf")

#Significance bar locations
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.35, 0.36, 0.36, 0.36, 0.35))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.39, 0.4, 0.4, 0.4, 0.39))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.42, 0.43, 0.43, 0.43, 0.42))

#Draw boxplot
(winter_S<-ggplot(tea[tea$Season=="Winter",], aes(snow,S))+
    geom_boxplot(aes(fill=snow))+
    theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x="Melt zone",y="TBI S (stabilisation factor)")+
    scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
    ggtitle("b) Across melt zones: Winter + Spring \n    incubation")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.37, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.41, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.44, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 5)+
    ylim(0.1,0.45))

##Summer
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Summer",]$S, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Summer",]$snow, p.adj = "bonf")

#Significance locations
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.35, 0.36, 0.36, 0.36, 0.35))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.39, 0.4, 0.4, 0.4, 0.39))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.42, 0.43, 0.43, 0.43, 0.42))

(Summer_S<-ggplot(tea[tea$Season=="Summer",], aes(snow,S))+
    geom_boxplot(aes(fill=snow))+
    theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x="Melt zone",y="TBI S (stabilisation factor)")+
    scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
    ggtitle("c) Across melt zones: Summer \n    incubation")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.37, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 3.5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.41, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.44, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
    ylim(0.1,0.45))

##Year
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos"&tea$Season=="Year",]$S, tea[tea$Tea_Type=="Rooibos"&tea$Season=="Year",]$snow, p.adj = "bonf")

#Significance locations
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.35, 0.36, 0.36, 0.36, 0.35))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.39, 0.4, 0.4, 0.4, 0.39))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.42, 0.43, 0.43, 0.43, 0.42))

(Year_S<-ggplot(tea[tea$Season=="Year",], aes(snow,S))+
    geom_boxplot(aes(fill=snow))+
    theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x="Melt zone",y="TBI S (stabilisation factor)")+
    scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
    ggtitle("a) Across melt zones: One year \n    incubation")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.37, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 3.5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.41, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 3.5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.44, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 3.5)+
    ylim(0.1,0.45))

#Comparison of seasons across melt zones####
tea$Season<- factor(tea$Season,levels = c("Year","Summer","Winter"))

#Significances
w_r<-pairwise.t.test(tea[tea$Tea_Type=="Rooibos",]$S, tea[tea$Tea_Type=="Rooibos",]$Season, p.adj = "bonf")

#Significance bars
df1 <- data.frame(a = c(1, 1,1.5,2,2), b = c(0.35, 0.36, 0.36, 0.36, 0.35))
df2 <- data.frame(a = c(2, 2,2.5,3,3), b = c(0.39, 0.4, 0.4, 0.4, 0.39))
df3 <- data.frame(a = c(1, 1,2,3,3), b = c(0.42, 0.43, 0.43, 0.43, 0.42))

#Draw boxplot
(season_diffs_S<-ggplot(tea, aes(Season,S))+
    geom_boxplot(aes(fill=Season))+
    theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
    labs(x="Melt zone",y="TBI S (stabilisation factor)")+
    scale_fill_manual(values = c("grey40", "grey75","white"),name="Snowmelt")+
    ggtitle("d) Across seasons\n")+
    geom_line(data = df1, aes(x = a, y = b)) + annotate("text", x = 1.5, y = 0.37, label = ifelse(w_r$p.value[1,1]<0.001,"***",ifelse(w_r$p.value[1,1]<0.01,"**",ifelse(w_r$p.value[1,1]<0.05,"*","ns"))), size = 5)+
    geom_line(data = df2, aes(x = a, y = b)) + annotate("text", x = 2.5, y = 0.41, label = ifelse(w_r$p.value[2,2]<0.001,"***",ifelse(w_r$p.value[2,2]<0.01,"**",ifelse(w_r$p.value[2,2]<0.05,"*","ns"))), size = 5) +
    geom_line(data = df3, aes(x = a, y = b)) + annotate("text", x = 2, y = 0.44, label = ifelse(w_r$p.value[2,1]<0.001,"***",ifelse(w_r$p.value[2,1]<0.01,"**",ifelse(w_r$p.value[2,1]<0.05,"*","ns"))), size = 5)+
    ylim(0.1,0.45))

#Overll figure:
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/patch_diffs_S.pdf", width = 7.5, height = 6)
snowpatch_diffs<-grid.arrange(Year_S,winter_S,Summer_S,season_diffs_S,ncol=2)
dev.off()


#Mixed model
lmer_elevation_a <- lmer(Loss ~  Snow_Days * Tea_Type  + (1|rep) + (1|Season), data=tea)
summary(lmer_elevation_a)
elevation_effects_a <- as.data.frame(effect(c("Snow_Days", "Tea_Type"), lmer_elevation_a, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - individual tea lines
(decompVele_indiv_ambient<-ggplot(tea) +
    geom_point(aes(Snow_Days, Loss, colour = Tea_Type), size=1.5) +
    theme_classic()+
    scale_fill_manual(values=c("#50BB50","#8B2323")) +
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('% Mass loss') + xlab('Snow cover duration (days)') +
    ggtitle("a) All incubations")+
    xlim(150,181) +
    ylim(0.05,0.8) +
    geom_ribbon(data = elevation_effects_a[elevation_effects_a$Tea_Type=="Green",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a[elevation_effects_a$Tea_Type=="Green",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    geom_ribbon(data = elevation_effects_a[elevation_effects_a$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a[elevation_effects_a$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") +
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Winter#
tea_winter<-subset(tea,Season=="Winter")

#Mixed model
lmer_elevation_a_winter <- lmer(Loss ~ Snow_Days* Tea_Type + (1|rep), data=tea_winter)
summary(lmer_elevation_a_winter)
elevation_effects_a_winter <- as.data.frame(effect(c("Snow_Days", "Tea_Type"), lmer_elevation_a_winter, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - winter
(decompVele_indiv_ambient_winter<-ggplot(tea_winter) +
    geom_point(aes(Snow_Days, Loss, colour = Tea_Type), size=1.5) +
    theme_classic()+
    scale_fill_manual(values=c("#50BB50","#8B2323")) +
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('% Mass loss') + xlab('Snow cover duration (days)') +
    ggtitle("b) Winter + Spring")+
    xlim(150,181) +
    ylim(0.05,0.8) +
    geom_ribbon(data = elevation_effects_a_winter[elevation_effects_a_winter$Tea_Type=="Green",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a_winter[elevation_effects_a_winter$Tea_Type=="Green",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    geom_ribbon(data = elevation_effects_a_winter[elevation_effects_a_winter$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a_winter[elevation_effects_a_winter$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") +
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Summer#
tea_summer<-subset(tea,Season=="Summer")

#Mixed model
lmer_elevation_a_summer <- lmer(Loss ~ Snow_Days * Tea_Type + (1|rep), data=tea_summer)
summary(lmer_elevation_a_summer)
elevation_effects_a_summer <- as.data.frame(effect(c("Snow_Days", "Tea_Type"), lmer_elevation_a_summer, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - summer
(decompVele_indiv_ambient_summer<-ggplot(tea_summer) +
    geom_point(aes(Snow_Days, Loss, colour = Tea_Type), size=1.5) +
    theme_classic()+
    scale_fill_manual(values=c("#50BB50","#8B2323")) +
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('% Mass loss') + xlab('Snow cover duration (days)') +
    ggtitle("c) Summer")+
    xlim(150,181) +
    ylim(0.05,0.8) +
    geom_ribbon(data = elevation_effects_a_summer[elevation_effects_a_summer$Tea_Type=="Green",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a_summer[elevation_effects_a_summer$Tea_Type=="Green",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    geom_ribbon(data = elevation_effects_a_summer[elevation_effects_a_summer$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a_summer[elevation_effects_a_summer$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") +
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Winter#
tea_year<-subset(tea,Season=="Year")

#Mixed model
lmer_elevation_a_year <- lmer(Loss ~ Snow_Days * Tea_Type + (1|rep), data=tea_year)
summary(lmer_elevation_a_year)
elevation_effects_a_year <- as.data.frame(effect(c("Snow_Days", "Tea_Type"), lmer_elevation_a_year, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - winter
(decompVele_indiv_ambient_year<-ggplot(tea_year) +
    geom_point(aes(Snow_Days, Loss, colour = Tea_Type), size=1.5) +
    theme_classic()+
    scale_fill_manual(values=c("#50BB50","#8B2323")) +
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('% Mass loss') + xlab('Snow cover duration (days)') +
    ggtitle("d) Full Year")+
    xlim(150,181) +
    ylim(0.05,0.8) +
    geom_ribbon(data = elevation_effects_a_year[elevation_effects_a_year$Tea_Type=="Green",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a_year[elevation_effects_a_year$Tea_Type=="Green",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    geom_ribbon(data = elevation_effects_a_year[elevation_effects_a_year$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a_year[elevation_effects_a_year$Tea_Type=="Rooibos",], mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") +
    scale_colour_manual(values=c("#50BB50", "#8B2323")))


#Save as image
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/tea_rates_comb.pdf", width = 6, height = 5.5)
grid.arrange(decompVele_indiv_ambient,decompVele_indiv_ambient_winter,decompVele_indiv_ambient_summer,decompVele_indiv_ambient_year,nrow = 2)
dev.off()


#TBI_k####
#Mixed model
lmer_elevation_a_k <- lmer(k ~ Snow_Days + (1|rep) + (1|Season), data=tea)
summary(lmer_elevation_a_k)
elevation_effects_a <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_k, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - individual tea lines
(decompVele_indiv_ambient_k<-ggplot(tea) +
    geom_point(aes(Snow_Days, k), colour = "#8B2323", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI k (decomposition rate)') + xlab('Snow cover duration (days)') +
    ggtitle("b) All incubations")+
    xlim(150,181) +
    ylim(0.00,0.025) +
    geom_ribbon(data = elevation_effects_a, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Winter#
tea_winter<-subset(tea,Season=="Winter")

#Mixed model
lmer_elevation_a_k_winter <- lmer(k ~ Snow_Days + (1|rep), data=tea_winter)
summary(lmer_elevation_a_k_winter)
elevation_effects_a_winter <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_k_winter, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - winter
(decompVele_indiv_ambient_winter_k<-ggplot(tea_winter) +
    geom_point(aes(Snow_Days, k), colour = "#8B2323", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI k (decomposition rate)') + xlab('Snow cover duration (days)') +
    ggtitle("d) Winter + Spring")+
    xlim(150,181) +
    ylim(0.00,0.025) +
    geom_ribbon(data = elevation_effects_a_winter, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a_winter, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Summer#
tea_summer<-subset(tea,Season=="Summer")

#Mixed model
lmer_elevation_a_k_summer <- lmer(k ~ Snow_Days + (1|rep), data=tea_summer)
summary(lmer_elevation_a_k_summer)
elevation_effects_a_summer <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_k_summer, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - summer
(decompVele_indiv_ambient_summer_k<-ggplot(tea_summer) +
    geom_point(aes(Snow_Days, k), colour = "#8B2323", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI k (decomposition rate)') + xlab('Snow cover duration (days)') +
    ggtitle("f) Summer")+
    xlim(150,181) +
    ylim(0.00,0.025) +
    geom_ribbon(data = elevation_effects_a_summer, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a_summer, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Year#
tea_year<-subset(tea,Season=="Year")

#Mixed model
lmer_elevation_a_k_year <- lmer(k ~ Snow_Days + (1|rep), data=tea_year)
summary(lmer_elevation_a_k_year)
elevation_effects_a_year <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_k_year, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - year
(decompVele_indiv_ambient_year_k<-ggplot(tea_year) +
    geom_point(aes(Snow_Days, k), colour = "#8B2323", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI k (decomposition rate)') + xlab('Snow cover duration (days)') +
    ggtitle("h) Full Year")+
    xlim(150,181) +
    ylim(0.00,0.025) +
    geom_ribbon(data = elevation_effects_a_year, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#8B2323") +
    geom_line(data = elevation_effects_a_year, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#8B2323") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))


#Save as image
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/tea_rates_comb_k.pdf", width = 6, height = 5.5)
grid.arrange(decompVele_indiv_ambient_k,decompVele_indiv_ambient_winter_k,decompVele_indiv_ambient_summer_k,decompVele_indiv_ambient_year_k,nrow = 2)
dev.off()

#TBI_S####
#Mixed model
lmer_elevation_a_S <- lmer(S ~ Snow_Days + (1|rep) + (1|Season), data=tea)
summary(lmer_elevation_a_S)
elevation_effects_a <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_S, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - individual tea lines
(decompVele_indiv_ambient_S<-ggplot(tea) +
    geom_point(aes(Snow_Days, S), colour = "#50BB50", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI S (stabilisation factor)') + xlab('Snow cover duration (days)') +
    ggtitle("a) All incubations")+
    xlim(150,181) +
    ylim(0.00,0.4) +
    geom_ribbon(data = elevation_effects_a, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Winter#
tea_winter<-subset(tea,Season=="Winter")

#Mixed model
lmer_elevation_a_S_winter <- lmer(S ~ Snow_Days + (1|rep), data=tea_winter)
summary(lmer_elevation_a_S_winter)
elevation_effects_a_winter <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_S_winter, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - winter
(decompVele_indiv_ambient_winter_S<-ggplot(tea_winter) +
    geom_point(aes(Snow_Days, S), colour = "#50BB50", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI S (stabilisation factor)') + xlab('Snow cover duration (days)') +
    ggtitle("c) Winter + Spring")+
    xlim(150,181) +
    ylim(0.00,0.4) +
    geom_ribbon(data = elevation_effects_a_winter, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a_winter, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Summer#
tea_summer<-subset(tea,Season=="Summer")

#Mixed model
lmer_elevation_a_S_summer <- lmer(S ~ Snow_Days + (1|rep), data=tea_summer)
summary(lmer_elevation_a_S_summer)
elevation_effects_a_summer <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_S_summer, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - summer
(decompVele_indiv_ambient_summer_S<-ggplot(tea_summer) +
    geom_point(aes(Snow_Days, S), colour = "#50BB50", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI S (stabilisation factor)') + xlab('Snow cover duration (days)') +
    ggtitle("e) Summer")+
    xlim(150,181) +
    ylim(0.00,0.4) +
    geom_ribbon(data = elevation_effects_a_summer, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a_summer, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))

#Year#
tea_year<-subset(tea,Season=="Year")

#Mixed model
lmer_elevation_a_S_year <- lmer(S ~ Snow_Days + (1|rep), data=tea_year)
summary(lmer_elevation_a_S_year)
elevation_effects_a_year <- as.data.frame(effect(c("Snow_Days"), lmer_elevation_a_S_year, xlevels = list(Snow_Days = seq(153, 179, 1))))

#Plot - year
(decompVele_indiv_ambient_year_S<-ggplot(tea_year) +
    geom_point(aes(Snow_Days, S), colour = "#50BB50", size=1.5) +
    theme_classic()+
    theme(legend.title = element_blank())+
    theme(legend.position = "none")+
    ylab('TBI S (stabilisation factor)') + xlab('Snow cover duration (days)') +
    ggtitle("g) Full Year")+
    xlim(150,181) +
    ylim(0.00,0.4) +
    geom_ribbon(data = elevation_effects_a_year, mapping = aes(x = Snow_Days, ymin = lower, ymax = upper), alpha = 0.2, fill = "#50BB50") +
    geom_line(data = elevation_effects_a_year, mapping = aes(x = Snow_Days, y = fit) , size = 0.5, colour = "#50BB50") + 
    scale_colour_manual(values=c("#50BB50", "#8B2323")))


#Save as image
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/tea_rates_comb_S.pdf", width = 6, height = 5.5)
grid.arrange(decompVele_indiv_ambient_S,decompVele_indiv_ambient_winter_S,decompVele_indiv_ambient_summer_S,decompVele_indiv_ambient_year_S,nrow = 2)
dev.off()


#Save as image
pdf(file="scripts/users/hthomas/Output_Images/Aus_T/tea_rates_comb_S_k.pdf", width = 6, height = 10)
grid.arrange(decompVele_indiv_ambient_S,decompVele_indiv_ambient_k,
             decompVele_indiv_ambient_winter_S,decompVele_indiv_ambient_winter_k,
             decompVele_indiv_ambient_summer_S,decompVele_indiv_ambient_summer_k,
             decompVele_indiv_ambient_year_S,decompVele_indiv_ambient_year_k,
             nrow = 4)
dev.off()


