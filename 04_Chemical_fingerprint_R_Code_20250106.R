###Chemical fingerprint fire salamander larvae experiment
#authors: Laura Schulte, Jeanne Friedrichs, Caroline Müller & Barbara A. Caspers
#Created: 22.05.2023
#last modified: 06.01.2025 



##################################
#1. Is there a size difference in the larvae between the two habitat types or between the locations? 
#import dataset
library(readxl)
setwd("F:/fire_salamander/22_Olfaktorik/02_Raw_data")
data <- read_excel("olfactory_samples_20230522.xlsx")

head(data)

#inspect raw data 
hist(data$length)
boxplot(data$length ~ data$habitat)
boxplot(data$length ~ data$location)

#test for normal distribution & homogeneity of variance
shapiro.test(data$length)

var.test(data$length ~ data$habitat)


#t test
t.test(data$length ~ data$habitat)


library(psych)
size_location <- aov(data$length ~ data$location)
summary(size_location)


#post hoc test 
pairwise.t.test(data$length, data$location, 
                p.adjust="bonferroni")


##################################
#2. How many metabolites are overlapping from the locations?

install.packages("VennDiagram")
install.packages("ggvenn")
library("VennDiagram")
library("ggvenn")

# use list as input for both habitats 
Vennplot <-list('Streams'=c(3691,44955,53105,112718,184172,195515,252217,313257,321204,335882,384961,409388,431648,493664,509823,564089,582942,749671,751779,774801,808726,868404,1032605,1056373,1131534,1143672,1196388,1201648,1228388,1268536,1446376,1491827,1618539,1754342,1764797,1798547,1831414,1921129,1979683,2042935,2064791,2085142,2108555,2111916,2509452,2575117,2587943,3013074,3298742,3369597,3409813,3411084,3469452,3478941,3518004,3521174,3668695,3710806,3719625,3786342,3791111,3979586,4010966,4115873,4213273,4619403,5114675,5417671,5725007,6024369,6120191,6318257,6417376,6512899,6618652,6812483,7020703,7124048,7315061,7612178,8111305,8312576,8411721,8524647,8715552,8814968,9117035,9220332,9613697,9713454,9819698,9925791,10621681,10819954,11224531,11811713,12311624,12920718,13014299,13310249,13519098,13613851,13717211,13823559,13922837,15112898,15214839,15317024,15416123,16213701,16610201,16726065,16819715,16913833,17018406,17414855,17712596,17818637,18012756,18112919,18513371,18621862,18718947,19314833,19618636,19922137,20019706,20124223,20221031,20512475,21222114,21717045,21824619,22024336,22212892,22325676,22522109,22619088,22718262,22917173,23123484,23222796,23419704,23625709,23718289,24112591,24319109,24914863,25324219,25512912,25617676,25924241,26018375,26113812,26319264,27010489,27317015,27412575,30815139,31318415,31622158,32618925,32724528,32813366,33423489,33522612,33910521,34519226,34912913,35320346,35514281,35815869,35918626,36114644,36216506,36421881,36724204,37011714,37212902,37313438,37510511,37611477,37718434,38020351,38116543,38219252,38313419,38520692,38713678,38819737,39313013,39417796,40215906,40611907,40811738,41015904,41212602,41417342),             '
                Ponds'=c(3691,44955,53105,112718,184172,312129,313257,321204,335882,384961,431648,474982,509823,564089,582942,749671,808726,868404,1032605,1056373,1131534,1143672,1196388,1201648,1228388,1268536,1446376,1491827,1754342,1764797,1831414,1921129,1979683,2042935,2085142,2111916,2587943,3013074,3298742,3369597,3409813,3411084,3469452,3478941,3518004,3668695,3710806,3786342,4010966,4115873,4213273,4399665,4599813,4609549,4618494,4619403,4629663,4686357,4788364,4806243,4946338,5028516,5114675,5417671,5725007,6024369,6120191,6318257,6417376,6512899,6618652,6812483,7020703,7124048,7315061,7612178,8111305,8312576,8411721,8524647,8715552,8814968,9117035,9220332,9613697,9713454,9819698,9925791,10621681,10819954,11224531,11811713,12311624,12920718,13014299,13310249,13613851,13717211,13823559,13922837,15112898,15317024,15416123,16213701,16610201,16726065,16819715,17414855,17818637,18012756,18621862,18718947,19314833,19922137,20124223,20221031,20512475,21717045,21824619,22212892,22522109,22619088,22718262,23222796,23718289,24112591,25324219,25924241,26018375,28514856,28911187,29724528,30022869,30815139,31020289,31318415,31720339,31821705,32311751,32413469,32618925,32724528,33423489,33522612,34519226,34912913,35514281,35918626,36114644,36216506,36421881,37011714,37212902,37313438,37510511,37611477,37718434,38219252,38520692,38713678,38819737,39417796,40811738,41015904,41417342,44519723,45222104,45410799,45510964,45611299,45712482,45810148,46513458,46711475,47914078,48510469,48614857,48824523,49115571,49210945,49510654,49715321,50017768))

# create plot
ggvenn(Vennplot)
ggvenn(Vennplot, fill_color = c("#228B22", "#8B864E"), text_size = 12, set_name_size = 12)

install.packages("eulerr")
library(eulerr)

fit <- euler(c('Streams'=22.8,'Ponds'=16.5,'Streams&Ponds'=60.8))
plot(fit, fill=c('#228B22', '#8B864E'))


###############################
#presence absence with water  

# use list as input for both habitats 
Vennplot <-list('Streams'=c(),'Ponds'=c())

# create plot
ggvenn(Vennplot)
ggvenn(Vennplot, fill_color = c("#008B45", "#8B0000"), stroke_size = 0.5, set_name_size = 4)




#################################
#3. Is there a difference in the alpha diversity (Shannon index) between the different habitat types? 
#import dataset
library(readxl)
setwd("D:/fire_salamander/22_Olfaktorik/04_Statistics")


####WATER SAMPLES
data <- read_excel("Shannon_diversity_water_20240902.xlsx")

head(data)

#inspect raw data 
hist(data$H)
boxplot(data$H ~ data$Habitat)
boxplot(data$H ~ data$Location)

#test for normal distribution & homogeneity of variance
shapiro.test(data$H)


install.packages("psych")
library(psych)

wilcox.test(H~Habitat, data = data, exact = TRUE)

# plot as boxplot
library(plyr)
library(ggpubr) 

Water<-
  ggplot(data=data, aes(Habitat,H), fill=Habitat)+
  geom_boxplot(aes(fill=Habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=Habitat), shape=21)+
  #geom_line(aes(group = ID), alpha = 0.6, colour = "black") +
  #facet_wrap(~habitat_before)+
  scale_fill_manual(values=c("#2778c3", "#c0c0c0"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  # annotate("text", x = 1.5, y = 25, size =10, label = "*")+
  #scale_x_discrete(labels=c("baseline","stress-induced"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat", y="Shannon Index")+
  #scale_y_continuous(breaks=seq(0,45,5), limits = c(2.5, 5))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()

Water


####LARVAL SAMPLES
data <- read_excel("Shannon_diversity_larvae_20240902.xlsx")

head(data)

#inspect raw data 
hist(data$H)
boxplot(data$H ~ data$Habitat)
boxplot(data$H ~ data$Location)

#test for normal distribution & homogeneity of variance
shapiro.test(data$H)


t.test(H~Habitat, data = data)

# plot as boxplot
library(plyr)
library(ggpubr) 

Larvae<-
  ggplot(data=data, aes(Habitat,H), fill=Habitat)+
  geom_boxplot(aes(fill=Habitat), outlier.shape = NA)+
  geom_jitter(aes(fill=Habitat), shape=21)+
  #geom_line(aes(group = ID), alpha = 0.6, colour = "black") +
  #facet_wrap(~habitat_before)+
  scale_fill_manual(values=c("#8B864E", "#228B22"))+
  stat_summary(fun=mean, shape=8, show.legend=FALSE) +
  # annotate("text", x = 1.5, y = 25, size =10, label = "*")+
  #scale_x_discrete(labels=c("baseline","stress-induced"))+
  theme_classic(base_size=12, base_family="Arial")+
  labs(x="Habitat", y="Shannon Index")+
  #scale_y_continuous(breaks=seq(0,45,5), limits = c(2.5, 5))+
  theme(axis.text.x=element_text(family="Arial", size=12, color="black"), 
        axis.text.y=element_text(family="Arial", size=12, color="black"),
        legend.position="none")
dev.off()

Larvae
