setwd("D:/Dropbox/Research/Experiment_JuvLobsterOA")
EI<-read.csv("Experiment_JuvLobsterOA_RFile_AntennaeStiffness.csv")
head(EI)
attach(EI)
names(EI)
nrow(EI)

unique(EI$molt_status)
unique(EI$run)

EI<-subset(EI, run=="1") #gets rid of duplicate run

plot(EI$treatment, EI$proximal_ei)
plot(EI$treatment, EI$distal_ei)


#Using all the data
EI_unmolt<-subset(EI, molt_status=="0") #Lobsters that didnt' molt
EI_molt<-subset(EI, molt_status=="1")
plot(EI_unmolt$treatment, EI_unmolt$proximal_ei)
plot(EI_molt$treatment, EI_molt$proximal_ei)
plot(EI_unmolt$treatment, EI_unmolt$distal_ei)
plot(EI_molt$treatment, EI_molt$distal_ei)

#Testing whether molting matters for each treatment at each location (x8)
#A
shapiro.test(subset(EI_molt,treatment=="A")$proximal_ei) #A is normal
shapiro.test(subset(EI_unmolt,treatment=="A")$proximal_ei) #A is normal
EI_A<-rbind(subset(EI_unmolt, treatment=="A"), subset(EI_molt, treatment=="A")) #make data frame of just A
bartlett.test(EI_A$proximal_ei~EI_A$molt_status) #equal variances
t.test(EI_A$proximal_ei~EI_A$molt_status) #molting doesn't matter

shapiro.test(subset(EI_molt,treatment=="A")$distal_ei) #A is normal
shapiro.test(subset(EI_unmolt,treatment=="A")$distal_ei) #A is normal
bartlett.test(EI_A$distal_ei~EI_distal_A$molt_status) #equal variances
t.test(EI_A$distal_ei~EI_distal_A$molt_status) #molting doesn't matter

#B doesn't work cuz there's only b that molted. HOwever, the single molted one is outside one sd of unmolted ones. But what can you do? 

#C
shapiro.test(subset(EI_molt,treatment=="C")$proximal_ei) #normal
shapiro.test(subset(EI_unmolt,treatment=="C")$proximal_ei) #normal
EI_C<-rbind(subset(EI_unmolt, treatment=="C"), subset(EI_molt, treatment=="C")) #make data frame of just A
bartlett.test(EI_C$proximal_ei~EI_C$molt_status) #equal variances
t.test(EI_C$proximal_ei~EI_C$molt_status) #molting doesn't matter

#D doesn't work cuz there's only one that molted

#####So, can combine molted and unmolted animals###



##############
###Proximal##
############
#all molt statuses
shapiro.test(subset(EI,treatment=="A")$proximal_ei) #A is normal
shapiro.test(subset(EI,treatment=="B")$proximal_ei) #B is normal
shapiro.test(subset(EI,treatment=="C")$proximal_ei) #C is normal
shapiro.test(subset(EI,treatment=="D")$proximal_ei) #D is normal
bartlett.test(EI$proximal_ei~EI$treatment) #very not equal; one high data point in A
oneway.test(proximal_ei~treatment, data=EI, var.equal=FALSE)
require(userfriendlyscience)
posthocTGH(EI$proximal_ei, EI$treatment, method="games-howell", digits=2)

#Still, testing if things are different among molted animals
#molted
EI_moltrep<-rbind(subset(EI, treatment=="A"), subset(EI, treatment=="C")) #only using treatments w/ 2+ animals
shapiro.test(subset(EI_moltrep,treatment=="A")$proximal_ei) #A is normal
shapiro.test(subset(EI_moltrep,treatment=="C")$proximal_ei) #C is normal
bartlett.test(EI_moltrep$proximal_ei~EI_moltrep$treatment) #very not equal; one high data point in A
oneway.test(proximal_ei~treatment, data=EI_moltrep, var.equal=FALSE) #not different


#unmolted  ******
shapiro.test(subset(EI_unmolt,treatment=="A")$proximal_ei) #A is normal
shapiro.test(subset(EI_unmolt,treatment=="B")$proximal_ei) #B is normal
shapiro.test(subset(EI_unmolt,treatment=="C")$proximal_ei) #C is normal
shapiro.test(subset(EI_unmolt,treatment=="D")$proximal_ei) #D is normal
bartlett.test(EI_unmolt$proximal_ei~EI_unmolt$treatment) #very not equal; one high data point in A
oneway.test(proximal_ei~treatment, data=EI_unmolt, var.equal=FALSE) #barely different
require(userfriendlyscience)
posthocTGH(EI_unmolt$proximal_ei, EI_unmolt$treatment, method="games-howell", digits=4) #B &D are different

      

##############
###Distal##
############
#all molt statuses
shapiro.test(subset(EI,treatment=="A")$distal_ei) #A is normal
shapiro.test(subset(EI,treatment=="B")$distal_ei) #B is not normal
shapiro.test(subset(EI,treatment=="C")$distal_ei) #C is normal
shapiro.test(subset(EI,treatment=="D")$distal_ei) #D is not normal
bartlett.test(EI$distal_ei~EI$treatment) #equal
kruskal.test(EI$distal_ei~EI$treatment) #not different

#unmolted ******
shapiro.test(subset(EI_unmolt,treatment=="A")$distal_ei) #A is normal
shapiro.test(subset(EI_unmolt,treatment=="B")$distal_ei) #B is not normal
shapiro.test(subset(EI_unmolt,treatment=="C")$distal_ei) #C is normal
shapiro.test(subset(EI_unmolt,treatment=="D")$distal_ei) #D is not normal
bartlett.test(EI_unmolt$distal_ei~EI_unmolt$treatment) #equal
kruskal.test(EI_unmolt$distal_ei~EI_unmolt$treatment) #not different

#molted
EI_moltrep<-rbind(subset(EI, treatment=="A"), subset(EI, treatment=="C")) #only using treatments w/ 2+ animals
shapiro.test(subset(EI_moltrep,treatment=="A")$distal_ei) #A is normal
shapiro.test(subset(EI_moltrep,treatment=="C")$distal_ei) #C is normal
bartlett.test(EI_moltrep$distal_ei~EI_moltrep$treatment) #very not equal; one high data point in A
summary(aov(distal_ei~treatment, data=EI_moltrep)) #not different


#################################################### GRAPHING #####################################\

##Graphing unmolted animals // MANUSCRIPT FIGURE

#Setting up dataframe to have a location factor
library(reshape2)
EI_unmoltlong <- melt(EI_unmolt[1:4], id.vars = c("treatment", "lobster"))
names(EI_unmoltlong)[1]<-"Treatment"
names(EI_unmoltlong)[3]<-"Location"
names(EI_unmoltlong)[4]<-"stiffness"

###Final touches on stats 
##For geom_bar
require(plyr)
require(magrittr)
require(forcats)
flexural_stats<-ddply(EI_unmoltlong,~Treatment + Location,summarise,mean=mean(stiffness),sd=sd(stiffness)) #computes the mean &sd per treatment
flexural_stats$ID<-paste0(flexural_stats$Location, flexural_stats$Treatment) #creates ID for textures later
flexural_stats <- flexural_stats %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Location = fct_recode(Location,"Proximal" = "proximal_ei", "Distal" = "distal_ei"))
flexural_stats <- flexural_stats %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67 ±0.10" = "D", "7.67 ±0.05" = "C"))
levels(flexural_stats$Treatment)<-gsub(" ", "\n",levels(flexural_stats$Treatment))

#For geom_box
EI_unmoltlong$ID<-paste0(EI_unmoltlong$Location, EI_unmoltlong$Treatment) #creates ID for textures later
EI_unmoltlong <- EI_unmoltlong %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Location = fct_recode(Location,"Proximal" = "proximal_ei", "Distal" = "distal_ei"))
EI_unmoltlong <- EI_unmoltlong %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67 ±0.10" = "D", "7.67 ±0.05" = "C"))
levels(EI_unmoltlong$Treatment)<-gsub(" ", "\n",levels(EI_unmoltlong$Treatment))

#Getting colors for graph #For future, order is wrong! pay attention when making new names
imagesantenna = c(
  proximal_eiA = "https://i.postimg.cc/HxQv7MhM/Experiment-Juv-Lobster-OA-RFile-SEMExo-Antenna-Pattern.jpg",
  distal_eiA = "https://i.postimg.cc/g2QgS49R/Experiment-Juv-Lobster-OA-RFile-SEMAntenna-Endo-Pattern.jpg", 
  proximal_eiB = "https://i.postimg.cc/kGLj2fw6/Experiment-Juv-Lobster-OA-RFile-SEMExo-Antenna-BPattern.jpg",
  distal_eiB = "https://i.postimg.cc/QCF4NRgZ/Experiment-Juv-Lobster-OA-RFile-SEMAntenna-BEndo-Pattern.jpg", 
  distal_eiC = "https://i.postimg.cc/JnvgXrzz/Experiment-Juv-Lobster-OA-RFile-SEMAntenna-CEndo-Pattern.jpg",
  distal_eiD = "https://i.postimg.cc/0QbFrMx4/Experiment-Juv-Lobster-OA-RFile-SEMAntenna-DEndo-Pattern.jpg",
  proximal_eiC = "https://i.postimg.cc/XNBtbCsG/Experiment-Juv-Lobster-OA-RFile-SEMExo-Antenna-CPattern.jpg",
  proximal_eiD  = "https://i.postimg.cc/SR9P7Bn7/Experiment-Juv-Lobster-OA-RFile-SEMExo-Antenna-DPattern.jpg" )

require(dplyr)
EI_unmolt%>% #How many replicates I have of each region for my graphing
  group_by(treatment) %>% 
  count()

library(ggtextures) #for barplot textures
library(ggsignif) 

ggplot(data=EI_unmoltlong, aes(x=Treatment, y=mean, image=ID)) + 
  #data=flexural_stats, aes(x=Treatment, y=mean, image=ID)) + 
  theme_bw() + #cheap way of removing gray background. Write over most of its defaults
  #geom_textured_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_image_manual(values = imagesantenna) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.9)) + 
  scale_y_continuous(name="Flexural stiffness (Nm²)") + 
  expand_limits(y = c(0, 0.04)) + 
  theme(#legend.position=c(0.75,0.89),
    legend.position="none",
    legend.title=element_text(size = 15), 
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
    panel.border = element_rect(fill=NA, colour='black'), 
    axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
    axis.text.y  = element_text(color="black", size=17), 
    axis.title.x = element_blank(),
    axis.title.y = element_text(color="black", size=20)) + 
  geom_hline(yintercept = 0) +
  annotate(geom="text", x=1, y=-0.002, label="10", color="black", size=5) +
  annotate(geom="text", x=2, y=-0.002, label="14", color="black", size=5) +
  annotate(geom="text", x=3, y=-0.002, label="11", color="black", size=5) +
  annotate(geom="text", x=4, y=-0.002, label="15", color="black", size=5) +
  #geom_signif(annotations=c("*"), y_position=.038, xmin=2.25, xmax=4.25, tip_length = c(0.01, 0.01)) +
  geom_signif(annotations=c("0.035"), y_position=.0373, xmin=2.25, xmax=4.25, tip_length = c(0.01, 0.01)) +
  ggsave("Experiment_JuvLobsterOA_Image_AntennaStiffnessManuscript.jpg.jpg", width=7.48, height=4, dpi=600)

#Boxplot
ggplot(data=EI_unmoltlong, aes(x = Treatment, y=stiffness, fill=ID, color=ID)) + 
  theme_bw() +
  geom_boxplot_pattern(aes(pattern = Location, pattern_fill = Location), pattern_density = 0.35, outlier.shape = NA) +
  scale_pattern_manual(values= c("Proximal" = "stripe", "Distal" = "none")) + # manually assign pattern
  scale_pattern_fill_manual(values=c("Proximal" = "white", "Distal" = "black"))  +
  scale_color_manual(values=rep("#000000", times=8)) + 
  scale_fill_manual(values=c("distal_eiA"="#fdcc8a","distal_eiB"="#fbb4b9", "distal_eiC"="#bfebeb","distal_eiD"="#dfffcc", "proximal_eiA"="#fdcc8a","proximal_eiB"="#fbb4b9", "proximal_eiC"="#bfebeb","proximal_eiD"="#dfffcc"))+
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Flexural stiffness (Nm²)") + 
    stat_summary(fun=base::mean, geom="point", shape=23, size=2, color="black", position=position_dodge(width=-0.75)) +    
  stat_summary(fun=base::mean, geom="point", shape=23, size=1, color="white", position=position_dodge(width=-0.75)) + 
    theme(
    legend.position="none",
    legend.title=element_text(size = 15), 
    legend.text = element_text(size = 12),
    panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
    panel.border = element_rect(fill=NA, colour='black'), 
    axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
    axis.text.y  = element_text(color="black", size=17), 
    axis.title.x = element_text(color="black", size=20),
    axis.title.y = element_text(color="black", size=20)) + 
  geom_hline(yintercept = 0) +
  annotate(geom="text", x=1, y=-0.002, label="10", color="black", size=5) +
  annotate(geom="text", x=2, y=-0.002, label="14", color="black", size=5) +
  annotate(geom="text", x=3, y=-0.002, label="11", color="black", size=5) +
  annotate(geom="text", x=4, y=-0.002, label="15", color="black", size=5) +
  geom_signif(annotations=c("0.035"), y_position=.048, xmin=1.75, xmax=3.75, tip_length = c(0.01, 0.01)) 
  ggsave("Experiment_JuvLobsterOA_Figure_AntennaStiffness.jpg", width=7.87, height=7, dpi=600)
  ggsave("Experiment_JuvLobsterOA_Figure_AntennaStiffness.tiff", width=7.87, height=7, dpi=400)
  





##Graphing all animals // TALK FIGURE

proximal_ei_mean<-aggregate(proximal_ei ~  treatment, EI, FUN = "mean")
names(proximal_ei_mean)[2]<-"mean" #rename distance to mean
proximal_ei_mean[ , "location"] <- c(replicate(4, "proximal")) #repeat proximal in one column
proximal_ei_sd<-aggregate(proximal_ei ~   treatment, EI, FUN = "sd")
proximal_ei_stats<-cbind(proximal_ei_mean,proximal_ei_sd$proximal_ei) #Put mean and sd together in one dataframe
names(proximal_ei_stats)[4]<-"sd" #rename distance to sd

distal_ei_mean<-aggregate(distal_ei ~   treatment, EI, FUN = "mean")
names(distal_ei_mean)[2]<-"mean" #rename distance to mean
distal_ei_mean[ , "location"] <- c(replicate(4, "distal")) #repeat proximal in one column
distal_ei_sd<-aggregate(distal_ei ~   treatment, EI, FUN = "sd")
distal_ei_stats<-cbind(distal_ei_mean,distal_ei_sd$distal_ei) #Put mean and sd together in one dataframe
names(distal_ei_stats)[4]<-"sd" #rename distance to sd

ei_stats<-rbind(distal_ei_stats,proximal_ei_stats)

#getting letter codes for significance
require (agricolae)
anova=aov(proximal_ei~treatment, data=EI_unmolt)
codes<-HSD.test(anova,trt="treatment",  group=T) 
codes 

require(ggplot2)

ggplot(data=ei_stats, aes(x=treatment, y=mean, fill=location)) + 
  theme_bw() + #cheap way of removing gray background. Write over most of its defaults
  geom_bar(stat="identity", position=position_dodge(), colour="black") +
  scale_fill_manual(values=c("#FB7109", "#FFA400"), labels=c("Distal", "Proximal"), name="Location") +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.9)) + 
  scale_y_continuous(name="Flexural stiffness (Nm²)") +
  scale_x_discrete(name="pH Treatment", labels=c("7.97", "7.67", "7.67 ± 0.05","7.67 ± 0.10")) +
  labs(color = "Treatment", shape="Treatment") + 
  theme(legend.position=c(0.55,0.88),
        legend.title=element_text(size = 20), 
        legend.text = element_text(size = 17),
        panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_rect(fill=NA, colour='black'), 
        axis.text.x  = element_text(angle= 45,  hjust=1, color="black", size=17), #x axis treatment labels
        axis.text.y  = element_text(color="black", size=17), 
        axis.title.x = element_text(color="black", size=20),
        axis.title.y = element_text(color="black", size=20)) +
  geom_hline(yintercept = 0) +
  annotate("text",x=0.760,y=.008,label="a", size=6)+
  annotate("text",x=1.760,y=.008,label="a", size=6)+
  annotate("text",x=2.760,y=.008,label="a", size=6)+
  annotate("text",x=3.760,y=.008,label="a", size=6)+
  annotate("text",x=1.225,y=.035,label="bc", size=6)+
  annotate("text",x=2.225,y=.024,label="c", size=6)+
  annotate("text",x=3.225,y=.027,label="bc", size=6)+
  annotate("text",x=4.225,y=.037,label="b", size=6)+
  ggsave("Experiment_JuvLobsterOA_Image_AntennaStiffnessTalk.jpg", width=6, height=6, dpi=800)
