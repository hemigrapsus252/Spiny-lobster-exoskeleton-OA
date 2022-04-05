require(plyr) #for ddplyr function for graph summary stats
require(dunn.test)
require(tidyverse) #for count function
require(userfriendlyscience) #games-howell posthoc; no longer available
require (ggplot2)
library(ggsignif) #for figure with lines of significance crossing trts
require(gridExtra) # for putting 4 plots together
require(cowplot) # for putting 4 plots together


nano<-read.csv("Data_MaterialProperties.csv")
head(nano)
attach(nano)
names(nano)

###############
###Hardness####
###############

boxplot(Hardness~Treatment, data=nano) #can see many outliers, all on the high en

#Dropping measurements that are outside interquartile ranges (outliers)/ outside the Tukey fence
hard<-aggregate(Hardness ~ Animal + Region + Molted. + Treatment , subset(nano, HIQR=="yes"), FUN = "mean")

hard %>% #checking sample size
  group_by(Treatment) %>% 
  count()

#Separating into regions and molt status 
HardDspUnmolted<-subset(subset(hard, Region=="Dsp"), Molted.=="No")
HardHornUnmolted<-subset(subset(hard, Region=="Horn"), Molted.=="No")

#######################  Horn spine stats  ####################################

boxplot(Hardness~Treatment, data=HardHornUnmolted)

HardHornUnmolted %>%
  group_by(Treatment)%>%
  mean(as.numeric(Hardness))

temporary<-HardHornUnmolted %>%
  filter(Molted. == "No")%>%
  select(Region, Treatment, Hardness) %>%
  group_by(Region, Treatment) %>%
  summarise_all(.funs=c(mean="mean", sd="sd"))

str(as.numeric(HardHornUnmolted$Hardness))

shapiro.test(subset(HardHornUnmolted,Treatment=="A")$Hardness) 
shapiro.test(subset(HardHornUnmolted,Treatment=="B")$Hardness) 
shapiro.test(subset(HardHornUnmolted,Treatment=="C")$Hardness) 
shapiro.test(subset(HardHornUnmolted,Treatment=="D")$Hardness) 
bartlett.test(HardHornUnmolted$Hardness~HardHornUnmolted$Treatment) 
summary(aov(HardHornUnmolted$Hardness~HardHornUnmolted$Treatment))
TukeyHSD(aov(HardHornUnmolted$Hardness~HardHornUnmolted$Treatment))


 #######################  Carapace (Dorsal spine) stats  ####################################

boxplot(Hardness~Treatment, data=HardDspUnmolted)

shapiro.test(subset(HardDspUnmolted,Treatment=="A")$Hardness) 
shapiro.test(subset(HardDspUnmolted,Treatment=="B")$Hardness)
shapiro.test(subset(HardDspUnmolted,Treatment=="C")$Hardness) # 
shapiro.test(subset(HardDspUnmolted,Treatment=="D")$Hardness) # 
bartlett.test(HardDspUnmolted$Hardness~HardDspUnmolted$Treatment) 
kruskal.test(HardDspUnmolted$Hardness~HardDspUnmolted$Treatment) 
summary(aov(HardDspUnmolted$Hardness~HardDspUnmolted$Treatment))


################
###stiffness####
################

#Dropping measurements that are outside interquartile ranges (outliers)/ outside the Tukey fence
stiffIQR<-aggregate(Stiffness ~ Animal + Region + Molted. + Treatment , subset(nano, SIQR=="yes"), FUN = "mean")
stiff<-stiffIQR 

#Separating into regions and molt status 
StiffDspUnmolted<-subset(subset(stiff, Region=="Dsp"), Molted.=="No")
StiffHornUnmolted<-subset(subset(stiff, Region=="Horn"), Molted.=="No")

#######################  Horn spine stats  ####################################

boxplot(Stiffness~Treatment, data=StiffHornUnmolted) #looks great except for that weird horn tip

shapiro.test(subset(StiffHornUnmolted,Treatment=="A")$Stiffness) 
shapiro.test(subset(StiffHornUnmolted,Treatment=="B")$Stiffness) 
shapiro.test(subset(StiffHornUnmolted,Treatment=="C")$Stiffness) 
shapiro.test(subset(StiffHornUnmolted,Treatment=="D")$Stiffness) 
bartlett.test(StiffHornUnmolted$Stiffness~StiffHornUnmolted$Treatment) 
summary(aov(StiffHornUnmolted$Stiffness~StiffHornUnmolted$Treatment)) 

#global mean
aggregate(Stiffness ~ Region + Molted. , StiffHornUnmolted, FUN = "mean")
aggregate(Stiffness ~ Region + Molted. , StiffHornUnmolted, FUN = "sd")

aggregate(Stiffness ~ Region + Molted. + Treatment , StiffHornUnmolted, FUN = "mean")

#######################  Dorsal spine stats  ####################################

boxplot(Stiffness~Treatment, data=StiffDspUnmolted)

shapiro.test(subset(StiffDspUnmolted,Treatment=="A")$Stiffness) 
shapiro.test(subset(StiffDspUnmolted,Treatment=="B")$Stiffness)
shapiro.test(subset(StiffDspUnmolted,Treatment=="C")$Stiffness) # 
shapiro.test(subset(StiffDspUnmolted,Treatment=="D")$Stiffness) # 
bartlett.test(StiffDspUnmolted$Stiffness~StiffDspUnmolted$Treatment) 
summary(aov(StiffDspUnmolted$Stiffness~StiffDspUnmolted$Treatment)) 


###############
###Graphing####
###############


hard %>%
  filter(Molted. == "No")%>%
  select(Region, Treatment, Hardness) %>%
  group_by(Region, Treatment) %>%
  summarise_all(.funs=c(mean="mean", sd="sd"))

hardness_stats<-ddply(hard,~Treatment + Region,summarise,mean=mean(Hardness),sd=sd(Hardness)) #computes the mean thickness per treatment
hardness_stats <- hardness_stats %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Region = fct_recode(Region,"Carapace spine" = "Dsp"))
hardness_stats <- hardness_stats %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67 ±0.10" = "D", "7.67 ±0.05" = "C"))
levels(hardness_stats$Treatment)<-gsub(" ", "\n",levels(hardness_stats$Treatment))

hardness_stats_dsp<-subset(hardness_stats, Region=="Carapace spine")

HardDspUnmolted %>% #How many replicates I have of each region for my graphing
  select(Region, Treatment, Hardness) %>% 
  group_by(Treatment)%>% 
  dplyr::count()

harddsp<-ggplot(hardness_stats_dsp, aes(x=Treatment, y=mean, fill=Treatment)) + 
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.9)) +
  scale_fill_manual(values=c("#c92f20", "#870362", "#4492ba",  "#003b14")) +
  scale_y_continuous(name="Hardness (GPa)", breaks =seq(0,0.7,0.1)) +
  scale_x_discrete(name="pH") +
  expand_limits(y = c(0, 0.55)) + 
  theme(legend.position="none",
        legend.title=element_blank(), panel.background = element_rect(fill='transparent'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border =element_rect(fill=NA, colour='black'), 
        plot.title = element_text(size = 20, hjust= 0.5),
        axis.title.y = element_text(color="black", size=20),
        axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
        axis.text.y  = element_text(color="black", size=17), #y axis number labels
        axis.title.x = element_blank()) +
  geom_hline(yintercept = 0) +
  ggtitle("Carapace") +
  annotate(geom="text", x=0.65, y=.65, label="A", color="black", size=8) +
  annotate(geom="text", x=1, y=-0.02, label="10", color="black", size=5) +
  annotate(geom="text", x=2, y=-0.02, label="11", color="black", size=5) +
  annotate(geom="text", x=3, y=-0.02, label="9", color="black", size=5) +
  annotate(geom="text", x=4, y=-0.02, label="11", color="black", size=5)

hardness_stats_horn<-subset(hardness_stats, Region=="Horn")

HardHornUnmolted%>% #How many replicates I have of each region for my graphing
  group_by(Treatment) %>% 
  dplyr::count()

hardhorn<-ggplot(hardness_stats_horn, aes(x=Treatment, y=mean, fill=Treatment)) + 
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.9)) +
  scale_fill_manual(values=c("#f97537", "#ed3d83", "#17c9b7",  "#0f6110")) +
  scale_y_continuous(name="Hardness (GPa)", breaks =seq(0,0.7,0.1)) +
  scale_x_discrete(name="pH") +
  expand_limits(y = c(0, 0.55)) + 
  theme(legend.position="none",
        legend.title=element_blank(), panel.background = element_rect(fill='transparent'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border =element_rect(fill=NA, colour='black'), 
        plot.title = element_text(hjust = 0.5, size= 20),
        axis.title.y = element_blank(),
        axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
        axis.text.y  = element_text(color="black", size=17), #y axis number labels
        axis.title.x = element_blank()) +
  geom_hline(yintercept = 0) +
  annotate(geom="text", x=0.65, y=.65, label="B", color="black", size=8) +
  ggtitle("Horn") +
  annotate(geom="text", x=1, y=-0.02, label="10", color="black", size=5) +
  annotate(geom="text", x=2, y=-0.02, label="10", color="black", size=5) +
  annotate(geom="text", x=3, y=-0.02, label="9", color="black", size=5) +
  annotate(geom="text", x=4, y=-0.02, label="11", color="black", size=5) +
  geom_signif(annotations=c("0.004"), y_position=.50, xmin=2, xmax=3, tip_length = c(0.01, 0.01)) +
  geom_signif(annotations=c("0.045"), y_position=.55, xmin=1, xmax=4, tip_length = c(0.01, 0.01)) +
  geom_signif(annotations=c("0.003"), y_position=.45, xmin=2, xmax=4, tip_length = c(0.01, 0.01))

   # ----------------------

stiffness_stats<-ddply(stiff,~Treatment + Region,summarise,mean=mean(Stiffness),sd=sd(Stiffness)) #computes the mean thickness per treatment
stiffness_stats <- stiffness_stats %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Region = fct_recode(Region,"Carapace spine" = "Dsp"))

stiffness_stats <- stiffness_stats %>% #This is changing the names from my defaults to what I want on the graphs
  mutate(Treatment = fct_recode(Treatment,"7.97" = "A", "7.67" = "B",  "7.67 ±0.10" = "D", "7.67 ±0.05" = "C"))
levels(stiffness_stats$Treatment)<-gsub(" ", "\n",levels(stiffness_stats$Treatment))

stiff_stats_dsp<-subset(stiffness_stats, Region=="Carapace spine")

StiffDspUnmolted%>% #How many replicates I have of each region for my graphing
  group_by(Treatment) %>% 
  dplyr::count()

stiffdsp<-ggplot(stiff_stats_dsp, aes(x=Treatment, y=mean, fill=Treatment)) + 
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.9)) +
  scale_fill_manual(values=c("#c92f20", "#870362", "#4492ba",  "#003b14")) +
  scale_y_continuous(name="Stiffness (GPa)", breaks =seq(0,22,2)) +
  scale_x_discrete(name="pH") +
  theme(legend.position="none",
        legend.title=element_blank(), panel.background = element_rect(fill='transparent'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border =element_rect(fill=NA, colour='black'), 
        axis.title.y = element_text(color="black", size=20),
        axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
        axis.text.y  = element_text(color="black", size=17), #y axis number labels
        axis.title.x = element_text(color="black", size=20)) +
  geom_hline(yintercept = 0) +
  annotate(geom="text", x=0.65, y=22, label="C", color="black", size=8) +
  annotate(geom="text", x=1, y=-0.8, label="10", color="black", size=5) +
  annotate(geom="text", x=2, y=-0.8, label="11", color="black", size=5) +
  annotate(geom="text", x=3, y=-0.8, label="9", color="black", size=5) +
  annotate(geom="text", x=4, y=-0.8, label="11", color="black", size=5)
stiff_stats_horn<-subset(stiffness_stats, Region=="Horn")

StiffHornUnmolted%>% #How many replicates I have of each region for my graphing
  group_by(Treatment) %>% 
  dplyr::count()

stiffhorn<-ggplot(stiff_stats_horn, aes(x=Treatment, y=mean, fill=Treatment)) + 
  theme_bw() +
  geom_bar(stat="identity", position=position_dodge(1)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,position=position_dodge(0.9)) +
  scale_fill_manual(values=c("#f97537", "#ed3d83", "#17c9b7",  "#0f6110")) +
  scale_y_continuous(breaks=seq(0,22,2)) +
  scale_x_discrete(name="pH") +
  theme(legend.position="none",
        legend.title=element_blank(), panel.background = element_rect(fill='transparent'), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border =element_rect(fill=NA, colour='black'), 
        axis.title.y = element_blank(),
        axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
        axis.text.y  = element_text(color="black", size=17), #y axis number labels
        axis.title.x = element_text(color="black", size=20)) +
  geom_hline(yintercept = 0) +
  annotate(geom="text", x=0.65, y=22, label="D", color="black", size=8) +
  annotate(geom="text", x=1, y=-0.8, label="10", color="black", size=5) +
  annotate(geom="text", x=2, y=-0.8, label="10", color="black", size=5) +
  annotate(geom="text", x=3, y=-0.8, label="9", color="black", size=5) +
  annotate(geom="text", x=4, y=-0.8, label="11", color="black", size=5)


tiff(filename="Figure_MaterialProperties.tif",
     width = 5000, height = 4500, #adjusted by hand until it looks right
     res=500) #dpi
plot_grid(harddsp, hardhorn, stiffdsp, stiffhorn,  nrow=2, rel_widths = c(1.05,1), align = 'v', axis = 'l') #align and axis fixes issue         
dev.off()
