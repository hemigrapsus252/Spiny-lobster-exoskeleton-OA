library(ggsignif)
library(ggplot2)
library(dunn.test)
library(ggpubr) # for ggarrange
library(forcats)
library(dplyr)

icpms<-read.csv("Data_Composition_ICPMS.csv")
head(icpms)
attach(icpms)
names(icpms)


# Abdominal segment (As)-------------------------------------------------------

as<-subset(icpms, Region=='Abdominal segment')

#separating out molted and unmolted
as_molt<-subset(as, molt_status=="yes") #splitting out molted and unmolted animals #did not study small number of molted animals
as_unmolt<-subset(as, molt_status=="no")


#Calcium content in unmolted lobster abdominal segement cuticle
ggplot(data=as_unmolt, aes(x=treatment, y=Ca)) + 
  geom_boxplot() + 
  geom_point()
shapiro.test(subset(as_unmolt, treatment=="A")$Ca)
shapiro.test(subset(as_unmolt, treatment=="B")$Ca)
shapiro.test(subset(as_unmolt, treatment=="C")$Ca)
shapiro.test(subset(as_unmolt, treatment=="D")$Ca) #all 4 normal
bartlett.test(Ca~treatment, data=as_unmolt) #equal variances
summary(aov(Ca~treatment, data=as_unmolt)) #not different; p=0.10

mean(as_unmolt$Ca)
sd(as_unmolt$Ca)

#Unmolted Mg
ggplot(data=as_unmolt, aes(x=treatment, y=Mg)) + 
  geom_boxplot() + 
  geom_point()
shapiro.test(subset(as_unmolt, treatment=="A")$Mg)
shapiro.test(subset(as_unmolt, treatment=="B")$Mg)
shapiro.test(subset(as_unmolt, treatment=="C")$Mg)
shapiro.test(subset(as_unmolt, treatment=="D")$Mg) #all 4 normal
bartlett.test(Mg~treatment, data=as_unmolt) #equal variances
summary(aov(Mg~treatment, data=as_unmolt)) #not different

mean(as_unmolt$Mg)
sd(as_unmolt$Mg)

# Carapace (Dorsal spine = Dsp) ----------------------------------------------------------------

dsp<-subset(icpms, Region=="Carapace")

#separating out molted and unmolted
dsp_molt<-subset(dsp, molt_status=="yes")
dsp_unmolt<-subset(dsp, molt_status=="no")

#Unmolted Ca
ggplot(data=dsp_unmolt, aes(x=treatment, y=Ca)) + 
  geom_boxplot() + 
  geom_point()
shapiro.test(subset(dsp_unmolt, treatment=="A")$Ca)
shapiro.test(subset(dsp_unmolt, treatment=="B")$Ca)
shapiro.test(subset(dsp_unmolt, treatment=="C")$Ca)
shapiro.test(subset(dsp_unmolt, treatment=="D")$Ca) #All normal
bartlett.test(Ca~treatment, data=dsp_unmolt) #equal variances
summary(aov(Ca~treatment, data=dsp_unmolt)) #sig different
TukeyHSD(aov(Ca~treatment, data=dsp_unmolt)) #D sig different from A and B

#Unmolted Mg
ggplot(data=dsp_unmolt, aes(x=treatment, y=Mg)) + 
  geom_boxplot() + 
  geom_point()
shapiro.test(subset(dsp_unmolt, treatment=="A")$Mg) # not normal; one high value, but no notes about sample being unusual
shapiro.test(subset(dsp_unmolt, treatment=="B")$Mg)
shapiro.test(subset(dsp_unmolt, treatment=="C")$Mg)
shapiro.test(subset(dsp_unmolt, treatment=="C")$Mg) #rest normal
bartlett.test(Mg~treatment, data=dsp_unmolt) #equal variances
kruskal.test(Mg, treatment, data=dsp_unmolt)  #sig different
dunn.test(dsp_unmolt$Mg, dsp_unmolt$treatment, list=TRUE) 
#Without bonferroni: A and B different slightly; C and & D different
#With bonf / strong effects: , A and C + D different; B & D different



# Graphing ----------------------------------------------------------------

as_unmolt <- as_unmolt %>%
  mutate(treatment = fct_recode(treatment,"7.97" = "A","7.67" = "B","7.67\n± 0.05" = "C","7.67\n± 0.10" = "D"))
unique(as_unmolt$treatment)

dsp_unmolt <- dsp_unmolt %>%
  mutate(treatment = fct_recode(treatment,"7.97" = "A","7.67" = "B","7.67\n± 0.05" = "C","7.67\n± 0.10" = "D"))
unique(dsp_unmolt$treatment)

dsp_unmolt %>%
  group_by(treatment)  %>%
  count()


(DspCa<-ggplot(data=dsp_unmolt, aes(x = treatment, y=Ca, fill=treatment)) + 
        geom_boxplot(color="black", outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    scale_y_continuous(name="[Ca]\n(micromol element/mg sample) ") +
    theme_bw() +
    coord_cartesian(ylim=c(4, 7.15)) + #Forces limits
    scale_x_discrete(name="Treatment")+
    scale_fill_manual(values =c("7.97"= "#c92f20", "7.67" = "#870362", "7.67\n± 0.05" = "#4492ba", "7.67\n± 0.10" ="#003b14")) + 
    ggtitle("Carapace") + 
    theme(
      plot.title = element_text(size=12,hjust = 0.5),
      legend.position="none", #removes legend,
      panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
      panel.border = element_rect(fill=NA, colour='black'), 
      axis.text.x  = element_text(color="black", size=10), 
      axis.text.y  = element_text(color="black", size=10), 
      axis.title.x = element_blank(),
      axis.title.y = element_text(color="black", size=12))+
    geom_signif(annotations=c("0.01"), textsize=3, y_position=7.05, xmin=1, xmax=4, tip_length = c(0.02, 0.02)) +
    geom_signif(annotations=c("0.04"), textsize=3, y_position=6.85, xmin=2, xmax=4, tip_length = c(0.02, 0.02)) 
)

(DspMg<-ggplot(data=dsp_unmolt, aes(x = treatment, y=Mg, fill=treatment)) + 
    scale_y_continuous(name="[Mg]\n(micromol element/mg sample) ") +
    theme_bw() +
    geom_boxplot(color="black", outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    coord_cartesian(ylim=c(0.35, 0.68)) + #Forces limits
    scale_fill_manual(values =c("7.97"= "#c92f20", "7.67" = "#870362", "7.67\n± 0.05" = "#4492ba", "7.67\n± 0.10" ="#003b14")) + 
    theme(
      legend.position="none", #removes legend,
      panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
      panel.border = element_rect(fill=NA, colour='black'), 
      axis.text.x  = element_text(color="black", size=10), 
      axis.text.y  = element_text(color="black", size=10), 
      axis.title.x = element_blank(),
      axis.title.y = element_text(color="black", size=12))+
    geom_signif(annotations=c("0.047"), textsize=3, y_position=0.62, xmin=1, xmax=2, tip_length = c(0.02, 0.02)) +
    geom_signif(annotations=c("<0.001"), textsize=3, y_position=0.65, xmin=1, xmax=4, tip_length = c(0.02, 0.02)) +
    geom_signif(annotations=c("0.010"), textsize=3, y_position=0.59, xmin=1, xmax=3, tip_length = c(0.02, 0.02)) +
    geom_signif(annotations=c("0.005"), textsize=3, y_position=0.56, xmin=2, xmax=4, tip_length = c(0.02, 0.02)) +
    geom_signif(annotations=c("0.042"), textsize=3, y_position=0.53, xmin=3, xmax=4, tip_length = c(0.02, 0.02)) +
    annotate(geom="text", x=1, y=0.35, label="12", color="black", size=3) +
    annotate(geom="text", x=2, y=0.35, label="15", color="black", size=3) +
    annotate(geom="text", x=3, y=0.35, label="13", color="black", size=3) +
    annotate(geom="text", x=4, y=0.35, label="15", color="black", size=3)
)

as_unmolt %>%
  group_by(treatment)  %>%
  count()

(AsCa<-ggplot(data=as_unmolt, aes(x = treatment, y=Ca, fill=treatment)) + 
    geom_boxplot(color="black", outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    scale_y_continuous(name="Mean Ca (micromol element/mg sample) ",breaks=seq(0,10,by = .5)) +
    theme_bw() +
    coord_cartesian(ylim=c(4, 7.15)) + #Forces limits
    scale_x_discrete(name="Treatment")+
    scale_fill_manual(values =c("7.97"= "#f97537", "7.67" = "#ed3d83", "7.67\n± 0.05" = "#17c9b7", "7.67\n± 0.10" ="#0f6110")) + 
    ggtitle("Abdominal segment") +
    theme(
      plot.title = element_text(size=12,hjust = 0.5),
      legend.position="none", #removes legend,
      panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
      panel.border = element_rect(fill=NA, colour='black'), 
      axis.text.x  = element_text(color="black", size=10), 
      axis.text.y  = element_text(color="black", size=10), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank())
)

(AsMg<-ggplot(data=as_unmolt, aes(x = treatment, y=Mg, fill=treatment)) + 
    geom_boxplot(color="black", outlier.colour="black", outlier.shape=16, outlier.size=1, notch=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    coord_cartesian(ylim=c(0.35, 0.68)) + #Forces limits
    theme_bw() +
    scale_fill_manual(values =c("7.97"= "#f97537", "7.67" = "#ed3d83", "7.67\n± 0.05" = "#17c9b7", "7.67\n± 0.10" ="#0f6110")) + 
    theme(
      legend.position="none", #removes legend,
      panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
      panel.border = element_rect(fill=NA, colour='black'), 
      axis.text.x  = element_text(color="black", size=10), 
      axis.text.y  = element_text(color="black", size=10), 
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    annotate(geom="text", x=1, y=0.35, label="12", color="black", size=3) +
    annotate(geom="text", x=2, y=0.35, label="15", color="black", size=3) +
    annotate(geom="text", x=3, y=0.35, label="13", color="black", size=3) +
    annotate(geom="text", x=4, y=0.35, label="15", color="black", size=3)
)


ggarrange(DspCa, AsCa, DspMg, AsMg, 
          labels = c("A", "B", "C", "D"),
          font.label=15,
          widths=c(1.2,1),
          ncol = 2, nrow = 2)+
  ggsave("Figure_Composition_ICPMS.jpg", width=4.75, height=4.75, dpi=600)

dev.off()



