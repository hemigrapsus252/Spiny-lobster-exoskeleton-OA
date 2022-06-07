library(plyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(modelr)
library(broom)
library(reshape2)
library(ggpubr)


edx<-read.csv("Data_Composition_EDX.csv")
head(edx)
attach(edx)
names(edx)
sapply(edx, class)

#Cleaning data
#remove rows that aren't data
edx<-subset(edx, measure=="Weight %")

#Removing 7 data points where scan was stopped early (2), area studies was shadowed (2) or Ca values that are too high to likely be a true value (3)
edx<-subset(edx, Status=="good")

#Pulling relevant layers
edx<-subset(edx, layer %in% c("exo", "endo", "core", "outer"))

#Only animals that did not molt during the experiment
edx<-subset(edx, moltstatus=="no")

##Removing data from SEM that returned data inconsistent with other independent measures for this species
edx<-subset(edx, SEM=="Apreo LoVac")

#Reforming the data frame so that all elements are in one column
melt.edx <- melt(edx, id.vars= names(edx)[c(1:6,18:19)])
names(melt.edx) <- c(names(edx)[c(1:6,18:19)], "element", "percent")
melt.edx$percent<-as.numeric(melt.edx$percent)


###STATS###

#Aggregating to one value per lobster (replicate)
melt.edx_reps<-aggregate(percent ~ Treatment + Lobster + layer + region + element, melt.edx, FUN="mean")

use<-melt.edx_reps 

antenna_unmolted<-subset(use, region=="antenna")
carapace_unmolted<-subset(use, region=="carapace")
horn_unmolted<-subset(use, region=="horn")

###### Ca stats ########
##Antenna
antenna_unmolted_ca<-subset(antenna_unmolted, element=="Ca")
antenna_unmolted_ca_exo<-subset(antenna_unmolted_ca, layer=="exo")
antenna_unmolted_ca_endo<-subset(antenna_unmolted_ca, layer=="endo")

shapiro.test(subset(antenna_unmolted_ca_exo, Treatment=="A")$percent) # normal
shapiro.test(subset(antenna_unmolted_ca_exo, Treatment=="B")$percent) # normal
        hist(subset(antenna_unmolted_ca_exo, Treatment=="B")$percent) # has a big spread though
shapiro.test(subset(antenna_unmolted_ca_exo, Treatment=="C")$percent) # normal
shapiro.test(subset(antenna_unmolted_ca_exo, Treatment=="D")$percent) # normal
bartlett.test(percent ~ Treatment, data=antenna_unmolted_ca_exo) # equal
summary(aov(antenna_unmolted_ca_exo$percent ~ antenna_unmolted_ca_exo$Treatment)) # Not different

shapiro.test(subset(antenna_unmolted_ca_endo, Treatment=="A")$percent) # normal
shapiro.test(subset(antenna_unmolted_ca_endo, Treatment=="B")$percent) # normal
shapiro.test(subset(antenna_unmolted_ca_endo, Treatment=="C")$percent) # normal
shapiro.test(subset(antenna_unmolted_ca_endo, Treatment=="D")$percent) # normal
bartlett.test(percent ~ Treatment, data=antenna_unmolted_ca_endo) # equal
summary(aov(antenna_unmolted_ca_endo$percent ~ antenna_unmolted_ca_endo$Treatment)) # Not different

##Carapace
carapace_unmolted_ca<-subset(carapace_unmolted, element=="Ca")
carapace_unmolted_ca_exo<-subset(carapace_unmolted_ca, layer=="exo")
carapace_unmolted_ca_endo<-subset(carapace_unmolted_ca, layer=="endo")

boxplot(carapace_unmolted_ca_exo$percent ~ carapace_unmolted_ca_exo$Treatment)
shapiro.test(subset(carapace_unmolted_ca_exo, Treatment=="A")$percent) # not normal
        hist(subset(carapace_unmolted_ca_exo, Treatment=="A")$percent) # one high outlier
shapiro.test(subset(carapace_unmolted_ca_exo, Treatment=="B")$percent) # not normal; 0.045
        hist(subset(carapace_unmolted_ca_exo, Treatment=="B")$percent) # looks fine
shapiro.test(subset(carapace_unmolted_ca_exo, Treatment=="C")$percent) # normal
shapiro.test(subset(carapace_unmolted_ca_exo, Treatment=="D")$percent) # normal
bartlett.test(percent ~ Treatment, data=carapace_unmolted_ca_exo) #equal
kruskal.test(carapace_unmolted_ca_exo$percent, carapace_unmolted_ca_exo$Treatment)
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(carapace_unmolted_ca_exo$percent ~ carapace_unmolted_ca_exo$Treatment)$residuals) #not normal

boxplot(carapace_unmolted_ca_endo$percent ~ carapace_unmolted_ca_endo$Treatment)
shapiro.test(subset(carapace_unmolted_ca_endo, Treatment=="A")$percent) # normal
shapiro.test(subset(carapace_unmolted_ca_endo, Treatment=="B")$percent) # normal
shapiro.test(subset(carapace_unmolted_ca_endo, Treatment=="C")$percent) # normal
shapiro.test(subset(carapace_unmolted_ca_endo, Treatment=="D")$percent) # normal
bartlett.test(percent ~ Treatment, data=carapace_unmolted_ca_endo)
summary(aov(carapace_unmolted_ca_endo$percent ~ carapace_unmolted_ca_endo$Treatment)) #not different

##Horn
horn_unmolted_ca<-subset(horn_unmolted, element=="Ca")
horn_unmolted_ca_outer<-subset(horn_unmolted_ca, layer=="outer")
horn_unmolted_ca_core<-subset(horn_unmolted_ca, layer=="core")

shapiro.test(subset(horn_unmolted_ca_outer, Treatment=="A")$percent) # not normal
        hist(subset(horn_unmolted_ca_outer, Treatment=="A")$percent) # one high outlier
shapiro.test(subset(horn_unmolted_ca_outer, Treatment=="B")$percent) # normal
shapiro.test(subset(horn_unmolted_ca_outer, Treatment=="C")$percent) # normal
shapiro.test(subset(horn_unmolted_ca_outer, Treatment=="D")$percent) # not normal
        hist(subset(horn_unmolted_ca_outer, Treatment=="D")$percent) # one high outlier; no apparent issue with sample pic
bartlett.test(percent ~ Treatment, data=horn_unmolted_ca_outer) # not equal
kruskal.test(horn_unmolted_ca_outer$percent , horn_unmolted_ca_outer$Treatment) #not different 
mean(horn_unmolted_ca_outer$percent)
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(horn_unmolted_ca_outer$percent ~ horn_unmolted_ca_outer$Treatment)$residuals) # normal
  summary(aov(horn_unmolted_ca_outer$percent ~ horn_unmolted_ca_outer$Treatment)) # not significant


shapiro.test(subset(horn_unmolted_ca_core, Treatment=="A")$percent) # normal
shapiro.test(subset(horn_unmolted_ca_core, Treatment=="B")$percent) # normal
shapiro.test(subset(horn_unmolted_ca_core, Treatment=="C")$percent) # normal
shapiro.test(subset(horn_unmolted_ca_core, Treatment=="D")$percent) # normal
bartlett.test(percent ~ Treatment, data=horn_unmolted_ca_core) #  equal
summary(aov(horn_unmolted_ca_core$percent ~ horn_unmolted_ca_core$Treatment)) #no differences



###### Mg stats ########
##Antenna
antenna_unmolted_mg<-subset(antenna_unmolted, element=="Mg")
antenna_unmolted_mg_exo<-subset(antenna_unmolted_mg, layer=="exo")
antenna_unmolted_mg_endo<-subset(antenna_unmolted_mg, layer=="endo")

shapiro.test(subset(antenna_unmolted_mg_exo, Treatment=="A")$percent) 
shapiro.test(subset(antenna_unmolted_mg_exo, Treatment=="B")$percent)#not normal
  hist(subset(antenna_unmolted_mg_exo, Treatment=="B")$percent) # one low value
shapiro.test(subset(antenna_unmolted_mg_exo, Treatment=="C")$percent) # 
shapiro.test(subset(antenna_unmolted_mg_exo, Treatment=="D")$percent) #
bartlett.test(percent ~ Treatment, data=antenna_unmolted_mg_exo) # equal
kruskal.test(antenna_unmolted_mg_exo$percent, antenna_unmolted_mg_exo$Treatment) #not different 
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(antenna_unmolted_mg_exo$percent ~ antenna_unmolted_mg_exo$Treatment)$residuals) # normal
  summary(aov(antenna_unmolted_mg_exo$percent ~ antenna_unmolted_mg_exo$Treatment)) # not significant

shapiro.test(subset(antenna_unmolted_mg_endo, Treatment=="A")$percent) #not normal
shapiro.test(subset(antenna_unmolted_mg_endo, Treatment=="B")$percent)
shapiro.test(subset(antenna_unmolted_mg_endo, Treatment=="C")$percent) #
shapiro.test(subset(antenna_unmolted_mg_endo, Treatment=="D")$percent) #not normal
bartlett.test(percent ~ Treatment, data=antenna_unmolted_mg_endo) #
kruskal.test(antenna_unmolted_mg_endo$percent, antenna_unmolted_mg_endo$Treatment) # not different
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(antenna_unmolted_mg_endo$percent ~ antenna_unmolted_mg_endo$Treatment)$residuals) # normal
  summary(aov(antenna_unmolted_mg_endo$percent ~ antenna_unmolted_mg_endo$Treatment)) # not significant


##Carapace
carapace_unmolted_mg<-subset(carapace_unmolted, element=="Mg")
carapace_unmolted_mg_exo<-subset(carapace_unmolted_mg, layer=="exo")
carapace_unmolted_mg_endo<-subset(carapace_unmolted_mg, layer=="endo")

shapiro.test(subset(carapace_unmolted_mg_exo, Treatment=="A")$percent)
shapiro.test(subset(carapace_unmolted_mg_exo, Treatment=="B")$percent) 
shapiro.test(subset(carapace_unmolted_mg_exo, Treatment=="C")$percent)
shapiro.test(subset(carapace_unmolted_mg_exo, Treatment=="D")$percent)
bartlett.test(percent ~ Treatment, data=carapace_unmolted_mg_exo) # not equal
oneway.test(carapace_unmolted_mg_exo$percent ~ carapace_unmolted_mg_exo$Treatment) # not diff

shapiro.test(subset(carapace_unmolted_mg_endo, Treatment=="A")$percent) #
shapiro.test(subset(carapace_unmolted_mg_endo, Treatment=="B")$percent) # not normal
        hist(subset(carapace_unmolted_mg_endo, Treatment=="B")$percent) # one low value
shapiro.test(subset(carapace_unmolted_mg_endo, Treatment=="C")$percent) #
shapiro.test(subset(carapace_unmolted_mg_endo, Treatment=="D")$percent) #
bartlett.test(percent ~ Treatment, data=carapace_unmolted_mg_endo)
kruskal.test(carapace_unmolted_mg_endo$percent , carapace_unmolted_mg_endo$Treatment) # 0.73
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(carapace_unmolted_mg_endo$percent ~ carapace_unmolted_mg_endo$Treatment)$residuals) # normal
  summary(aov(carapace_unmolted_mg_endo$percent ~ carapace_unmolted_mg_endo$Treatment)) # not significant


###Horn stats
horn_unmolted_mg<-subset(horn_unmolted, element=="Mg")
horn_unmolted_mg_outer<-subset(horn_unmolted_mg, layer=="outer")
horn_unmolted_mg_core<-subset(horn_unmolted_mg, layer=="core")

shapiro.test(subset(horn_unmolted_mg_outer, Treatment=="A")$percent) # 
shapiro.test(subset(horn_unmolted_mg_outer, Treatment=="B")$percent) #
shapiro.test(subset(horn_unmolted_mg_outer, Treatment=="C")$percent) #
shapiro.test(subset(horn_unmolted_mg_outer, Treatment=="D")$percent) # not normal
        hist(subset(horn_unmolted_mg_outer, Treatment=="D")$percent) # one high one
bartlett.test(percent ~ Treatment, data=horn_unmolted_mg_outer) # not equal
kruskal.test(horn_unmolted_mg_outer$percent , horn_unmolted_mg_outer$Treatment) # not different
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(horn_unmolted_mg_outer$percent ~ horn_unmolted_mg_outer$Treatment)$residuals) #not normal

shapiro.test(subset(horn_unmolted_mg_core, Treatment=="A")$percent) # 
shapiro.test(subset(horn_unmolted_mg_core, Treatment=="B")$percent) # 
shapiro.test(subset(horn_unmolted_mg_core, Treatment=="C")$percent) # not normal
shapiro.test(subset(horn_unmolted_mg_core, Treatment=="D")$percent) # 
bartlett.test(percent ~ Treatment, data=horn_unmolted_mg_core) #  
kruskal.test(horn_unmolted_mg_core$percent, horn_unmolted_mg_core$Treatment) # not different
  #Testing normality of residuals if an anova were used
  shapiro.test(aov(horn_unmolted_mg_core$percent ~ horn_unmolted_mg_core$Treatment)$residuals) # normal
  summary(aov(horn_unmolted_mg_core$percent ~ horn_unmolted_mg_core$Treatment)) # not significant


#Summary stats in manuscript--elements averaged over the layers, as some other research does
aggregate(percent ~ Treatment + region, data=subset(use, element=="Ca"), FUN="mean")
aggregate(percent ~ Treatment + region, data=subset(use, element=="Ca"), FUN="sd")

aggregate(percent ~ Treatment + region + layer, data=subset(use, element=="Mg"), FUN="mean")
aggregate(percent ~ Treatment + region, data=subset(use, element=="Mg"), FUN="sd")


####### FIGURES ####
subset(subset(use, region=="carapace"), element=="Ca") %>%
  group_by(Treatment, layer)%>%
  dplyr::count()

(carapaceCa<-ggplot(data=(use %>%
                            subset(region=="carapace") %>%
                            subset(element=="Ca") %>% 
                            mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                            mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C"))), 
                    aes(x=Treatment, y=percent, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("exoA"="#c92f20", "endoA"="#f97537","exoB"="#870362","endoB"="#ed3d83",
                               "exoC"="#4492ba", "endoC"="#17c9b7","exoD"="#003b14","endoD"="#0f6110")) + 
    scale_y_continuous(name="Ca (% wt)", 
                       breaks = seq(0, 50, by = 10)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_blank(), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=15), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20)) + 
    ggtitle("Carapace") +
    coord_cartesian(ylim=c(0,50)) +
    annotate(geom="text", x=0.75, y=50, label="A", color="black", size=6))

subset(subset(use, region=="antenna"), element=="Ca") %>%
  group_by(Treatment, layer)%>%
  dplyr::count()

(antennaeCa<-ggplot(data=use %>%
                      subset(region=="antenna")  %>%
                      subset(element=="Ca") %>% 
                      mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                      mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                    aes(x=Treatment, y=percent, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("exoA"="#c92f20", "endoA"="#f97537","exoB"="#870362","endoB"="#ed3d83",
                               "exoC"="#4492ba", "endoC"="#17c9b7","exoD"="#003b14","endoD"="#0f6110")) + 
    scale_y_continuous(breaks = seq(0, 50, by = 10)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_blank(), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=12), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    ggtitle("Antenna") +
    annotate(geom="text", x=0.75, y=50, label="B", color="black", size=6) +
    coord_cartesian(ylim=c(0,50)))

subset(subset(use, region=="horn"), element=="Ca") %>%
  group_by(Treatment, layer)%>%
  dplyr::count()

(hornCa<-ggplot(data=use %>%
                  subset(region=="horn")  %>%
                  subset(element=="Ca") %>% 
                  mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                  mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                aes(x=Treatment, y=percent, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("outerA"="#c92f20", "coreA"="#f97537","outerB"="#870362","coreB"="#ed3d83",
                               "outerC"="#4492ba", "coreC"="#17c9b7","outerD"="#003b14","coreD"="#0f6110")) + 
    scale_y_continuous(breaks = seq(0, 50, by = 10)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_blank(), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=15), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    ggtitle("Horn") +
    annotate(geom="text", x=0.75, y=50, label="C", color="black", size=6) +
    coord_cartesian(ylim=c(0,50)))


(carapaceMg<-ggplot(data=use %>%
                      subset(region=="carapace")  %>%
                      subset(element=="Mg") %>% 
                      mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                      mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                    aes(x=Treatment, y=percent, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("exoA"="#c92f20", "endoA"="#f97537","exoB"="#870362","endoB"="#ed3d83",
                               "exoC"="#4492ba", "endoC"="#17c9b7","exoD"="#003b14","endoD"="#0f6110")) + 
    scale_y_continuous(name="Mg (% wt)", 
                       breaks = seq(0, 3, by = 0.5)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_text(color="black", size=12), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=15), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20)) + 
    coord_cartesian(ylim=c(0,2.3)) +
    annotate(geom="text", x=0.75, y=2.3, label="D", color="black", size=6) +
    annotate(geom="text", x=1, y=0.002, label="6", color="black", size=4) +
    annotate(geom="text", x=2, y=0.002, label="7", color="black", size=4) +
    annotate(geom="text", x=3, y=0.002, label="6", color="black", size=4) +
    annotate(geom="text", x=4, y=0.002, label="9", color="black", size=4))


(antennaMg<-ggplot(data=use %>%
                     subset(region=="antenna")  %>%
                     subset(element=="Mg") %>% 
                     mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                     mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                   aes(x=Treatment, y=percent, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("exoA"="#c92f20", "endoA"="#f97537","exoB"="#870362","endoB"="#ed3d83",
                               "exoC"="#4492ba", "endoC"="#17c9b7","exoD"="#003b14","endoD"="#0f6110")) + 
    scale_y_continuous(breaks = seq(0, 2.5, by = 0.5)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_text(color="black", size=12), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=15), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    coord_cartesian(ylim=c(0,2.3)) +
    annotate(geom="text", x=0.75, y=2.3, label="E", color="black", size=6) +
    annotate(geom="text", x=1, y=0.002, label="6", color="black", size=4) +
    annotate(geom="text", x=2, y=0.002, label="7", color="black", size=4) +
    annotate(geom="text", x=3, y=0.002, label="7", color="black", size=4) +
    annotate(geom="text", x=4, y=0.002, label="9", color="black", size=4))


(hornMg<-ggplot(data=use %>%
                  subset(region=="horn")  %>%
                  subset(element=="Mg") %>% 
                  mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                  mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                aes(x=Treatment, y=percent, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("outerA"="#c92f20", "coreA"="#f97537","outerB"="#870362","coreB"="#ed3d83",
                               "outerC"="#4492ba", "coreC"="#17c9b7","outerD"="#003b14","coreD"="#0f6110")) + 
    scale_y_continuous(breaks = seq(0, 2.5, by = 0.5)) + 
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_text(color="black", size=12), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=15), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    coord_cartesian(ylim=c(0,2.3)) +
    annotate(geom="text", x=0.75, y=2.3, label="F", color="black", size=6) +
    annotate(geom="text", x=1, y=0.002, label="5", color="black", size=4) +
    annotate(geom="text", x=2, y=0.002, label="6", color="black", size=4) +
    annotate(geom="text", x=3, y=0.002, label="6", color="black", size=4) +
    annotate(geom="text", x=4, y=0.002, label="7", color="black", size=4))


ggarrange(carapaceCa, antennaeCa, hornCa, carapaceMg, antennaMg, hornMg, 
          widths=c(1.1,1,1),
          ncol = 3, nrow = 2)+
  ggsave("Figure_EDX.jpg", width=7.49, height=7.49, dpi=600)

dev.off()


