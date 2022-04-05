library(tidyverse)
require(ggpubr)
require(dunn.test)

sem<-read.csv("Data_Thickness.csv")
head(sem)
attach(sem)
names(sem)
length(sem)
nrow(sem)


#Removing samples of poor quality
semgood<-subset(sem, !(Quality %in% c("no")))
nrow(semgood)

#Averaging the measurements per animal, so correct # of replicates
nrow(aggregate(distance ~Lobster, sem, FUN="mean"))
replicatesmean<-aggregate(distance ~ Lobster + layer + region + moltstatus + Treatment, semgood, FUN = "mean") #Just approved quality


##################STATS############################
use<-replicatesmean

#Partitioning
useC<-subset(use,region=="carapace")
useC<-subset(useC,moltstatus=="unmolted") #only unmolted animals; not enough molted for stats
useA<-subset(use,region=="antenna")
useA<-subset(useA,moltstatus=="unmolted")

useCexo<-subset(useC, layer=="exo")
useCendo<-subset(useC, layer=="endo")
useCtotal<-subset(useC, layer=="total")

useAexo<-subset(useA, layer=="exo")
useAendo<-subset(useA, layer=="endo")
useAtotal<-subset(useA, layer=="total")


# Antenna, unmolted animals

shapiro.test(subset(useAtotal,Treatment=="A")$distance) #A is normal
shapiro.test(subset(useAtotal,Treatment=="B")$distance) #B is normal
shapiro.test(subset(useAtotal,Treatment=="C")$distance) #C is normal
shapiro.test(subset(useAtotal,Treatment=="D")$distance) #D is normal
bartlett.test(useAtotal$distance ~ useAtotal$Treatment) #equal
summary(aov(useAtotal$distance ~ useAtotal$Treatment)) #no difference

useAtotal %>% #checking sample size
  group_by(Treatment) %>% 
  dplyr::count()

mean(useAtotal$distance)
sd(useAtotal$distance)

shapiro.test(subset(useAexo,Treatment=="A")$distance) #A is normal
shapiro.test(subset(useAexo,Treatment=="B")$distance) #B is normal
shapiro.test(subset(useAexo,Treatment=="C")$distance) #C is normal
shapiro.test(subset(useAexo,Treatment=="D")$distance) #D is normal
bartlett.test(useAexo$distance, useAexo$Treatment) #equal
summary(aov(useAexo$distance ~ useAexo$Treatment)) #not significant

shapiro.test(subset(useAendo,Treatment=="A")$distance) #A is normal
shapiro.test(subset(useAendo,Treatment=="B")$distance) #B is normal
shapiro.test(subset(useAendo,Treatment=="C")$distance) #C is normal
shapiro.test(subset(useAendo,Treatment=="D")$distance) #D is normal
bartlett.test(useAendo$distance, useAendo$Treatment) #equal
summary(aov(useAendo$distance ~ useAendo$Treatment)) #no difference


# Carapace, unmolted animals
shapiro.test(subset(useCtotal,Treatment=="A")$distance) #A is normal
shapiro.test(subset(useCtotal,Treatment=="B")$distance) #B is normal
shapiro.test(subset(useCtotal,Treatment=="C")$distance) #C is normal
shapiro.test(subset(useCtotal,Treatment=="D")$distance) #D is normal
bartlett.test(useCtotal$distance, useCtotal$Treatment) #equal
summary(aov(useCtotal$distance ~ useCtotal$Treatment)) #no difference

useCtotal %>% #checking sample size
  group_by(Treatment) %>% 
  count()

mean(useCtotal$distance)
sd(useCtotal$distance)

shapiro.test(subset(useCexo,Treatment=="A")$distance) #A is normal
shapiro.test(subset(useCexo,Treatment=="B")$distance) #B is not normal
        hist(subset(useCexo,Treatment=="B")$distance) #unimodal; one outlier
shapiro.test(subset(useCexo,Treatment=="C")$distance) #C is normal
shapiro.test(subset(useCexo,Treatment=="D")$distance) #D is normal
bartlett.test(useCexo$distance, useCexo$Treatment) #equal
summary(aov(useCexo$distance ~ useCexo$Treatment)) #still use anova b/c of robustness; no difference

shapiro.test(subset(useCendo,Treatment=="A")$distance) #A is normal
shapiro.test(subset(useCendo,Treatment=="B")$distance) #B is normal
shapiro.test(subset(useCendo,Treatment=="C")$distance) #C is normal
shapiro.test(subset(useCendo,Treatment=="D")$distance) #D is normal
bartlett.test(useCendo$distance, useCendo$Treatment) #equal
summary(aov(useCendo$distance ~ useCendo$Treatment)) # no difference


##################FIGURES#########################

#Counts for sample size
subset(subset(subset(replicatesmean, layer=="total"), region=="carapace"), moltstatus=="unmolted") %>%
  group_by(Treatment)  %>%
  count()

(carapacetotal<-ggplot(data=replicatesmean %>%
                         subset(region=="carapace")  %>%
                         subset(moltstatus=="unmolted") %>% 
                         subset(layer %in% c("total")) %>% #just total layer
                         mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                         mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                       aes(x=Treatment, y=distance, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("totalA"="#f97537","totalB"="#ed3d83", "totalC"="#17c9b7","totalD"="#0f6110")) + 
    scale_y_continuous(name="Total thickness (µm)", breaks = seq(100, 1000, by = 50)) + 
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_blank(),
          axis.text.y  = element_text(color="black", size=17), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20)) + 
    ggtitle("Carapace") +
    coord_cartesian(ylim=c(400,900)) +
    annotate(geom="text", x=1, y=400, label="12", color="black", size=5) +
    annotate(geom="text", x=2, y=400, label="14", color="black", size=5) +
    annotate(geom="text", x=3, y=400, label="12", color="black", size=5) +
    annotate(geom="text", x=4, y=400, label="14", color="black", size=5) )

#Counts for sample size
subset(subset(subset(replicatesmean, layer=="total"), region=="antenna"), moltstatus=="unmolted") %>%
  group_by(Treatment)  %>%
  count()

(antennatotal<-ggplot(data=replicatesmean %>%
                        subset(region=="antenna")  %>%
                        subset(moltstatus=="unmolted") %>% 
                        subset(layer %in% c("total")) %>% #just total layer
                        mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                        mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                      aes(x=Treatment, y=distance, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_fill_manual(values=c("totalA"="#f97537","totalB"="#ed3d83", "totalC"="#17c9b7","totalD"="#0f6110")) + 
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_y_continuous(name="Total thickness (µm)", breaks = seq(100, 1000, by = 50)) + 
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size= 20),
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_blank(),
          axis.text.y  = element_text(color="black", size=17), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    ggtitle("Antenna") +
    coord_cartesian(ylim=c(400,900)) +
    annotate(geom="text", x=1, y=400, label="12", color="black", size=5) +
    annotate(geom="text", x=2, y=400, label="15", color="black", size=5) +
    annotate(geom="text", x=3, y=400, label="13", color="black", size=5) +
    annotate(geom="text", x=4, y=400, label="15", color="black", size=5) )

(carapacelayer<-ggplot(data=replicatesmean %>%
                         subset(region=="carapace")  %>%
                         subset(moltstatus=="unmolted") %>% 
                         subset(layer %in% c("endo","exo")) %>% #remove total layer
                         mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                         mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                       aes(x=Treatment, y=distance, fill=ID, color=ID)) + 
    theme_bw() + 
    geom_boxplot() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75)) +
    scale_color_manual(values=rep("#000000", times=8)) + 
    scale_fill_manual(values=c("exoA"="#c92f20", "endoA"="#f97537","exoB"="#870362","endoB"="#ed3d83",
                               "exoC"="#4492ba", "endoC"="#17c9b7","exoD"="#003b14","endoD"="#0f6110")) + 
    scale_y_continuous(name="Layer thickness (µm)", breaks = seq(100, 475, by = 50)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=17), 
          axis.title.x = element_blank(),
          axis.title.y = element_text(color="black", size=20)) + 
    coord_cartesian(ylim=c(100,475)) ) 

(antennalayer<-ggplot(data=replicatesmean %>%
                        subset(region=="antenna")  %>%
                        subset(moltstatus=="unmolted") %>% 
                        subset(layer %in% c("endo","exo")) %>% #remove total layer
                        mutate(ID = paste0(layer, Treatment)) %>% #Make column to differentiate layers
                        mutate(Treatment = fct_recode(Treatment,"7.97" = "A",  "7.67" = "B", "7.67\n±0.10" = "D", "7.67\n±0.05" = "C")), 
                      aes(x=Treatment, y=distance, fill=ID, color=ID)) + 
    geom_boxplot() +
    theme_bw() + 
    stat_summary(fun.y = base::mean, geom = "point", shape = 18, size = 2, position=position_dodge(width=0.75) ) +
    scale_color_manual(values=c("exoA"="#000000", "endoA"="#000000","exoB"="#000000","endoB"="#000000",
                                "exoC"="#000000", "endoC"="#000000","exoD"="#000000","endoD"="#000000")) + 
    scale_fill_manual(values=c("exoA"="#c92f20", "endoA"="#f97537","exoB"="#870362","endoB"="#ed3d83",
                               "exoC"="#4492ba", "endoC"="#17c9b7","exoD"="#003b14","endoD"="#0f6110")) + 
    scale_y_continuous(name="Layer thickness (µm)", breaks = seq(100, 475, by = 50)) + #is 550 w/ legend
    labs(color = "Treatment", shape="Treatment") + 
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent'),panel.grid.major=element_blank(), panel.grid.minor = element_blank(), 
          panel.border = element_rect(fill=NA, colour='black'), 
          axis.text.x  = element_text(color="black", size=17), #x axis treatment labels
          axis.text.y  = element_text(color="black", size=17), 
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) + 
    coord_cartesian(ylim=c(100,475)))


ggarrange(carapacetotal, antennatotal, carapacelayer, antennalayer, 
          labels = c("A", "B", "C", "D"),
          font.label=15,
          widths=c(1,1,1),
          ncol = 2, nrow = 2)+
  ggsave("Figure_Thickness.jpg", width=7.49, height=7.49, dpi=600)

dev.off()
