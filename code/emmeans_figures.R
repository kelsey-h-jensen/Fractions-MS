# Figures for SSSA presentation

library(ggpubr)
library(tidyverse)

soc_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/emmeans_gCm2.csv")
soc_means$depth <- as.factor(soc_means$depth)
isoC_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/emmeans_isoC.csv")
isoC_means$depth <- as.factor(isoC_means$depth)

N_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/emmeans_gNm2.csv")
isoN_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/emmeans_isoN.csv")




simple_theme_vertical <- theme(axis.line = element_line(colour = "black"),
                               axis.title.y = element_text(size = 16, face="bold"), 
                               axis.title.x = element_blank(), 
                               axis.text.y = element_text(size=14),
                               axis.text.x = element_text(size=12),
                               axis.ticks = element_blank(),
                               legend.text = element_text(size = 14),
                               legend.title = element_text(size = 14, face= "bold"))

depth.labs <- c("Depth 0-20 cm", "Depth 20-40 cm")
names(depth.labs) <- c("0", "20")

##### Characteristics of fractions

all_isoC <- isoC_means %>% group_by(fraction, depth) %>% 
  summarise_if(is.numeric, mean) %>% 
  ggplot( aes(x = fraction, y = emmean)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),
                 position=position_dodge(0.3), size=1)+
  facet_grid(.~depth,
             labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("MAOM  ", "POM")) +
  ylab(expression(bold(paste(SOC~delta^{13},'C (‰)'))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#6699FF","#CC9900"),
                      labels=c("MAOM","POM")) +
  theme_bw() + theme(legend.position = "none") +simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"))

all_isoC

ggsave(filename = "../NDFF-fractions/figures/avg_isoC.png",
       plot=all_isoC, dpi = 300, width = 4, height = 4, units = "in")


### SOC color by treatment
emmeans_gCm2 <- ggplot(data = soc_means, aes(x = treatment, y = response, group = fraction)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = response - SE, ymax = response + SE, colour=fraction),
                 position=position_dodge(0.3), size=1) +
  geom_path(data = soc_means, aes(x = treatment, y = response, group = fraction, colour= fraction), 
            position=position_dodge(0.3))+
  facet_grid(.~depth,
             labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2})))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#6699FF","#CC9900"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(
    size = 12, face = "bold.italic"))

emmeans_gCm2

ggsave(filename = "../NDFF-fractions/figures/emmeans_LATR_gCm2.png",
       plot=emmeans_gCm2, dpi = 300, width = 6, height = 4, units = "in")

################
# by depth only
################

lf_gCm2 <-soc_means %>% filter (fraction == "hf") %>% 
  ggplot(aes(x = treatment, y = response, group = depth)) + 
  geom_point(aes(color= depth), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = response - SE, ymax = response + SE, colour= depth),
                 position=position_dodge(0.3), size=1) +
  geom_path(aes(x = treatment, y = response, group = depth, colour= depth), 
            position=position_dodge(0.3))+
  #facet_grid(.~depth,
          #   labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2})))))+
 # scale_colour_manual(name= "Soil Fraction",
             #         values = c("black","#808080"),
             #         labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(
    size = 12, face = "bold.italic"))

lf_isoC <- isoC_means %>% filter (fraction == "lf") %>% 
  ggplot(aes(x = treatment, y = emmean, group = depth)) + 
  geom_point(aes(color= depth), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour= depth),
                 position=position_dodge(0.3), size=1) +
  geom_path(aes(x = treatment, y = emmean, group = depth, colour= depth), 
            position=position_dodge(0.3))+
  #facet_grid(.~depth,
  #   labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(SOC~delta^{13},'C (‰)'))))+
  # scale_colour_manual(name= "Soil Fraction",
  #         values = c("black","#808080"),
  #         labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(
    size = 12, face = "bold.italic"))

######################################



emmeans_gNm2 <- ggplot(data = N_means, aes(x = treatment, y = response, group = fraction)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = response - SE, ymax = response + SE, colour=fraction),
                 position=position_dodge(0.3), size=1) +
  geom_path(data = N_means, aes(x = treatment, y = response, group = fraction, colour= fraction), 
            position=position_dodge(0.3))+
  facet_grid(.~depth,
             labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2})))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("black","#808080"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(
    size = 12, face = "bold.italic"))

emmeans_gNm2

ggsave(filename = "../NDFF-fractions/figures/emmeans_LATR_gNm2.png",
       plot=emmeans_gNm2, dpi = 300, width = 6, height = 4, units = "in")


emmeans_isoC <- ggplot(data = isoC_means, aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_path(data = isoC_means, aes(x = treatment, y = emmean, group = fraction, colour= fraction), 
            position=position_dodge(0.3)) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),
                 position=position_dodge(0.3), size=1)+
  facet_grid(.~depth,
             labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(SOC~delta^{13},'C (‰)'))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#6699FF","#CC9900"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"))

emmeans_isoC

ggsave(filename = "../NDFF-fractions/figures/emmeans_LATR_isoC.png",
       plot=emmeans_isoC, dpi = 300, width = 6, height = 4, units = "in")

emmeans_isoN <- ggplot(data = isoN_means, aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_path(data = isoN_means, aes(x = treatment, y = emmean, group = fraction, colour= fraction), 
            position=position_dodge(0.3)) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),
                 position=position_dodge(0.3), size=1)+
  facet_grid(.~depth,
             labeller = labeller(depth = depth.labs)) +
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(Total~N~delta^{15},'N (‰)'))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("black","#808080"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"))

emmeans_isoN

ggsave(filename = "../NDFF-fractions/figures/emmeans_LATR_isoN.png",
       plot=emmeans_isoN, dpi = 300, width = 6, height = 4, units = "in")

###########
### Stocks
###########

Cstock <- read.csv("../NDFF-fractions/models/output/fractions/emmeans_Cstock.csv")

emmeans_Cstock <- ggplot(data = Cstock, aes(x = treatment, y = response, group = fraction)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = response - SE, ymax = response + SE, colour=fraction),
                 position=position_dodge(0.3), size=1) +
  geom_path(data = Cstock, aes(x = treatment, y = response, group = fraction, colour= fraction), 
            position=position_dodge(0.3))+
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2})))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("black","#808080"),
                      labels=c("MAOM","POM")) +
  scale_y_continuous(lim= c(0, 1500))+
  theme_bw() + simple_theme_vertical +
  theme(strip.text.x = element_text(
    size = 12, face = "bold.italic"))

Nstock <- read.csv("../NDFF-fractions/models/output/fractions/emmeans_Nstock.csv")

emmeans_Nstock <- ggplot(data = Nstock, aes(x = treatment, y = response, group = fraction)) + 
  geom_point(aes(color=fraction), position=position_dodge(0.3),
             size=2, shape=19) +
  geom_linerange(aes(ymin = response - SE, ymax = response + SE, colour=fraction),
                 position=position_dodge(0.3), size=1) +
  geom_path(data = Nstock, aes(x = treatment, y = response, group = fraction, colour= fraction), 
            position=position_dodge(0.3))+
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2})))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("black","#808080"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical +
  scale_y_continuous(lim= c(0, 150))+
  theme(strip.text.x = element_text(
    size = 12, face = "bold.italic"))



Cstocks <- Cstock %>% mutate_if(is.numeric, round, 2) %>% 
  ggplot(aes(x=treatment, y=response, fill=fraction)) +
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  geom_text(aes(label = response), position = position_stack(reverse = TRUE), 
            vjust = 5,  size = 4)


barC <- Cstocks + 
  ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2})))))+
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=14),
        axis.title.y=element_text(size=16, face="bold"),
        axis.text.x = element_text(size=14, face="bold", hjust = .5, vjust=0),
        axis.ticks = element_blank(),
        plot.margin = margin(20, 10, 10, 10),
        legend.title = element_blank(),
        legend.text = element_text(size = 14))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1600))+
  scale_fill_manual( values = c("#808080","#D3D3D3"), 
                    labels=c( "MAOM", "POM"),
                    guide = guide_legend(reverse = TRUE))

ggsave(filename = "../NDFF-fractions/figures/barplot_LATR_Cstock.png",
       plot= barC, dpi = 300, width = 6, height = 5, units = "in")



Nstock <- read.csv("../NDFF-fractions/models/output/fractions/emmeans_Nstock.csv")

Nstocks <- Nstock %>% mutate_if(is.numeric, round, 2) %>% 
  ggplot(aes(x=treatment, y=response, fill=fraction)) +
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  geom_text(aes(label = response), position = position_stack(reverse = TRUE), 
            vjust = 4,  size = 4)


barN <- Nstocks + 
  ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2})))))+
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.background=element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size=12),
        axis.title.y=element_text(size=14, face="bold"),
        axis.text.x = element_text(size=12,face="bold", hjust = .5, vjust=0),
        axis.ticks = element_blank(),
        plot.margin = margin(20, 10, 10, 10),
        legend.title = element_blank())+
  scale_y_continuous(expand = c(0,0), limits = c(0, 175))+
  scale_fill_manual( values = c("#808080","#D3D3D3"), 
                     labels=c( "MAOM", "POM"),
                     guide = guide_legend(reverse = TRUE))

ggsave(filename = "../NDFF-fractions/figures/barplot_LATR_Nstock.png",
       plot= barN, dpi = 300, width = 6, height = 5, units = "in")

