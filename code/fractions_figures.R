# Figures for SSSA presentation

library(ggpubr)
library(tidyverse)

lf_c_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/lf_emmeans_gCm2.csv")
lf_c_means$depth <- as.factor(lf_c_means$depth)
lf_c <- lf_c_means %>% select(-X) %>% 
  mutate(fraction = "lf")

lf_n_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/lf_emmeans_gNm2.csv")
lf_n_means$depth <- as.factor(lf_n_means$depth)
lf_n <- lf_n_means %>% select(-X) %>% 
  mutate(fraction = "lf")

lf_isoC_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/lf_emmeans_isoC.csv")
lf_isoC_means$depth <- as.factor(lf_isoC_means$depth)
lf_isoC <- lf_isoC_means %>% select(-X) %>% 
  mutate(fraction = "lf")

lf_isoN_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/lf_emmeans_isoN.csv")
lf_isoN_means$depth <- as.factor(lf_isoN_means$depth)
lf_isoN <- lf_isoN_means %>% select(-X)%>% 
  mutate(fraction = "lf")

lf_CN_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/lf_emmeans_CN.csv")
lf_CN_means$depth <- as.factor(lf_CN_means$depth)
lf_CN <- lf_CN_means %>% select(-X)%>% 
  mutate(fraction = "lf")

hf_c_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/hf_emmeans_gCm2.csv")
hf_c_means$depth <- as.factor(hf_c_means$depth)
hf_c <- hf_c_means %>% select(-X) %>% 
  mutate(fraction = "hf")

hf_n_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/hf_emmeans_gNm2.csv")
hf_n_means$depth <- as.factor(hf_n_means$depth)
hf_n <- hf_n_means %>% select(-X) %>% 
  mutate(fraction = "hf")

hf_isoC_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/hf_emmeans_isoC.csv")
hf_isoC_means$depth <- as.factor(hf_isoC_means$depth)
hf_isoC <- hf_isoC_means %>% select(-X) %>% 
  mutate(fraction = "hf")

hf_isoN_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/hf_emmeans_isoN.csv")
hf_isoN_means$depth <- as.factor(hf_isoN_means$depth)
hf_isoN <- hf_isoN_means %>% select(-X) %>% 
  mutate(fraction = "hf")

hf_CN_means <- read.csv(file = "../NDFF-fractions/models/output/fractions/hf_emmeans_CN.csv")
hf_CN_means$depth <- as.factor(hf_CN_means$depth)
hf_CN <- hf_CN_means %>% select(-X)%>% 
  mutate(fraction = "hf")

#################
# Merge HF & LF
################

C_means <- lf_c %>% bind_rows(hf_c)
N_means <- lf_n %>% bind_rows(hf_n) 
isoC <- lf_isoC %>% bind_rows(hf_isoC)
isoN <- lf_isoN %>% bind_rows(hf_isoN)
CN <- lf_CN %>% bind_rows(hf_CN)

simple_theme_vertical <- theme(axis.line = element_line(colour = "black"),
                               axis.title.y = element_text(size = 16, face="bold"), 
                               axis.title.x = element_blank(), 
                               axis.text.y = element_text(size=12),
                               axis.text.x = element_text(size=12, face = "bold"),
                               axis.ticks = element_blank(),
                               legend.text = element_text(size = 12),
                               legend.title = element_text(size = 12, face= "bold")) 
  
                               

depth.labs <- c("0-20 cm", "20-40 cm")
names(depth.labs) <- c("0", "20")

frac.labs <- c("POM", "MAOM")
names(frac.labs) <- c("lf", "hf")

plant.labs <- c("Interspace", "L. tridentata")
names(plant.labs) <- c("INSP", "LATR")

##### C stock ######

plot_meanC <- C_means %>% 
  ggplot(aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction), size=.7) +
  geom_path(aes(x = treatment, y = emmean, colour= fraction, linetype= fraction), show.legend = FALSE) +
  facet_grid(depth ~ plant, labeller = labeller(depth = depth.labs, plant = plant.labs)) +
  scale_x_discrete(labels=c(expression(bold(paste(Control  ))), (expression(bold(paste(+CO[2])))))) +
  ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2}))))) +
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#ef8a62","#67a9cf"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))

plot_meanC

ggsave(filename = "../NDFF-fractions/figures/Cstock.png",
       plot=plot_meanC, dpi = 300, width = 6, height = 4, units = "in")

#########
# N stock
#########

plot_meanN <- N_means %>% 
  ggplot(aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),size=.7) +
  geom_path(aes(x = treatment, y = emmean, colour= fraction, linetype= fraction), show.legend = FALSE) +
  facet_grid(depth ~ plant, labeller = labeller(depth = depth.labs, plant = plant.labs)) +
  scale_x_discrete(labels=c(expression(bold(paste(Control  ))), (expression(bold(paste(+CO[2])))))) +
  ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2}))))) +
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#ef8a62","#67a9cf"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))

plot_meanN

ggsave(filename = "../NDFF-fractions/figures/Nstock.png",
       plot=plot_meanN, dpi = 300, width = 6, height = 4, units = "in")

#######
# 13C 
#######

plot_isoC <- isoC %>% 
  ggplot(aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),size=.7) +
  geom_path(aes(x = treatment, y = emmean, colour= fraction, linetype= fraction), show.legend = FALSE) +
  facet_grid(depth ~ plant, labeller = labeller(depth = depth.labs, plant = plant.labs)) +
  scale_x_discrete(labels=c(expression(bold(paste(Control  ))), (expression(bold(paste(+CO[2])))))) +
  ylab(expression(bold(paste(SOC~delta^{13},'C (‰)'))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#ef8a62","#67a9cf"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))

plot_isoC

ggsave(filename = "../NDFF-fractions/figures/isoC.png",
       plot=plot_isoC, dpi = 300, width = 6, height = 4, units = "in")


#######
# isoN
#######

plot_isoN <- isoN %>% 
  ggplot(aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),size=.7) +
  geom_path(aes(x = treatment, y = emmean, colour= fraction, linetype= fraction), show.legend = FALSE) +
  facet_grid(depth ~ plant, labeller = labeller(depth = depth.labs, plant = plant.labs)) +
  scale_x_discrete(labels=c(expression(bold(paste(Control  ))), (expression(bold(paste(+CO[2])))))) +
  ylab(expression(bold(paste(Total~N~delta^{15},'N (‰)'))))+
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#ef8a62","#67a9cf"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))

plot_isoN

ggsave(filename = "../NDFF-fractions/figures/isoN.png",
       plot=plot_isoN, dpi = 300, width = 6, height = 4, units = "in")


######
# CN
######

plot_CN <- CN %>% 
  ggplot(aes(x = treatment, y = emmean, group = fraction)) + 
  geom_point(aes(color=fraction), size=2, shape=19) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour=fraction),size=.7) +
  geom_path(aes(x = treatment, y = emmean, colour= fraction, linetype= fraction), show.legend = FALSE) +
  facet_grid(depth ~ plant, labeller = labeller(depth = depth.labs, plant = plant.labs)) +
  scale_x_discrete(labels=c(expression(bold(paste(Control  ))), (expression(bold(paste(+CO[2])))))) +
  ylab(expression(bold(paste(Soil~C:N)))) +
  scale_colour_manual(name= "Soil Fraction",
                      values = c("#ef8a62","#67a9cf"),
                      labels=c("MAOM","POM")) +
  theme_bw() + simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))

plot_CN

ggsave(filename = "../NDFF-fractions/figures/CN.png",
       plot=plot_CN, dpi = 300, width = 6, height = 4, units = "in")

#######
# INSP v LATR
#######
plant_meanC <- C_means %>% 
  ggplot(aes(x = treatment, y = emmean, group = plant)) + 
  geom_point(aes(color= plant), size=2, shape=19, position = position_dodge(0.3)) +
  geom_linerange(aes(ymin = emmean - SE, ymax = emmean + SE, colour= plant),size=.7, 
                 position = position_dodge(0.3)) +
  facet_grid(depth ~ fraction, labeller = labeller(depth = depth.labs, fraction = frac.labs)) +
  scale_x_discrete(labels=c(expression(bold(paste(Control  ))), (expression(bold(paste(+CO[2])))))) +
  ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2}))))) +
  scale_colour_manual(name= "Cover Type",
                      values = c("#a6611a","#018571"),
                      labels=c("INSP","LATR")) +
  theme_bw() + simple_theme_vertical + 
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))


plant_meanC

ggsave(filename = "../NDFF-fractions/figures/INSP_LATR_C.png",
       plot=plant_meanC, dpi = 300, width = 6, height = 4, units = "in")

