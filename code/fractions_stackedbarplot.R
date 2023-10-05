# Stacked bar plots
library(tidyverse)
library(ggpubr)
library(cowplot)


load("../NDFF-fractions/data/all_fractions_long_avgbd.RData")
fractions_long$depth <- as.factor(fractions_long$depth)
fractions_long$ring <- as.factor(fractions_long$ring)


#### ggbarplot for C stock
# Not quite right, giving the same means for each group
C_data1 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gCm2) %>%
  filter(grepl('LATR', plant)) %>% 
  filter(!grepl('20|40|60|80', depth)) %>% 
  filter(!grepl("organic", fraction))

p1 <- ggbarplot( C_data1, x = "treatment", y = "gCm2", add = "mean_se",
  fill = "fraction") + ggtitle("L. tridentata", subtitle = "0-20 cm") +
  theme(plot.title = element_text(size = 16, face = "bold.italic"),
        plot.subtitle = element_text (size= 14))


C_data2 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gCm2) %>%
  filter(grepl('LATR', plant)) %>% 
  filter(grepl('20', depth)) %>% 
  filter(!grepl("organic", fraction))

p2 <- ggbarplot( C_data2, x = "treatment", y = "gCm2", add = "mean_se",
                 fill = "fraction") + ggtitle("20-40 cm") +
  theme(plot.title = element_text(size = 14))

C_data3 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gCm2) %>%
  filter(grepl('INSP', plant)) %>% 
  filter(grepl('0', depth)) %>% 
  filter(!grepl("organic", fraction))

p3 <- ggbarplot( C_data3, x = "treatment", y = "gCm2", add = "mean_se",
                 fill= "fraction") + ggtitle("Interspace", subtitle = "0-20 cm") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text (size= 14))


C_data4 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gCm2) %>%
  filter(grepl('INSP', plant)) %>% 
  filter(grepl('20', depth)) %>% 
  filter(!grepl("organic", fraction))

p4 <- ggbarplot( C_data4, x = "treatment", y = "gCm2", add = "mean_se",
                 fill = "fraction") + ggtitle("20-40 cm") +
  theme(plot.title = element_text(size = 14))
p4


#######

simple_theme <- theme(panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background=element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      axis.text.y = element_text(size=12),
                      axis.title.y=element_text(size=14, face="bold"),
                      axis.text.x = element_text(size=14, face="bold", hjust = .5, vjust=0),
                      legend.text=element_text(size=14),
                      legend.title=element_text(size=14, face = "bold"),
                      axis.ticks = element_blank(),
                      plot.margin = margin(20, 10, 10, 10))

# LATR 0-20
bar.1 <- p1 + ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,400))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+
  simple_theme

bar.1

# LATR, 20-40 
bar.2 <- p2 + ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,400))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+
  simple_theme

bar.2

# INSP, 0-20 cm
bar.3 <- p3 + ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits=c(0,400))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+ 
  simple_theme

bar.3

# INSP, 20-40 
bar.4 <- p4 + ylab(expression(bold(paste(SOC~(g~C~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits=c(0,400))+
  scale_fill_manual(name= "Soil Fraction",
                      values = c("#ef8a62","#67a9cf"),
                      labels=c("MAOM","POM"))+
  simple_theme

bar.4

bar.C <- ggarrange(bar.1, bar.3, bar.2, bar.4,
          align='hv', legend = "bottom",
          common.legend = T)

ggsave(filename= "../NDFF-fractions/figures/stackedbar_gCm2.png",
       dpi = 300, width = 10, height = 10, units = "in",
       plot= bar.C)

##### ggbarplot for N stock

#
N_data1 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gNm2) %>%
  filter(grepl('LATR', plant)) %>% 
  filter(!grepl('20|40|60|80', depth)) %>% 
  filter(!grepl("organic", fraction))

p1 <- ggbarplot( N_data1, x = "treatment", y = "gNm2", add = "mean_se",
                 fill = "fraction") + ggtitle("L. tridentata", subtitle = "0-20 cm") +
  theme(plot.title = element_text(size = 16, face = "bold.italic"),
        plot.subtitle = element_text (size= 14))


N_data2 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gNm2) %>%
  filter(grepl('LATR', plant)) %>% 
  filter(grepl('20', depth)) %>% 
  filter(!grepl("organic", fraction))

p2 <- ggbarplot( N_data2, x = "treatment", y = "gNm2", add = "mean_se",
                 fill = "fraction") + ggtitle("20-40 cm") +
  theme(plot.title = element_text(size = 14))

N_data3 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gNm2) %>%
  filter(grepl('INSP', plant)) %>% 
  filter(grepl('0', depth)) %>% 
  filter(!grepl("organic", fraction))

p3 <- ggbarplot( N_data3, x = "treatment", y = "gNm2", add = "mean_se",
                 fill= "fraction") + ggtitle("Interspace", subtitle = "0-20 cm") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text (size= 14))


N_data4 <- fractions_long %>%
  select(treatment, fraction, depth, plant, gNm2) %>%
  filter(grepl('INSP', plant)) %>% 
  filter(grepl('20', depth)) %>% 
  filter(!grepl("organic", fraction)) %>% 
  na.omit()

p4 <- ggbarplot( N_data4, x = "treatment", y = "gNm2", add = "mean_se",
                 fill = "fraction") + ggtitle("20-40 cm") +
  theme(plot.title = element_text(size = 14))
p4


#######

simple_theme <- theme(panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background=element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black"),
                      axis.text.y = element_text(size=12),
                      axis.title.y=element_text(size=14, face="bold"),
                      axis.text.x = element_text(size=14, face="bold", hjust = .5, vjust=0),
                      legend.text=element_text(size=14),
                      legend.title=element_text(size=14, face = "bold"),
                      axis.ticks = element_blank(),
                      plot.margin = margin(20, 10, 10, 10))

# LATR 0-20
bar.1 <- p1 + ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,60))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+
  simple_theme

bar.1

# LATR, 20-40 
bar.2 <- p2 + ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits = c(0,60))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+
  simple_theme

bar.2

# INSP, 0-20 cm
bar.3 <- p3 + ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits=c(0,60))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+ 
  simple_theme

bar.3

# INSP, 20-40 
bar.4 <- p4 + ylab(expression(bold(paste(Total~N~(g~N~"*"~m^{-2}))))) +
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste("Control")),expression("eCO"[2],)))+
  scale_y_continuous(expand = c(0,0), limits=c(0,60))+
  scale_fill_manual(name= "Soil Fraction",
                    values = c("#ef8a62","#67a9cf"),
                    labels=c("MAOM","POM"))+
  simple_theme

bar.4

bar.N <- ggarrange(bar.1, bar.3, bar.2, bar.4,
                   align='hv', legend = "bottom",
                   common.legend = T)

ggsave(filename= "../NDFF-fractions/figures/stackedbar_gNm2.png",
       dpi = 300, width = 10, height = 10, units = "in",
       plot= bar.N)
