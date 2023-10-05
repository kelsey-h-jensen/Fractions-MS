library(tidyverse)
library(lme4)
library(emmeans)


pygcms <- read.csv("../NDFF-fractions/data/compounds_all.csv", header=T)


pygcms_long <- pygcms %>% 
  pivot_longer(
    c(LATR_1, AMDU_1, PLRI_1, LYPA_1, LYAN_1, INSP_1,
      LATR_2, AMDU_2, PLRI_2, LYPA_2, LYAN_2,
      LATR_3, AMDU_3, PLRI_3, LYPA_3, LYAN_3, INSP_3,
      LATR_4, AMDU_4, PLRI_4, LYPA_4, LYAN_4, INSP_4,
      LATR_5, AMDU_5, PLRI_5, LYPA_5, LYAN_5, INSP_5,
      LATR_6, AMDU_6, PLRI_6, LYPA_6, LYAN_6, INSP_6),
    names_to='ID', 
    values_to='percent') %>% 
  separate(ID, c("plant","ring"), sep="_") %>% ## Add CO2 treatment info based on ring number
  mutate(treatment = if_else(ring <= 3, "elevated", "ambient"))

view(pygcms_long)


pygcms_fig <- pygcms_long %>% filter(grepl("INSP|LATR", plant)) %>% 
  mutate(Source = recode(Source, 'Lignin+TMAH' = "Lignin", 'Phenol+TMAH'="Phenol"))

pygcms_means <- pygcms_fig %>%
  group_by(treatment, plant, Compound, Source) %>% 
  summarize(mean = mean(percent)) %>%  
  group_by(treatment, plant, Source) %>% 
  summarize(sum = sum(mean))

cmpds_means <- pygcms_long %>%
  filter(grepl("INSP|LATR", plant)) %>% 
  group_by(Source, Compound, plant, treatment) %>%
  summarize(mean = mean(percent)) %>% 
  filter(mean > 0.01)
  
#   group_by(Source, Compound, plant, treatment) %>% 
# summarize(sum = sum(percent)) %>% 

simple_theme <- theme(panel.border = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(colour = "black"),
                      axis.title.y = element_text(size = 18),
                      axis.title.x = element_blank(),
                      axis.text.y = element_text(size=16),
                      axis.text.x = element_text(size=16),
                      axis.ticks = element_blank(),
                      legend.key=element_blank(), 
                      legend.title= element_text(size = 18), 
                      legend.text = element_text(size =16))

plant.labs <- c("Interspace", "L. tridentata")
names(plant.labs) <- c("INSP", "LATR")

tmt.labs <- c("Control", "+CO2")
names(tmt.labs) <- c("ambient", "elevated")

colors <- c("#6a3d9a", "#b2df8a", "#33a02c", "#fb9a99", 
  "#e31a1c", "#fdbf6f", "#ff7f00", "#a6cee3")

###### LATR & INSP by treatment ######

bar.all <- pygcms_fig %>%
  group_by(treatment, plant, Compound, Source) %>% 
  summarize(mean = mean(percent)) %>%  
  group_by(treatment, plant, Source) %>% 
  summarize(sum = sum(mean)) %>%
  ggplot(aes(x = treatment, y = sum, fill = Source)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  facet_grid(~plant, labeller = labeller(plant = plant.labs)) + 
  scale_fill_manual(values= colors, 
    name= "Compund Source") + 
  scale_x_discrete(labels=c("Control  ", (expression(paste(+CO[2]))))) +
  scale_y_continuous(expand = c(0,0))+
  ylab("Relative Amount (%)")+
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"))+
  simple_theme 

bar.all

ggsave(filename= "../NDFF-fractions/figures/stackedbar_classes.png",
       dpi = 300, width = 8, height = 6, units = "in",
       plot= bar.all)

####

bar.tmt <- pygcms_fig %>%
  group_by(treatment, plant, Compound, Source) %>% 
  summarize(mean = mean(percent)) %>%  
  group_by(treatment, plant, Source) %>% 
  summarize(sum = sum(mean)) %>%
  ggplot(aes(x = plant, y = sum, fill = Source)) + 
  geom_bar(position = position_stack(reverse = TRUE), stat="identity") +
  facet_grid(~ treatment, labeller = labeller(treatment = tmt.labs)) + 
  scale_fill_manual(values= colors, 
                    name= "Compund Source") + 
  scale_y_continuous(expand = c(0,0))+
  scale_x_discrete(labels=c("Interspace", expression(italic(paste(L.~tridentata))))) +
  ylab("Relative Amount (%)")+
  theme(strip.text.x = element_text(size = 12, face = "bold"))+
  simple_theme 

bar.tmt

ggsave(filename= "../NDFF-fractions/figures/stackedbar_cmpds_plant.png",
       dpi = 300, width = 9, height = 6, units = "in",
       plot= bar.tmt)

####################
# Specific Compounds
####################

# Guaiacol/syringol

pygcms %>% select(-Type, -Source) %>% 
  filter(grepl('Guaiacol|Syringol', Compound)) %>% 
  pivot_longer(
    c(LATR_1, AMDU_1, PLRI_1, LYPA_1, LYAN_1, INSP_1,
      LATR_2, AMDU_2, PLRI_2, LYPA_2, LYAN_2,
      LATR_3, AMDU_3, PLRI_3, LYPA_3, LYAN_3, INSP_3,
      LATR_4, AMDU_4, PLRI_4, LYPA_4, LYAN_4, INSP_4,
      LATR_5, AMDU_5, PLRI_5, LYPA_5, LYAN_5, INSP_5,
      LATR_6, AMDU_6, PLRI_6, LYPA_6, LYAN_6, INSP_6),
    names_to='ID', 
    values_to='percent') %>% 
  pivot_wider(names_from = Compound,
              values_from = percent) %>% 
  rename(guaiacol = 'Phenol, 2-methoxy- (Guaiacol)') %>% 
  rename(syringol = 'Phenol, 2,6-dimethoxy- (Syringol)') %>%
  mutate(ratio = guaiacol / syringol) %>%
  View()
#### Mostly NAs and 0s 

## Polysaccharides

t1 <- pygcms_long %>%  filter(Source == 'Polysaccharide') %>% 
  group_by(plant, ring) %>% 
  summarize(sum_poly = sum(percent)) %>% 
  mutate(ID=paste0(plant, "_", ring)) %>% 
  mutate(treatment = if_else(ring <= 3, "elevated", "ambient"))

lm.1 <- lmer(data= t1, sum_poly ~ treatment*plant + (1|ring))
summary(lm.1)
anova(lm.1)
paircomp.1 <- summary(emmeans(lm.1, pairwise~treatment|plant)$contrasts)
# No sig differences

### levoglucosan: polysacharide

t2 <- pygcms_long %>%  filter(Compound == 'Levoglucosan') %>%
  pivot_wider(names_from = Compound,
              values_from = percent) %>%  
  mutate(ID=paste0(plant, "_", ring))

# this table will work for Levoglucosan/sum_poly analysis
t3 <- inner_join(t1, t2, by=c("ID","plant","treatment","ring")) %>% 
  mutate(ratio= Levoglucosan/sum_poly)
hist(t3$ratio)
# how to deal with left skewed data??? So many 0s

### Indole: N bearing (plant)
t4 <- pygcms_long %>%  filter(Source == 'N-Bearing') %>%
  group_by(plant, ring) %>% 
  summarize(sum_Nbear = sum(percent)) %>% 
  mutate(ID=paste0(plant, "_", ring))

t5 <- pygcms_long %>% filter(Compound == 'Indole') %>%
    pivot_wider(names_from = Compound,
                values_from = percent) %>% 
  mutate(ID=paste0(plant, "_", ring))
  
t6 <- inner_join(t4, t5, by=c("ID","plant","ring")) %>%
  select(-Type, -Source, -ID) %>% 
    mutate(ratio= Indole/sum_Nbear)

hist(t6$ratio)

lm.2 <- lmer(data= t6, ratio ~ treatment*plant + (1|ring))
summary(lm.2)
anova(lm.2)
paircomp.2 <- summary(emmeans(lm.2, pairwise~treatment|plant)$contrasts)

# Sig diff for LYAN but *not* for LATR


#### Pyrrole + pyridine / N cmpds

t7 <- pygcms_long %>% 
  filter(grepl('Pyrrole|Pyridine', Compound)) %>% # should I include both N bearing pyrrole and aromatic
  group_by(plant, ring) %>% 
  summarize(sum_pyrr = sum(percent)) %>% 
  mutate(ID=paste0(plant, "_", ring))

t8 <- inner_join(t7, t4, by=c("ID","plant","ring")) %>%
  mutate(ratio= sum_pyrr/sum_Nbear) %>% 
  mutate(treatment = if_else(ring <= 3, "elevated", "ambient"))

lm.4 <- lmer(data= t8, ratio ~ treatment*plant + (1|ring))
summary(lm.4)
anova(lm.4)
paircomp.4 <- summary(emmeans(lm.4, pairwise~treatment|plant)$contrasts)

############

palm <- pygcms_long %>% filter(Source == "Lipid") %>% 
  filter(plant == "LATR")

lm.5 <-  lm(data= palm, percent ~ treatment)
summary(lm.5)
anova(lm.5)

# filter(Compound == "Hexadecanoic acid, methyl ester (Palmitic acid-C16)")
means <- pygcms_long %>%  filter(plant == "LATR") %>% 
  filter(Compound == "Hexadecanoic acid, methyl ester (Palmitic acid-C16)") %>% 
           group_by(treatment) %>% 
           summarise(means = mean(percent)) %>%  View()

means <- pygcms_long %>%  filter(plant == "LATR") %>% 
  group_by(Compound, Source, treatment) %>% 
  summarise(means = mean(percent)) %>%  View()
