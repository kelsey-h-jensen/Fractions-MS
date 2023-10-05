library(tidyverse)

pygcms <- read.csv("../NDFF-fractions/data/compounds_all.csv", header=T)

# Number of compounds identified
count <- pygcms %>%  mutate_if(is.numeric, ~1 * (. != 0)) %>% 
  summarise_if(is.numeric, sum) %>% 
  pivot_longer(
    c(LATR_1, AMDU_1, PLRI_1, LYPA_1, LYAN_1, INSP_1,
      LATR_2, AMDU_2, PLRI_2, LYPA_2, LYAN_2,
      LATR_3, AMDU_3, PLRI_3, LYPA_3, LYAN_3, INSP_3,
      LATR_4, AMDU_4, PLRI_4, LYPA_4, LYAN_4, INSP_4,
      LATR_5, AMDU_5, PLRI_5, LYPA_5, LYAN_5, INSP_5,
      LATR_6, AMDU_6, PLRI_6, LYPA_6, LYAN_6, INSP_6),
    names_to='ID', 
    values_to='count') %>% 
  separate(ID, c("plant","ring"), sep="_") %>% ## Add CO2 treatment info based on ring number
  mutate(treatment = if_else(ring <= 3, "elevated", "ambient")) %>% 
  filter(grepl("INSP|LATR", plant)) %>% 
  mutate(richness = count/180)

means <- count %>% group_by(plant, treatment) %>% 
  summarise(mean = mean(count), 
             sd = sd(count),
             n = length(count),
             se = sd/sqrt(n))

## Transpose data to wide 
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
  mutate(treatment = if_else(ring <= 3, "elevated", "ambient")) %>% 
  filter(grepl("INSP|LATR", plant))


pygcms_wide <- pygcms_long %>% select(-Type, -Source) %>% 
  pivot_wider(names_from = Compound,
              values_from = percent ) 

palmitic <- pygcms_wide %>% 
  select(plant, treatment, ring, "Hexadecanoic acid, methyl ester (Palmitic acid-C16)") %>% 
  rename(palmitic = "Hexadecanoic acid, methyl ester (Palmitic acid-C16)") %>% 
  group_by(plant, treatment) %>% 
  summarise(mean = mean(palmitic), 
            sd = sd(palmitic),
            n = length(palmitic),
            se = sd/sqrt(n))
  

div.1 <- pygcms_long %>%  filter(plant == "LATR") %>% filter(ring == "1") %>% 
  mutate(lnpi = log10(percent)) %>% 
  mutate(x = lnpi * percent) %>% 
  na.omit() %>% summarise_if(is.numeric, sum) %>% 
  mutate(div.s = x*-1)
  
## loop to calculate diversity 

responses <- c("LATR_1", "INSP_1", "LATR_2", 
          "LATR_3", "INSP_3", "LATR_4", "INSP_4",
          "LATR_5", "INSP_5", "LATR_6", "INSP_6")


for(j in 1:length(responses) ){
  
  div <- pygcms %>% select(responses[j]) %>% 
    mutate(lnpi = log10(get(responses[j]))) %>% 
    mutate(x = lnpi * get(responses[j])) %>% 
    na.omit() %>% summarise_if(is.numeric, sum) %>% 
    mutate(div.s = x*-1)

  print(div)
  
  write.csv(div, paste0("../NDFF-fractions/models/output/diversity/div_",responses[j],".csv", sep= ""))
  
  
}


#####

pygcms_wide <- pygcms_long %>% select(-Type, -Source) %>% 
  pivot_wider(names_from = Compound,
              values_from = percent )

######

diversity <- read.csv(file= "../NDFF-fractions/data/cmpds_diversity.csv", header = TRUE)

mean.div <- diversity %>% group_by(plant, treatment) %>% 
  summarise(mean = mean(diversity), 
            sd = sd(diversity),
            n = length(diversity),
            se = sd/sqrt(n))

lm.1 <- lmer(data= diversity, diversity ~ treatment*plant + (1|ring))
summary(lm.1)
anova(lm.1)
paircomp.1 <- summary(emmeans(lm.1, pairwise~treatment|plant)$contrasts)
paircomp.2 <- summary(emmeans(lm.1, pairwise~plant|treatment)$contrasts)


mean.rich <- diversity %>% group_by(plant, treatment) %>% 
  summarise(mean = mean(richness), 
            sd = sd(richness),
            n = length(richness),
            se = sd/sqrt(n))

lm.2 <- lmer(data= diversity, richness ~ treatment*plant + (1|ring))
summary(lm.2)
anova(lm.2)
paircomp.3 <- summary(emmeans(lm.2, pairwise~treatment|plant)$contrasts)
paircomp.4 <- summary(emmeans(lm.2, pairwise~plant|treatment)$contrasts)

