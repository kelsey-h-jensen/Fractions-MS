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
  separate(ID, c("plant","ring"), sep="_") %>% 
  mutate(treatment = if_else(ring <= 3, "elevated", "ambient"))

view(pygcms_long)


t1 <- pygcms_long %>% filter(grepl("INSP|LATR", plant)) %>% 
  mutate(Source = recode(Source, 'Lignin+TMAH' = "Lignin", 'Phenol+TMAH'="Phenol")) %>% 
  group_by(plant, ring, treatment, Source) %>% 
  summarize(sum = sum(percent)) %>% 
  filter(!grepl('TMAH', Source)) %>% 
  pivot_wider(names_from = Source, values_from = sum) %>% 
  rename(NBearing = 'N-Bearing') %>% 
  rename(Unknown = 'Unknown Origin')

pygcms_insp <- pygcms_long %>% filter(plant == "INSP")
pygcms_latr <- pygcms_long %>% filter(plant == "LATR")


# vector with responses
responses <- c("Aromatic", "Lignin", "Lipid", "Polysaccharide", 
                   "NBearing", "Protein", "Phenol", "Unknown")

# list for holding the models
model_classes <- vector("list", length = length(responses) )
names( model_classes ) <- responses


#############

for(j in 1:length(responses) ){
  
  # model formula for all covertypes    
  form <- paste0(responses[j], " ~ (treatment+plant)^2 + (1|ring)")
  # fitted model
  model_classes[[ responses[j] ]] <- lmer( as.formula(form), data = t1 )
  
  cat(paste0("\nANOVA for ",responses[j],"\n"))
  cat("--------------------------------\n")
  print( anova( model_classes[[ responses[j] ]] ) )
  
  emmean <- print(summary(emmeans(model_classes[[ responses[j] ]], 
                                  ~treatment+plant), type= "response"))
  write.csv(emmean, paste0("../NDFF-fractions/models/output/classes/emmean_",responses[j],".csv", sep= ""))
  
  paircomp <- summary(emmeans(model_classes[[ responses[j] ]], 
                                pairwise~treatment|plant)$contrasts) 
  write.csv(paircomp, paste0("../NDFF-fractions/models/output/classes/paircomp_tmt_",responses[j],".csv", sep= ""))
  
  paircomp2 <- summary(emmeans(model_classes[[ responses[j] ]], 
                              pairwise~plant|treatment)$contrasts) 
  write.csv(paircomp2, paste0("../NDFF-fractions/models/output/classes/paircomp_plant_",responses[j],".csv", sep= ""))
  
}

# save the model
save( 
  model_classes, 
  file = "../NDFF-fractions/models/lmer_classes.RData" 
)

########################

