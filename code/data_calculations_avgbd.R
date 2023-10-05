library(tidyverse)

soil <- read.csv("../NDFF-fractions/data/fractions_data_avgbd.csv", header=T)
## LATR ring 1, HF N data are an average of ring 2 and 3

soil$ring <- as.factor(soil$ring)
soil$depth <- as.factor(soil$depth)

fractions <- soil %>% filter(grepl("LATR|INSP", plant)) %>%
  rename(hf_13C = hf_isoC) %>% 
  rename(lf_13C = lf_isoC) %>% 
  mutate(per_recovery_wt = (((lf_final + hf_final)/drywt)*100)) %>% 
  mutate(lf_frac = (lf_final/drywt)*100) %>% 
  mutate(hf_frac = (hf_final/drywt)*100) %>% 
  mutate(lf_mgCgsoil = ((lf_perC/100)*1000*(lf_final/drywt))) %>% 
  mutate(lf_gCm2 = (lf_mgCgsoil*bulkdensity)/10000) %>% 
  mutate(lf_mgNgsoil = ((lf_perN/100)*1000*(lf_final/drywt))) %>% 
  mutate(lf_gNm2 = (lf_mgNgsoil*bulkdensity)/10000) %>% 
  mutate(lf_CN = lf_mgCgsoil/lf_mgNgsoil) %>% 
  mutate(hf_mgCgsoil = ((hf_perC/100)*1000*(hf_final/drywt))) %>% 
  mutate(hf_gCm2 = (hf_mgCgsoil*bulkdensity)/10000) %>%
  mutate(hf_mgNgsoil = ((hf_perN/100)*1000*(hf_final/drywt))) %>% 
  mutate(hf_gNm2 = (hf_mgNgsoil*bulkdensity)/10000) %>%
  mutate(hf_CN = hf_mgCgsoil/hf_mgNgsoil) %>% 
  mutate(soc_mgCgsoil = ((soc_perC/100)*1000)) %>% 
  mutate(soc_gCm2 = (soc_mgCgsoil*bulkdensity)/10000) %>% 
  mutate(soc_mgNgsoil = ((soc_perN/100)*1000)) %>% 
  mutate(soc_gNm2 = (soc_mgNgsoil*bulkdensity)/10000) %>%
  mutate(sum_frac_C = hf_mgCgsoil + lf_mgCgsoil) %>% 
  mutate(soc_recovery = (sum_frac_C/soc_mgCgsoil)*100) %>% 
  mutate(lf_pertotC = (lf_mgCgsoil/sum_frac_C)*100) %>% 
  mutate(hf_pertotC = (hf_mgCgsoil/sum_frac_C)*100) %>% 
  mutate(sum_frac_N = hf_mgNgsoil + lf_mgNgsoil) %>% 
  mutate(son_recovery = (sum_frac_N/soc_mgNgsoil)*100) %>%
  mutate(lf_pertotN = (lf_mgNgsoil/sum_frac_N)*100) %>% 
  mutate(hf_pertotN = (hf_mgNgsoil/sum_frac_N)*100)

save( fractions, file = "../NDFF-fractions/data/calcs.Rdata" )
write.csv( fractions, "../NDFF-fractions/data/all_calcs.csv" )

### Save table of data derived from both HF and LF frac

combined_data <- fractions %>% 
  select(depth, ring, treatment, plant,
    per_recovery_wt, sum_frac_C, dif_soc, soc_recovery) %>% 
  filter(!grepl("4|6|8", depth))

save(combined_data, file = "../NDFF-fractions/data/combinded_frac_data.Rdata")

means_combined <- fractions %>% 
  filter(!grepl("4|6|8", depth)) %>% 
  group_by(plant, depth, treatment) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

save(means_combined, file = "../NDFF-fractions/data/combined_frac_means.csv")


###### Pivot long

fractions_long <- fractions  %>% filter(!grepl("4|6|8", depth)) %>% 
  select(-Sample, -bulkdensity, -drywt, 
         -per_recovery_wt, -sum_frac_C, -dif_soc, -soc_recovery) %>% 
  select(-contains("soc")) %>% 
  pivot_longer(where(is.numeric), names_to = "fraction") %>% 
  separate(fraction, c("fraction","response")) %>% 
  na.omit()

save( fractions_long, file = "../NDFF-fractions/data/fractions_long.Rdata" )


### Calc means ###

means <- fractions_long %>% 
  group_by(plant, depth, treatment, fraction, response) %>% 
  summarise( mean = mean(value), 
             sd = sd(value),
             n = length(value),
             se = sd/sqrt(n))

write.csv(means, file = "../NDFF-fractions/data/fraction_means.csv")

####

means_wide <- fractions %>% filter(!grepl("4|6|8", depth)) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

############
# Recovery
############

recoveryC <- fractions %>% group_by(plant, treatment) %>%  
  na.omit() %>% 
  summarise( mean = mean(soc_recovery), 
             sd = sd(soc_recovery),
             n = length(soc_recovery),
             se = sd/sqrt(n))

write.csv(recoveryC, file = "../NDFF-fractions/data/recovery_means.csv")

recoveryN <- fractions %>% filter(plant == "LATR") %>% 
  group_by(treatment) %>%  
  na.omit() %>% 
  summarise( mean = mean(son_recovery), 
             sd = sd(son_recovery),
             n = length(son_recovery),
             se = sd/sqrt(n))

fractions %>% filter(plant == "LATR") %>% 
  ggplot(aes(x= soc_recovery, y= son_recovery, color = treatment)) + geom_point()
write.csv(recoveryN, file = "../NDFF-fractions/data/recoveryN_means.csv")

library(corrplot)
cor.test(fractions$soc_recovery, fractions$son_recovery, method = "spearman")

fractions %>% filter(plant == "LATR") %>% 
  ggplot(aes(x= soc_recovery, y= lf_mgCgsoil, color = treatment)) + geom_point() +
  geom_smooth(method = "lm")


fractions %>% filter(plant == "LATR") %>% 
  ggplot(aes(x= hf_mgCgsoil, y= soc_recovery, color = treatment)) + geom_point() +
  geom_smooth(method = "lm")


fractions %>% filter(plant == "LATR") %>% 
  ggplot(aes(x= soc_mgCgsoil, y= soc_recovery, color = treatment)) + geom_point() +
  geom_smooth(method = "lm")
cor.test(fractions$soc_recovery, fractions$hf_mgCgsoil, method = "spearman")
