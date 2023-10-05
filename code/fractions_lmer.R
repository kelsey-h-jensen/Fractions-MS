#### These models are for comparison to Koyama table 2 and figure 1
## Not used for ecosystem C stock calculations


# analyze soils at depth with lmer
library(lme4)
library(lmerTest)
library(emmeans)
logit <- function(x){ log(x/(1-x)) }

# load in the data
load( "../NDFF-fractions/data/fractions_long.Rdata" )


fractions_wide <- fractions_long %>%  
  pivot_wider( values_from = value, names_from= response) %>% 
  rename( isoC = "13C" )

lf_wide <- fractions_wide %>% filter( fraction == "lf")
hf_wide <- fractions_wide %>% filter( fraction == "hf")

#########################
#### Light Fraction #####
#########################

# define response variables
lf_wide$isoC <- lf_wide$isoC
lf_wide$isoN <- lf_wide$isoN 

lf_wide$log_mgCgsoil <- lf_wide$mgCgsoil
lf_wide$log_mgNgsoil <- lf_wide$mgNgsoil

lf_wide$log_gCm2 <- lf_wide$gCm2
lf_wide$log_gNm2 <- lf_wide$gNm2

lf_wide$CN <- lf_wide$CN # Normal, non transformed


# vector with responses
responses <- c("isoC", "isoN", 
               "mgCgsoil", "mgNgsoil", 
               "gCm2", "gNm2","CN", 
               "pertotC", "pertotN")

# list for holding the models
model_tpd_lf <- vector("list", length = length(responses) )
names( model_tpd_lf ) <- responses 


#############

for(j in 1:length(responses) ){
  
  # model formula for all covertypes    
  form <- paste0(responses[j], " ~ (plant+treatment+depth)^3 + (1|ring)")
  # fitted model
  model_tpd_lf[[ responses[j] ]] <- lmer( as.formula(form), data = lf_wide )
  
  cat(paste0("\nANOVA for ",responses[j],"\n"))
  cat("--------------------------------\n")
  print( anova( model_tpd_lf[[ responses[j] ]] ) )
  
  anova <- anova( model_tpd_lf[[ responses[j] ]] )
  
  write.csv(anova, paste0("../NDFF-fractions/models/output/fractions/lf_anova_",responses[j],".csv", sep= ""))
  
  paircomp <- summary(emmeans(model_tpd_lf[[ responses[j] ]], 
                             pairwise~treatment|plant|depth)$contrasts) 
  
  write.csv(paircomp, paste0("../NDFF-fractions/models/output/fractions/lf_trt_paircomp_",responses[j],".csv", sep= ""))
  
  paircomp <- summary(emmeans(model_tpd_lf[[ responses[j] ]], 
                              pairwise~plant|treatment|depth)$contrasts) 
  
  write.csv(paircomp, paste0("../NDFF-fractions/models/output/fractions/lf_plant_paircomp_",responses[j],".csv", sep= ""))
  
  
  emmeans <- summary(emmeans(model_tpd_lf[[ responses[j] ]], 
                             ~treatment+plant+depth), type= "response")
  
  write.csv(emmeans, paste0("../NDFF-fractions/models/output/fractions/lf_emmeans_",responses[j],".csv", sep= ""))
  
  save( 
    model_tpd_lf, 
    file = "../NDFF-fractions/models/lmer_tmt_lf.RData" )
  
}




#########################
#### Heavy Fraction #####
#########################
hf_wide <- fractions_wide %>% filter( fraction == "hf")

# define response variables
hf_wide$isoC <- hf_wide$isoC
hf_wide$isoN <- hf_wide$isoN 

hf_wide$log_mgCgsoil <- hf_wide$mgCgsoil
hf_wide$log_mgNgsoil <- hf_wide$mgNgsoil

hf_wide$log_gCm2 <- hf_wide$gCm2
hf_wide$log_gNm2 <- hf_wide$gNm2

hf_wide$CN <- hf_wide$CN # Normal, non transformed


# vector with responses
responses <- c("isoC", "isoN", 
               "mgCgsoil", "mgNgsoil", 
               "gCm2", "gNm2","CN", 
               "pertotC", "pertotN")

# list for holding the models
model_tpd_hf <- vector("list", length = length(responses) )
names( model_tpd_hf ) <- responses 


#############

for(j in 1:length(responses) ){
  
  # model formula for all covertypes    
  form <- paste0(responses[j], " ~ (plant+treatment+depth)^3 + (1|ring)")
  # fitted model
  model_tpd_hf[[ responses[j] ]] <- lmer( as.formula(form), data = hf_wide )
  
  cat(paste0("\nANOVA for ",responses[j],"\n"))
  cat("--------------------------------\n")
  print( anova( model_tpd_hf[[ responses[j] ]] ) )
  
  anova <- anova( model_tpd_hf[[ responses[j] ]] )
  
  write.csv(anova, paste0("../NDFF-fractions/models/output/fractions/hf_anova_",responses[j],".csv", sep= ""))
  
  paircomp1 <- summary(emmeans(model_tpd_hf[[ responses[j] ]], 
                              pairwise~treatment|plant|depth)$contrasts) 
  
  write.csv(paircomp1, paste0("../NDFF-fractions/models/output/fractions/hf_tmt_paircomp_",responses[j],".csv", sep= ""))
  
  paircomp2 <- summary(emmeans(model_tpd_hf[[ responses[j] ]], 
                              pairwise~plant|treatment|depth)$contrasts) 
  
  write.csv(paircomp2, paste0("../NDFF-fractions/models/output/fractions/hf_plant_paircomp_",responses[j],".csv", sep= ""))
  
  
  emmeans <- summary(emmeans(model_tpd_hf[[ responses[j] ]], 
                             ~plant+treatment+depth), type= "response")
  
  write.csv(emmeans, paste0("../NDFF-fractions/models/output/fractions/hf_emmeans_",responses[j],".csv", sep= ""))
  
  save( 
    model_tpd_hf, 
    file = "../NDFF-fractions/models/lmer_tmt_hf.RData" )
  
}


## Checking models for normality
lmer1 <- lmer(log10(mgCgsoil) ~ (treatment+depth+plant)^3 + (1|ring), data= hf_wide)
plot(predict(lmer1), residuals(lmer1)) 
qqnorm(residuals(lmer1)); qqline(residuals(lmer1))
hist(residuals(lmer1))

emmip(lmer1, plant ~ treatment | depth, type = "response", CIs=TRUE )

write.csv( summary(emmeans(lmer1, pairwise~treatment|depth|fraction)$contrasts) , 
           "../NDFF-fractions/models/output/fractions/paircomp_gCm2.csv")

emmeans1 <- summary(emmeans(lmer1,  ~treatment+depth+plant), type= "response")
write.csv( emmeans1 , 
           "../NDFF-fractions/models/output/fractions/emmeans_gCm2.csv")

lmer1 <- lmer(log(mgCgsoil) ~ (treatment+depth+plant)^3 + (1|ring), data= lf_wide)



