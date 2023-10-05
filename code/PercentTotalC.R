## Figures for MS
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)


load("../NDFF-fractions/data/fractions_long.Rdata")
logit <- function(x){ log(x/(1-x)) }

perC <- fractions_long %>% filter(response == "pertotC") %>% 
  rename(pertotC = "value") 

### model change in fraction C

hf_perC <- perC %>% 
  filter(fraction == "hf")

lmer1 <- lmer(data= hf_perC, pertotC ~ (treatment+depth+plant)^3 + (1|ring))
plot(predict(lmer1), residuals(lmer1)) 
qqnorm(residuals(lmer1)); qqline(residuals(lmer1))
hist(residuals(lmer1))

anova(lmer1)

paircomp1 <- summary(emmeans(lmer1, pairwise~ treatment|depth|plant)$contrasts) 
write.csv( paircomp , "../NDFF-fractions/models/output/fractions/hf_pertotC_paircomp.csv")
emmeans1 <- summary(emmeans(lmer1,  ~treatment+depth+plant), type= "response")
write.csv( emmeans1 , "../NDFF-fractions/models/output/fractions/hf_pertotC_emmeans.csv")


lf_perC <- perC %>% 
  filter(fraction == "lf")
lmer2 <- lmer(data= lf_perC, pertotC ~ (treatment+depth+plant)^3 + (1|ring))
plot(predict(lmer2), residuals(lmer2)) 
qqnorm(residuals(lmer2)); qqline(residuals(lmer2))
hist(residuals(lmer2))

anova(lmer2)

paircomp2 <- summary(emmeans(lmer2, pairwise~ treatment|depth|plant)$contrasts) 
write.csv( paircomp2 , "../NDFF-fractions/models/output/fractions/lf_pertotC_paircomp.csv")
emmeans2 <- summary(emmeans(lmer1,  ~treatment+depth+plant), type= "response")
write.csv( emmeans2 , "../NDFF-fractions/models/output/fractions/lf_pertotC_emmeans.csv")

### Plot 
avg.perC <- perC %>%
  select(treatment, fraction, depth, plant, response, value) %>%
  group_by(fraction, plant, treatment, depth) %>%
  summarize_if(is.numeric, mean, na.rm=TRUE)

avg.perC$value <- round(avg.perC$value, digits=1)

depth.labs <- c("0-20 cm", "20-40 cm")
names(depth.labs) <- c("0", "20")

plant.labs <- c("Interspace", "L. tridentata")
names(plant.labs) <- c("INSP", "LATR")

p <- ggplot(avg.perC, aes(x=treatment, y=value, fill=fraction))+
  geom_bar(position = position_stack(reverse = TRUE), stat="identity")+
  scale_fill_discrete(limits=c("hf", "lf"))+
  facet_wrap(plant ~ depth, nrow= 1, labeller = labeller(depth = depth.labs, plant = plant.labs))

barplot <- p + ylab(expression(bold(paste("% Total SOC"))))+
  xlab(element_blank())+
  scale_x_discrete(labels = c(expression(paste(bold("Control"[],))),expression(paste(bold("+CO"[2],)))))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(name = "Fraction", 
                    values = c("#ef8a62","#67a9cf"), 
                    labels=c("MAOM","POM"),
                    guide = guide_legend(reverse = TRUE)) +
  geom_text(data= avg.perC, aes(label = value), position = position_stack(reverse = TRUE), 
            vjust = 3,  size = 4)+
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
        legend.title = element_text(size = 12, face= "bold"),
        legend.text = element_text(size = 12))+
  theme(strip.text.x = element_text(size = 12, face = "bold.italic"),
        strip.text.y = element_text(size = 12, face = "bold"))
  

barplot


ggsave(filename = "../NDFF-fractions/figures/pertotC_barplot.png",
       plot=barplot, dpi = 300, width = 8, height = 4, units = "in")
