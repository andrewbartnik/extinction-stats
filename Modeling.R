library(MASS)
library(AICcmodavg)
### Modeling


#Endemism
endemism <- glm(extinct~endemic, family = 'binomial', data = predictors_2)
summary(endemism)
#log reg - habitat


habitat <- glm(extinct ~ habitat, family = 'binomial', data = predictors_2)
summary(habitat)


#log reg - threat


threat <- glm(extinct ~ threat, family = 'binomial', data = predictors_2)
summary(threat)




##log reg - use



use <- glm(extinct ~ use, family = 'binomial', data = predictors_2)
summary(use)

##log reg - classification



taxa <- glm(extinct ~ class, family = 'binomial', data = predictors_2)
summary(taxa)

step1 <- glm(extinct~endemic, family = 'binomial',data = predictors_2)
step2 <- glm(extinct~endemic + habitat, family = 'binomial',data = predictors_2)
step3 <- glm(extinct~endemic + habitat + threat, family = 'binomial',data = predictors_2)
step4 <- glm(extinct~endemic + habitat + threat + class, family = 'binomial',data = predictors_2)
step5 <- glm(extinct~endemic + habitat + threat + class + use, family = 'binomial',data = predictors_2)



models <- list(step1, step2, step3, step4, step5)
names <- c('endemic', 'endemic_habitat', 'endemic_habitat_threat', 'endemic_habitat_threat_class', 'endemic_habitat_threat_class_use')

aictab(cand.set = models, modnames = names)





