#### Inter-annotator Agreement vs. System Performance (F1 measure) Analysis ####
#### Mayla Boguslav and Kevin Bretonnel Cohen ####

##########################
#### F1 measure > IAA ####
##########################

iaa_F1GthanIAA <- c(0.5535, 0.5535, 0.5535, 0.617, 0.526,
                          0.69, 0.933, 0.819, 0.779, 0.815,
                          0.798, 0.773, 0.628, 0.75, 0.75,
                          0.75, 0.75, 0.75, 0.75, 0.75)
# F-measure, except two that are precisions 
system_F1GthanIAA <- c(0.5741, 0.6504, 0.6649, 0.63, 0.664,
                           0.725, 0.978, 0.875, 0.824, 0.87,
                           0.857, 0.823, 0.702, 0.839, 0.839,
                           0.845, 0.8644, 1, 0.845, 0.833)

# Difference = F-measure minus IAA
difference_F1GthanIAA <- c(0.0206, 0.0969, 0.1114, 0.013, 0.138,
                               0.035, 0.045, 0.056, 0.045, 0.055,
                               0.059, 0.05, 0.074, 0.089, 0.089,
                               0.095, 0.1144, 0.25, 0.095, 0.083)
#Level of agreement
level_F1GthanIAA <- c(3,3,3,4,3,4,5,5,4,5,4,4,4,4,4,4,4,4,4,4)

#basic distributional aspects
labels1 <- c(rep("IAA", 20), rep("F1", 20))
iaa.and.f1_F1GthanIAA <- c(iaa_F1GthanIAA, system_F1GthanIAA)
boxplot(iaa.and.f1_F1GthanIAA~labels1, ylim=c(0, 1.0), main="Agreement and F-measure ranges")

#Summary of the difference
boxplot(difference_F1GthanIAA)

#summary statistics
summary(iaa_F1GthanIAA) # median = 0.75
summary(system_F1GthanIAA) #median = 0.8360
summary(difference_F1GthanIAA) #median = 0.0785

#Shapiro-Wilk normality test = null hypothesis = the data is normally distributed (p >0.05) 
#or not if p<0.05
shapiro.test(iaa_F1GthanIAA) #not normally distributed
shapiro.test(system_F1GthanIAA) #normally distributed
shapiro.test(difference_F1GthanIAA) #not normally distributed
shapiro.test(level_F1GthanIAA) #not normally distributed

#graph
plot(iaa_F1GthanIAA, system_F1GthanIAA,
     xlim=c(0,1.0), ylim=c(0,1.0),
     xlab="Inter-annotator agreement",
     ylab="F-measure (mostly)")

#Spearman's correlation
cor(iaa_F1GthanIAA,system_F1GthanIAA, method ="spearman") # positive = 0.8069756

# spearman's rank test
## positive correlation for iaa and system
cor.test(iaa_F1GthanIAA,system_F1GthanIAA, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho=0.8069756, p=8.56 X 10^(-6)<0.05

## difference and level are not negatively correlated p-value = 0.06381
cor.test(difference_F1GthanIAA, level_F1GthanIAA, alternative = 'less' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) #rho =-0.3523232 , p = 0.06381
plot(difference_F1GthanIAA, level_F1GthanIAA,
     xlim=c(0,1.0), ylim=c(0,5.0),
     xlab="Difference (IAA-F1)",
     ylab="Level of agreement")

##########################
#### F1 measure < IAA ####
##########################

IAAGThanFmeasure <- read.table(file = 'IAAGthanFmeasure.csv', header = TRUE, sep=',', dec='.')
print(IAAGThanFmeasure)
plot(IAAGThanFmeasure$IAA, IAAGThanFmeasure$System,
     xlim=c(0,1.0), ylim=c(0,1.0),
     xlab="Inter-annotator agreement",
     ylab="F-measure (mostly)")

#basic distributional aspects
labels2 <- c(rep("IAA", 82), rep("F1", 82)) #half of 132 entries
iaa.and.f1_IAAGthanF1 <- c(IAAGThanFmeasure$IAA, IAAGThanFmeasure$System) #164 entries
boxplot(iaa.and.f1_IAAGthanF1~labels2, ylim=c(0, 1.0), main="Agreement and F-measure ranges")

#Summary of the difference
boxplot(IAAGThanFmeasure$Difference)

#summary statistics
summary(IAAGThanFmeasure$IAA) # median = 0.7647
summary(IAAGThanFmeasure$System) #median = 0.5865
summary(IAAGThanFmeasure$Difference) #median = -0.1655

#Shapiro-Wilk normality test = null hypothesis = the data is normally distributed (p >0.05) 
#or not if p<0.05
shapiro.test(IAAGThanFmeasure$IAA) #not normally distributed
shapiro.test(IAAGThanFmeasure$System) # not normally distributed
shapiro.test(IAAGThanFmeasure$Difference) # not normally distributed
shapiro.test(IAAGThanFmeasure$Level) #not normally distributed


#Spearman's correlation
cor(IAAGThanFmeasure$IAA, IAAGThanFmeasure$System, method ="spearman") # positive = 0.6532331

# spearman's rank test
## positive correlation for iaa and system significant
cor.test(IAAGThanFmeasure$IAA, IAAGThanFmeasure$System, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho=0.6532331, p=1.449e-11<0.05

## difference and level are significantly positively correlated p-value = 0.0368
cor.test(IAAGThanFmeasure$Difference, IAAGThanFmeasure$Level, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho= 0.258308 , p=0.009563<0.05

plot(IAAGThanFmeasure$Difference, IAAGThanFmeasure$Level,
     xlim=c(-0.8,0.2), ylim=c(0,5.0),
     xlab="Difference (F1-IAA)",
     ylab="Level of agreement")

###########################
#### All Data Combined ####
###########################

alldata <- read.table(file = 'All_IAA_Fmeasure.csv', header = TRUE, sep=',', dec='.')
print(alldata)
plot(alldata$IAA, alldata$System,
     xlim=c(0,1.0), ylim=c(0,1.0),
     xlab="Inter-annotator agreement",
     ylab="F-measure (mostly)")

#basic distributional aspects
labels <- c(rep("IAA", 102), rep("F1", 102)) #half of 132 entries
iaa.and.f1_alldata <- c(alldata$IAA, alldata$System) #204 entries
boxplot(iaa.and.f1_alldata~labels, ylim=c(0, 1.0), main="Agreement and F-measure ranges")

#Summary of the difference
boxplot(alldata$Difference)

#summary statistics
summary(alldata$IAA) # median = 0.7504
summary(alldata$System) #median = 0.6380
summary(alldata$Difference) #median = -0.1383s


#Shapiro-Wilk normality test = null hypothesis = the data is normally distributed (p >0.05) 
#or not if p<0.05
shapiro.test(alldata$IAA) #not normally distributed
shapiro.test(alldata$System) #not normally distributed
shapiro.test(alldata$Difference) #normally distributed
shapiro.test(alldata$Level) #not normally distributed


#Spearman's correlation
cor(alldata$IAA, alldata$System, method ="spearman") # positive = 0.5126484

# spearman's rank test
## positive correlation for iaa and system significant
cor.test(alldata$IAA, alldata$System, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho=0.5126484, p=1.81e-08<0.05

## difference and level are not negatively correlated p-value = 0.06381
cor.test(alldata$Difference, alldata$Level, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho= 0.1162316, p=0.1223>0.05

plot(alldata$Difference, alldata$Level,
     xlim=c(0,1.0), ylim=c(0,5.0),
     xlab="Difference (IAA-F1)",
     ylab="Level of agreement")
