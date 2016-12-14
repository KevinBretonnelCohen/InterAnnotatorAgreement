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


#basic distributional aspects
labels1 <- c(rep("IAA", 20), rep("F1", 20))
iaa.and.f1_F1GthanIAA <- c(iaa_F1GthanIAA, system_F1GthanIAA)
boxplot(iaa.and.f1_F1GthanIAA~labels1, ylim=c(0, 1.0), main="Agreement and F-measure ranges")


#summary statistics
summary(iaa_F1GthanIAA) # median = 0.75
summary(system_F1GthanIAA) #median = 0.8360

#Shapiro-Wilk normality test = null hypothesis = the data is normally distributed (p >0.05) 
#or not if p<0.05
shapiro.test(iaa_F1GthanIAA) #not normally distributed
shapiro.test(system_F1GthanIAA) #normally distributed

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

##graph of spearman's rank correlation
plot(rank(iaa_F1GthanIAA), rank(system_F1GthanIAA))
# Add fit lines
abline(lm(iaa_F1GthanIAA~system_F1GthanIAA), col="red") # regression line (y~x) 
lines(lowess(system_F1GthanIAA,iaa_F1GthanIAA), col="blue") # lowess line (x,y)



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


#summary statistics
summary(IAAGThanFmeasure$IAA) # median = 0.7647
summary(IAAGThanFmeasure$System) #median = 0.5865

#Shapiro-Wilk normality test = null hypothesis = the data is normally distributed (p >0.05) 
#or not if p<0.05
shapiro.test(IAAGThanFmeasure$IAA) #not normally distributed
shapiro.test(IAAGThanFmeasure$System) # not normally distributed


#Spearman's correlation
cor(IAAGThanFmeasure$IAA, IAAGThanFmeasure$System, method ="spearman") # positive = 0.6532331

# spearman's rank test
## positive correlation for iaa and system significant
cor.test(IAAGThanFmeasure$IAA, IAAGThanFmeasure$System, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho=0.6532331, p=1.449e-11<0.05


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


#summary statistics
summary(alldata$IAA) # median = 0.7504
summary(alldata$System) #median = 0.6380

#Shapiro-Wilk normality test = null hypothesis = the data is normally distributed (p >0.05) 
#or not if p<0.05
shapiro.test(alldata$IAA) #not normally distributed
shapiro.test(alldata$System) #not normally distributed

#Spearman's correlation
cor(alldata$IAA, alldata$System, method ="spearman") # positive = 0.5126484

# spearman's rank test
## positive correlation for iaa and system significant
cor.test(alldata$IAA, alldata$System, alternative = 'greater' , method="spearman", exact = TRUE, 
         conf.level = 0.95, continuity = FALSE) # rho=0.5126484, p=1.81e-08<0.05
