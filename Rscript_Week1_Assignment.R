##To read the ESS Round 10 data in.
ESS10<-read.csv("./Week 1/ESS10.csv")

##Get an idea of the dataset
dim(ESS10)
names(ESS10)
table(ESS10$cntry)

##Subset the data for only Belgium
BE_ESS10<-subset(ESS10, cntry %in% "BE")

##Explore the variables related to age, health status, and religiosity
##Operationalize accordingly
class(BE_ESS10$agea)

class(BE_ESS10$health)
BE_ESS10$healthCat<-sub(1,"Very good", BE_ESS10$health)
BE_ESS10$healthCat<-sub(2,"Good", BE_ESS10$healthCat)
BE_ESS10$healthCat<-sub(3,"Fair", BE_ESS10$healthCat)
BE_ESS10$healthCat<-sub(4,"Bad", BE_ESS10$healthCat)
BE_ESS10$healthCat<-sub(5,"Very bad", BE_ESS10$healthCat)
BE_ESS10$healthCat<-factor(BE_ESS10$healthCat,
                           levels = c("Very bad","Bad","Fair","Good","Very good"))
table(BE_ESS10$healthCat)

class(BE_ESS10$rlgdgr)

##Plotting
#age
table(BE_ESS10$agea)
grep("999",BE_ESS10$agea)
BE_ESS10_age<-select(BE_ESS10,agea)
BE_ESS10_age<-arrange(BE_ESS10_age,agea)
BE_ESS10_age<-BE_ESS10_age[1:1339,]
hist(BE_ESS10_age,main="Distribution of Age", xlab="age",
     ylab="frequency",col="orange",breaks = 40)
abline(v=median(BE_ESS10_age),col="blue",lwd=2)


#health status
barplot(table(BE_ESS10$healthCat),main = "distribution of health status",
        xlab="health status", ylab="frequency", col="lightblue")

#religiosity
BE_ESS10_religious<-select(BE_ESS10,rlgdgr)
BE_ESS10_religious<-arrange(BE_ESS10_religious,rlgdgr)
BE_ESS10_religious<-BE_ESS10_religious[1:1339,]
hist(BE_ESS10_religious,main="distribution of religiosity", xlab="How religious are you",
     ylab="frequnecy", col="lightgreen",xlim = c(-1,10),breaks = 11)
abline(v=mean(BE_ESS10_religious),col="blue",lwd=2)


##ANOVA between health and age
BE_ESS10<-arrange(BE_ESS10,agea)
BE_ESS10<-BE_ESS10[1:1339,]
table(BE_ESS10$healthCat)
health_ageMean<-tapply(BE_ESS10$agea,BE_ESS10$healthCat,mean,na.rm=TRUE)
health_ageMean
health_age_ANOVA<-aov(agea~healthCat,data=BE_ESS10)
summary(health_age_ANOVA)
pairwise.t.test(BE_ESS10$agea,BE_ESS10$healthCat, p.adjust.method = "bonferroni")

##Pearson correlation between religiosity and age
BE_ESS10<-arrange(BE_ESS10,rlgdgr)
tail(BE_ESS10$rlgdgr)
dim(BE_ESS10)
BE_ESS10<-BE_ESS10[1:1337,]
cor.test(x=BE_ESS10$rlgdgr,BE_ESS10$agea,method=c("pearson"))

##ANOVA between religiosity and health
health_reliMean<-tapply(BE_ESS10$rlgdgr,BE_ESS10$healthCat,mean,na.rm=TRUE)
health_reliMean
health_reli_ANOVA<-aov(rlgdgr~healthCat,data=BE_ESS10)
summary(health_reli_ANOVA)


##linear regression model
lm_AgeHealthOnReligion<-lm(rlgdgr~agea+healthCat,data=BE_ESS10)
summary(lm_AgeHealthOnReligion)
