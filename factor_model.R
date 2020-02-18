load('processedData_yb.Rdata')
apply(features_complete,2,function(x)sum(is.na(x)))

cas01 <- read.csv("data/HarvardCAS01.csv")
sectionb_names <- c(paste0("B", c(1, 3, 5)), "B19",
                    colnames(features_complete)[grepl("B21", colnames(features_complete))])
sectionc_names <- c(paste0("C", c(1:2,7, 10:13)),
                    colnames(features_complete)[grepl("C17", colnames(features_complete))],
                    colnames(features_complete)[grepl("C20", colnames(features_complete))],
                    colnames(features_complete)[grepl("C22", colnames(features_complete))]
)
predictor_names <- c(paste0("A", 1:5), 
                     colnames(features_complete)[grepl("A6", colnames(features_complete))],
                     colnames(features_complete)[grepl("A7", colnames(features_complete))],
                     colnames(features_complete)[grepl("A8", colnames(features_complete))], "F5", 
                     colnames(features_complete)[grepl("G1", colnames(features_complete))], "G2",
                     colnames(features_complete)[grepl("G3", colnames(features_complete))],
                     colnames(features_complete)[grepl("G4", colnames(features_complete))],
                     colnames(features_complete)[grepl("G5", colnames(features_complete))],
                     "G7", "G8", "G13")[-11] # We don't want E29A7!

#features_complete$A6 = as.factor(features_complete$A6)
#features_complete$G1 = as.factor(features_complete$G1)
#features_complete$G4 = as.factor(features_complete$G4)


library(lavaan)
library(fields)

apply(group1,2,function(x)sum(is.na(x)))
apply(group2,2,function(x)sum(is.na(x)))
apply(group3,2,function(x)sum(is.na(x)))
apply(group4,2,function(x)sum(is.na(x)))

apply(group1[,sectionc_names],2,function(x)sum(is.na(x)))

image.plot(cor(group1[complete.cases(group1[,sectionb_names]),sectionb_names]))

g1modb1 = paste('b1 =~ ',paste(sectionb_names, collapse= ' + '))
g1modb2 = paste(paste('b1 =~ ',paste(c('B3',sectionb_names[-which(sectionb_names == 'B3' | sectionb_names == 'B21A')]), collapse= ' + ')),
               paste('b2 =~ ',paste(c('B21A',sectionb_names[-which(sectionb_names == 'B3' | sectionb_names == 'B21A')]), collapse= ' + ')),sep ="\n")

test1 = cfa(g1modb1,data = group1)
test2 = cfa(g1modb2,data = group1)

summary(test2,fit.measures=TRUE)

fitMeasures(test2, c('cfi','rmsea','srmr'))

#lam1 = inspect(test,'est')$lam
#the1 = inspect(test,'est')$the
#psi1 = inspect(test,'est')$psi
#ydat = group1[,sectionb_names]

#formula from textbook to get factor estimates
#f1 = psi1%*%t(lam1)%*%solve(lam1%*%psi1%*%t(lam1)+the1)%*%t(ydat)


image.plot(cor(features_complete[complete.cases(features_complete[,sectionc_names]),sectionc_names]))


g1modc1 = paste('c1 =~ ',paste(sectionc_names, collapse= ' + '))
g1modc2 = paste(paste('c1 =~ ',paste(c('C1',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C22A')]), collapse= ' + ')),
                paste('c2 =~ ',paste(c('C22A',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C22A')]), collapse= ' + ')),sep ="\n")
g1modc3 = paste(paste('c1 =~ ',paste(c('C1',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C20A' | sectionc_names == 'C22A')]), collapse= ' + ')),
                paste('c2 =~ ',paste(c('C20A',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C20A' | sectionc_names == 'C22A')]), collapse= ' + ')),
                paste('c3 =~ ',paste(c('C22A',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C20A' | sectionc_names == 'C22A')]), collapse= ' + ')),sep ="\n")


testc1 = cfa(g1modc1,data=group1)
testc2 = cfa(g1modc2,data=features_complete)
testc3 = cfa(g1modc3,data=features_complete)

fitMeasures(testc2, c('cfi','rmsea','srmr'))

summary(testc3,fit.measures=T)
fitMeasures(testc3, c('cfi','rmsea','srmr'))

#combined CFA (still need to fit SEM)

g1bcmod = paste(g1modb2,g1modc3,sep='\n')
testbc = cfa(g1bcmod,data=features_complete)
summary(testbc,fit.measures=T)
fitMeasures(testbc, c('cfi','rmsea','srmr'))

semmod = paste(g1bcmod,
               paste('b1 ~ ',paste(predictor_names, collapse= ' + ')),
               paste('b2 ~ ',paste(predictor_names, collapse= ' + ')),
               paste('c1 ~ ',paste(predictor_names, collapse= ' + ')),
               paste('c2 ~ ',paste(predictor_names, collapse= ' + ')),
               paste('c3 ~ ',paste(predictor_names, collapse= ' + ')),
               sep = '\n')
cat(semmod)

semtest = sem(semmod,data=features_complete)
semtest2 = sem(semmod,data=features_complete,sampling.weights = 'weights')
summary(semtest,fit.measures=T)
fitMeasures(semtest2, c('cfi','rmsea','srmr'))

save(semtest,file='sem_fit1.Rdata')
save(semtest2,file='sem_fit2.Rdata')

sem_sum = summary(semtest2)
summary(semtest2)

print(sem_sum)
save(sem_sum,file='sem_sum.Rdata')

sem_cov = inspect(semtest2,'est')$psi[1:5,1:5]
png('factor_cor.png')
image.plot(cov2cor(sem_cov),zlim=c(-1,1),xaxt='n',yaxt='n')
axis(1,at=seq(0,1,length=5),labels = c('b1','b2','c1','c2','c3'))
axis(2,at=seq(0,1,length=5),labels = c('b1','b2','c1','c2','c3'))
dev.off()
