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


image.plot(cor(group1[complete.cases(group1[,sectionc_names]),sectionc_names]))


g1modc1 = paste('c1 =~ ',paste(sectionc_names, collapse= ' + '))
g1modc2 = paste(paste('c1 =~ ',paste(c('C1',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C22A')]), collapse= ' + ')),
                paste('c2 =~ ',paste(c('C22A',sectionc_names[-which(sectionc_names == 'C1' | sectionc_names == 'C22A')]), collapse= ' + ')),sep ="\n")

testc1 = cfa(g1modc1,data=group1)
testc2 = cfa(g1modc2,data=group1)

summary(testc2,fit.measures=T)
fitMeasures(testc2, c('cfi','rmsea','srmr'))

#combined CFA (still need to fit SEM)

g1bcmod = paste(g1modb2,g1modc2,sep='\n')
testbc = cfa(g1bcmod,data=group1)
summary(testbc,fit.measures=T)
fitMeasures(testbc, c('cfi','rmsea','srmr'))
