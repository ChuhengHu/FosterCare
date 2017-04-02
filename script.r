library(tidyr)
library(dplyr)
library(sas7bdat)
nasp = read.sas7bdat("NSAPpuf.sas7bdat")

ado <- read.csv("Adoption and Foster Care Analysis and Reporting System (AFCARS) - Adoption File 2015.csv") # upload the data file
adoNY <- filter(ado, St == "NY") # select data about New York State
adoNY.c.f <- select(adoNY, 5, 7, 8, 16, 23:25, 55, 56, 28, 29, 30, 31, 57, 58, 59) # select variables related to Child Profile and Foster Family Profile
adoNY.c.f$AdoptYear <- ifelse(grepl("/15 0:00", adoNY.c.f$AdoptDate), 2015, 2014) # convert AdoptDate into AdoptYear
adoNY.c.f$AdoptMomAge <- adoNY.c.f$AdoptYear - adoNY.c.f$ADMOMYR # calculate Mom's age when adopting
adoNY.c.f$AdoptDadAge <- adoNY.c.f$AdoptYear - adoNY.c.f$ADDADYR # calculate Dad's age when adopting
adoNY.c.f$YOB <- ifelse(grepl("/15 0:00", adoNY.c.f$DOB), 2015, ifelse(grepl("/14 0:00", adoNY.c.f$DOB), 2014, ifelse(grepl("/13 0:00", adoNY.c.f$DOB), 2013, ifelse(grepl("/12 0:00", adoNY.c.f$DOB), 2012, ifelse(grepl("/11 0:00", adoNY.c.f$DOB), 2011, ifelse(grepl("/10 0:00", adoNY.c.f$DOB), 2010, ifelse(grepl("/09 0:00", adoNY.c.f$DOB), 2009, ifelse(grepl("/08 0:00", adoNY.c.f$DOB), 2008, ifelse(grepl("/07 0:00", adoNY.c.f$DOB), 2007, ifelse(grepl("/06 0:00", adoNY.c.f$DOB), 2006, ifelse(grepl("/05 0:00", adoNY.c.f$DOB), 2005, ifelse(grepl("/04 0:00", adoNY.c.f$DOB), 2004, ifelse(grepl("/03 0:00", adoNY.c.f$DOB), 2003, ifelse(grepl("/02 0:00", adoNY.c.f$DOB), 2002, ifelse(grepl("/01 0:00", adoNY.c.f$DOB), 2001, ifelse(grepl("/00 0:00", adoNY.c.f$DOB), 2000, ifelse(grepl("/99 0:00", adoNY.c.f$DOB), 1999, ifelse(grepl("/98 0:00", adoNY.c.f$DOB), 1998, ifelse(grepl("/97 0:00", adoNY.c.f$DOB), 1997, ifelse(grepl("/96 0:00", adoNY.c.f$DOB), 1996, ifelse(grepl("/95 0:00", adoNY.c.f$DOB), 1995, 1994))))))))))))))))))))) # Child Year of Birth
adoNY.c.f$BioMomAgeAtBirth <- adoNY.c.f$YOB - adoNY.c.f$DOBYRMOM # calculate Bio Mom's age when giving birth to the child
adoNY.c.f$BioDadAgeAtBirth <- adoNY.c.f$YOB - adoNY.c.f$DOBYRDAD # calculate Bio Dad's age when giving birth to the child
adoNY.c.f.clean <- select(adoNY.c.f, RECNUM, SEX, AGYSPNDS, AgeAtAdopt, ChildRace, BioMomAgeAtBirth, MOMARRD, AdoptMomAge, AdoptDadAge, ADPFAMST, MomRace, DadRace, Transracial) # select variables related to Child Profile and Foster Family Profile after cleaning
adoNY.c <- select(adoNY.c.f.clean, 1:7)
adoNY.f <- select(adoNY.c.f.clean, 1, 8:13)
write.csv(adoNY.c.f.clean, "adoNY_c_f.csv", row.names = FALSE)
write.csv(adoNY.c, "adoNY_c.csv", row.names = FALSE)
write.csv(adoNY.f, "adoNY_f.csv", row.names = FALSE)

na.omit(adoNY.c.f) # delete rows with missing values


fos <- read.csv("Adoption and Foster Care Analysis and Reporting System (AFCARS) - Foster Care 2015.csv")
fosNY <- filter(fos, St == "NY")
fosNY.ado <- select(fosNY, 8, 9, 17, 25:43, 45:66, 75, 93, 99, 100, 102, 104)

adoFromfos <- select(fosNY.ado, RecNumbr, CURPLSET, DISREASN, IsWaiting, CASEGOAL)
strange <- filter(adoFromfos, CASEGOAL == 3 & IsWaiting == 0 & DISREASN == 0)

fosNY.ado$ado <- ifelse(fosNY.ado$DISREASN == 3, 1, ifelse(fosNY.ado$IsWaiting == 1, 0, 99))

fosNY.c.f <- filter(fosNY.ado, ado != 99)
fosNY.c.f$YOB <- ifelse(grepl("/05", fosNY.c.f$DOB), 2005, ifelse(grepl("/04", fosNY.c.f$DOB), 2004, ifelse(grepl("/03", fosNY.c.f$DOB), 2003, ifelse(grepl("/02", fosNY.c.f$DOB), 2002, ifelse(grepl("/01", fosNY.c.f$DOB), 2001, ifelse(grepl("/00", fosNY.c.f$DOB), 2000, ifelse(grepl("/99", fosNY.c.f$DOB), 1999, ifelse(grepl("/98", fosNY.c.f$DOB), 1998, ifelse(grepl("/97", fosNY.c.f$DOB), 1997, ifelse(grepl("/96", fosNY.c.f$DOB), 1996, ifelse(grepl("/95", fosNY.c.f$DOB), 1995, 1994)))))))))))# Child Year of Birth
fosNY.c.f$NumRemRsn <- fosNY.c.f$PHYABUSE + fosNY.c.f$SEXABUSE + fosNY.c.f$NEGLECT + fosNY.c.f$AAPARENT + fosNY.c.f$DAPARENT + fosNY.c.f$PRTSDIED + fosNY.c.f$PRTSJAIL + fosNY.c.f$NOCOPE + fosNY.c.f$ABANDMNT + fosNY.c.f$RELINQSH + fosNY.c.f$HOUSING + fosNY.c.f$AACHILD + fosNY.c.f$DACHILD + fosNY.c.f$CHILDIS + fosNY.c.f$CHBEHPRB
fosNY.c.f$RemRsnChil <- ifelse(fosNY.c.f$AACHILD == 1 | fosNY.c.f$DACHILD == 1 | fosNY.c.f$CHILDIS == 1 | fosNY.c.f$CHBEHPRB == 1, 1, 0)
fosNY.c.f$NumF1Race <- ifelse(fosNY.c.f$HOFCCTK1 == 0, fosNY.c.f$RF1AMAKN + fosNY.c.f$RF1ASIAN + fosNY.c.f$RF1BLKAA + fosNY.c.f$RF1NHOPI + fosNY.c.f$RF1WHITE, 1)
fosNY.c.f$NumF2Race <- ifelse(fosNY.c.f$HOFCCTK2 == 0, fosNY.c.f$RF2AMAKN + fosNY.c.f$RF2ASIAN + fosNY.c.f$RF2BLKAA + fosNY.c.f$RF2NHOPI + fosNY.c.f$RF2WHITE, 1)
fosNY.c.f$F1Race <- ifelse(fosNY.c.f$NumF1Race >= 2, 6, ifelse(fosNY.c.f$HOFCCTK1 == 1, 7, ifelse(fosNY.c.f$RF1WHITE == 1, 1, ifelse(fosNY.c.f$RF1BLKAA == 1, 2, ifelse(fosNY.c.f$RF1AMAKN==1, 3, ifelse(fosNY.c.f$RF1ASIAN==1, 4, ifelse(fosNY.c.f$RF1NHOPI==1, 5, 99)))))))
fosNY.c.f$F2Race <- ifelse(fosNY.c.f$NumF2Race >= 2, 6, ifelse(fosNY.c.f$HOFCCTK2 == 1, 7, ifelse(fosNY.c.f$RF2WHITE == 1, 1, ifelse(fosNY.c.f$RF2BLKAA == 1, 2, ifelse(fosNY.c.f$RF2AMAKN==1, 3, ifelse(fosNY.c.f$RF2ASIAN==1, 4, ifelse(fosNY.c.f$RF2NHOPI==1, 5, 99)))))))
fosNY.c.f$P1AgeAtBirth <- fosNY.c.f$YOB-fosNY.c.f$CTK1YR
fosNY.c.f$F1Age <- fosNY.c.f$YOB-fosNY.c.f$FCCTK1YR+fosNY.c.f$AgeAtEnd
fosNY.c.f$F2Age <- fosNY.c.f$YOB-fosNY.c.f$FCCTK2YR+fosNY.c.f$AgeAtEnd
fosNY.c.f$Transracial <- ifelse(fosNY.c.f$RaceEthn == 99 | (fosNY.c.f$F1Race == 99 & fosNY.c.f$F2Race == 99), 9, ifelse(fosNY.c.f$RaceEthn == fosNY.c.f$F1Race | fosNY.c.f$RaceEthn == fosNY.c.f$F2Race, 0, ifelse(fosNY.c.f$RaceEthn == 1, 2, ifelse(fosNY.c.f$F1Race == 1 | fosNY.c.f$F2Race == 1, 1, 3))))
fosNY.c.f.clean <- select(fosNY.c.f, RecNumbr, SEX, CLINDIS, AgeAtEnd, RaceEthn, P1AgeAtBirth, CTKFAMST, TOTALREM, NUMPLEP, MANREM, RemRsnChil, IsTPR, ado, F1Age, F2Age, FOSFAMST, F1Race, F2Race, Transracial)
fosNY.c <- select(fosNY.c.f.clean, 1:13)
fosNY.f <- select(fosNY.c.f.clean, 1, 14:19)
write.csv(fosNY.c.f.clean, "fosNY_c_f.csv", row.names = FALSE)
write.csv(fosNY.c, "fosNY_c.csv", row.names = FALSE)
write.csv(fosNY.f, "fosNY_f.csv", row.names = FALSE)


fosNY.f1 <- select(fosNY.c.f.clean, 1, 13:19)
fosNY.f1$FOSFAMST1 <- ifelse(fosNY.f1$FOSFAMST==1, 1, 0)
fosNY.f1$FOSFAMST2 <- ifelse(fosNY.f1$FOSFAMST==2, 1, 0)
fosNY.f1$FOSFAMST3 <- ifelse(fosNY.f1$FOSFAMST==3, 1, 0)
fosNY.f1$FOSFAMST4 <- ifelse(fosNY.f1$FOSFAMST==4, 1, 0)

fosNY.f1$F1Race1 <- ifelse(fosNY.f1$F1Race==1, 1, 0)
fosNY.f1$F1Race2 <- ifelse(fosNY.f1$F1Race==2, 1, 0)
fosNY.f1$F1Race3 <- ifelse(fosNY.f1$F1Race==3, 1, 0)
fosNY.f1$F1Race4 <- ifelse(fosNY.f1$F1Race==4, 1, 0)
fosNY.f1$F1Race5 <- ifelse(fosNY.f1$F1Race==5, 1, 0)
fosNY.f1$F1Race6 <- ifelse(fosNY.f1$F1Race==6, 1, 0)
fosNY.f1$F1Race7 <- ifelse(fosNY.f1$F1Race==7, 1, 0)

fosNY.f1$F2Race1 <- ifelse(fosNY.f1$F2Race==1, 1, 0)
fosNY.f1$F2Race2 <- ifelse(fosNY.f1$F2Race==2, 1, 0)
fosNY.f1$F2Race3 <- ifelse(fosNY.f1$F2Race==3, 1, 0)
fosNY.f1$F2Race4 <- ifelse(fosNY.f1$F2Race==4, 1, 0)
fosNY.f1$F2Race5 <- ifelse(fosNY.f1$F2Race==5, 1, 0)
fosNY.f1$F2Race6 <- ifelse(fosNY.f1$F2Race==6, 1, 0)
fosNY.f1$F2Race7 <- ifelse(fosNY.f1$F2Race==7, 1, 0)

fosNY.f1$Transracial0 <- ifelse(fosNY.f1$Transracial==0, 1, 0)
fosNY.f1$Transracial1 <- ifelse(fosNY.f1$Transracial==1, 1, 0)
fosNY.f1$Transracial2 <- ifelse(fosNY.f1$Transracial==2, 1, 0)
fosNY.f1$Transracial3 <- ifelse(fosNY.f1$Transracial==3, 1, 0)

fosNY.f2 <- select(fosNY.f1, 1:4, 9:30)
fosNY.f3 <- select(fosNY.f2, 1:3, 5:15, 23:26) # only F1

library(rpart)
c.tree <- rpart(factor(ado) ~ F1Age + F2Age + FOSFAMST1 + FOSFAMST2 +  FOSFAMST3 + FOSFAMST4 + F1Race1 + F2Race1 + F1Race2 + F2Race2 + F1Race3 + F2Race3 + F1Race4 + F2Race4 + F1Race5 + F2Race5 + F1Race6 + F2Race6 + F1Race7 + F2Race7 + Transracial0 + Transracial1 + Transracial2 + Transracial3, method="class", data=fosNY.f2) # build the tree
post(c.tree, file = "ctree(rpart).ps", title = "the classification tree of engagement") # plot the tree
# build a classification tree with "party" as follows
library(party)
c.tree2 <- ctree(factor(ado) ~ F1Age + F2Age + FOSFAMST1 + FOSFAMST2 +  FOSFAMST3 + FOSFAMST4 + F1Race1 + F2Race1 + F1Race2 + F2Race2 + F1Race3 + F2Race3 + F1Race4 + F2Race4 + F1Race5 + F2Race5 + F1Race6 + F2Race6 + F1Race7 + F2Race7 + Transracial0 + Transracial1 + Transracial2 + Transracial3, fosNY.f2) # build the tree
plot(c.tree2) # plot the tree

fosNY.f2$prediction <- predict(c.tree, fosNY.f2, type = "class") # make a prediction on the training set, Session1, with the "rpart" tree
fosNY.f2$prediction2 <- predict(c.tree2, fosNY.f2)
mean(fosNY.f2$ado == fosNY.f2$prediction) * 100 # calculate the goodness of fit for the "rpart" tree (76.18847%)
mean(fosNY.f2$ado == fosNY.f2$prediction2) * 100 # calculate the goodness of fit for the "party" tree (76.39882%)

adoNY.f1 <- adoNY.f
adoNY.f1$ado <- 1
adoNY.f1$FOSFAMST1 <- ifelse(adoNY.f1$ADPFAMST==1, 1, 0)
adoNY.f1$FOSFAMST2 <- ifelse(adoNY.f1$ADPFAMST==2, 1, 0)
adoNY.f1$FOSFAMST3 <- ifelse(adoNY.f1$ADPFAMST==3, 1, 0)
adoNY.f1$FOSFAMST4 <- ifelse(adoNY.f1$ADPFAMST==4, 1, 0)

adoNY.f1$F1Race1 <- ifelse(adoNY.f1$MomRace==1, 1, 0)
adoNY.f1$F1Race2 <- ifelse(adoNY.f1$MomRace==2, 1, 0)
adoNY.f1$F1Race3 <- ifelse(adoNY.f1$MomRace==3, 1, 0)
adoNY.f1$F1Race4 <- ifelse(adoNY.f1$MomRace==4, 1, 0)
adoNY.f1$F1Race5 <- ifelse(adoNY.f1$MomRace==5, 1, 0)
adoNY.f1$F1Race6 <- ifelse(adoNY.f1$MomRace==6, 1, 0)
adoNY.f1$F1Race7 <- ifelse(adoNY.f1$MomRace==7, 1, 0)

adoNY.f1$F2Race1 <- ifelse(adoNY.f1$DadRace==1, 1, 0)
adoNY.f1$F2Race2 <- ifelse(adoNY.f1$DadRace==2, 1, 0)
adoNY.f1$F2Race3 <- ifelse(adoNY.f1$DadRace==3, 1, 0)
adoNY.f1$F2Race4 <- ifelse(adoNY.f1$DadRace==4, 1, 0)
adoNY.f1$F2Race5 <- ifelse(adoNY.f1$DadRace==5, 1, 0)
adoNY.f1$F2Race6 <- ifelse(adoNY.f1$DadRace==6, 1, 0)
adoNY.f1$F2Race7 <- ifelse(adoNY.f1$DadRace==7, 1, 0)

adoNY.f1$Transracial0 <- ifelse(adoNY.f1$Transracial==0, 1, 0)
adoNY.f1$Transracial1 <- ifelse(adoNY.f1$Transracial==1, 1, 0)
adoNY.f1$Transracial2 <- ifelse(adoNY.f1$Transracial==2, 1, 0)
adoNY.f1$Transracial3 <- ifelse(adoNY.f1$Transracial==3, 1, 0)

adoNY.f2 <- select(adoNY.f1, 1, 8:30)
adoNY.f2$F1Age <- adoNY.f1$AdoptMomAge
adoNY.f2$F2Age <- adoNY.f1$AdoptDadAge

adoNY.f2$prediction <- predict(c.tree, adoNY.f2, type = "class") # make a prediction on the training set, Session1, with the "rpart" tree
adoNY.f2$prediction2 <- predict(c.tree2, adoNY.f2)
mean(adoNY.f2$ado == adoNY.f2$prediction) * 100 # calculate the goodness of fit for the "rpart" tree (76.18847%)
mean(adoNY.f2$ado == adoNY.f2$prediction2) * 100 

NY.f.i <- select(adoNY.f2, 1, 25:26, 3:24, 2)
library(data.table)
setnames(NY.f.i, old = c('RECNUM'), new = c('RecNumbr'))
NY.f.2 <- select(fosNY.f2, 1, 3:26, 2)
NY.f <- rbind(NY.f.i, NY.f.2)

NY.f.1 <- NY.f
NY.f.1$prediction <- predict(c.tree2, NY.f.1)
mean(NY.f.1$ado == NY.f.1$prediction) * 100 #(42.5522%)

c.tree2.2 <- ctree(factor(ado) ~ F1Age + F2Age + FOSFAMST1 + FOSFAMST2 +  FOSFAMST3 + FOSFAMST4 + F1Race1 + F2Race1 + F1Race2 + F2Race2 + F1Race3 + F2Race3 + F1Race4 + F2Race4 + F1Race5 + F2Race5 + F1Race6 + F2Race6 + F1Race7 + F2Race7 + Transracial0 + Transracial1 + Transracial2 + Transracial3, NY.f) # build the tree
plot(c.tree2.2)
NY.f.1$prediction2 <- predict(c.tree2.2, NY.f.1)
mean(NY.f.1$ado == NY.f.1$prediction2) * 100 #(66.98376%)

## 75% of the sample size
smp_size <- floor(0.75 * nrow(NY.f))
## set the seed to make your partition reproductible
set.seed(555)
train_ind <- sample(seq_len(nrow(NY.f)), size = smp_size)
## get the training set and the test set
train1 <- NY.f[train_ind, ]
test1 <- NY.f[-train_ind, ]

c.tree2.3 <- ctree(factor(ado) ~ F1Age + F2Age + FOSFAMST1 + FOSFAMST2 +  FOSFAMST3 + FOSFAMST4 + F1Race1 + F2Race1 + F1Race2 + F2Race2 + F1Race3 + F2Race3 + F1Race4 + F2Race4 + F1Race5 + F2Race5 + F1Race6 + F2Race6 + F1Race7 + F2Race7 + Transracial0 + Transracial1 + Transracial2 + Transracial3, train1) # build the tree
plot(c.tree2.3)
train1$prediction <- predict(c.tree2.3, train1)
mean(train1$ado == train1$prediction) * 100
test1$prediction <- predict(c.tree2.3, test1)
mean(test1$ado == test1$prediction) * 100

png("c.tree2.3.png", res=80, height=800, width=1600) 
plot(c.tree2.3) 
dev.off()

NY.ff.2 <- select(fosNY.c.f, RecNumbr, F1Age, F2Age, FOSFAMST, RF1WHITE, RF1BLKAA, RF1AMAKN, RF1ASIAN, RF1NHOPI, HOFCCTK1, RF2WHITE, RF2BLKAA, RF2AMAKN, RF2ASIAN, RF2NHOPI, HOFCCTK2, Transracial, ado)

NY.ff.2$FOSFAMST1 <- ifelse(NY.ff.2$FOSFAMST==1, 1, 0)
NY.ff.2$FOSFAMST2 <- ifelse(NY.ff.2$FOSFAMST==2, 1, 0)
NY.ff.2$FOSFAMST3 <- ifelse(NY.ff.2$FOSFAMST==3, 1, 0)
NY.ff.2$FOSFAMST4 <- ifelse(NY.ff.2$FOSFAMST==4, 1, 0)

NY.ff.2$Transracial0 <- ifelse(NY.ff.2$Transracial==0, 1, 0)
NY.ff.2$Transracial1 <- ifelse(NY.ff.2$Transracial==1, 1, 0)
NY.ff.2$Transracial2 <- ifelse(NY.ff.2$Transracial==2, 1, 0)
NY.ff.2$Transracial3 <- ifelse(NY.ff.2$Transracial==3, 1, 0)

NY.ff.2.clean <- select(NY.ff.2, RecNumbr, F1Age, F2Age, FOSFAMST1, FOSFAMST2, FOSFAMST3, FOSFAMST4, 5:16, 23:26, ado)

NY.ff.1 <- select(adoNY, RECNUM, ADPFAMST, WHITEM, BLAFRAMM, AMIAKNM, ASIANM, HAWIIPIM, HOMOM, WHITED, BLAFRAMD, AMIAKND, ASIAND, HAWIIPID, HODAD, Transracial)
setnames(NY.ff.1, old='RECNUM', new='RecNumbr')
d2 <- select(NY.f.i, RecNumbr, F1Age, F2Age, ado)
NY.ff.1 <- left_join(NY.ff.1, d2, by="RecNumbr")

NY.ff.1$FOSFAMST1 <- ifelse(NY.ff.1$ADPFAMST==1, 1, 0)
NY.ff.1$FOSFAMST2 <- ifelse(NY.ff.1$ADPFAMST==2, 1, 0)
NY.ff.1$FOSFAMST3 <- ifelse(NY.ff.1$ADPFAMST==3, 1, 0)
NY.ff.1$FOSFAMST4 <- ifelse(NY.ff.1$ADPFAMST==4, 1, 0)

NY.ff.1$Transracial0 <- ifelse(NY.ff.1$Transracial==0, 1, 0)
NY.ff.1$Transracial1 <- ifelse(NY.ff.1$Transracial==1, 1, 0)
NY.ff.1$Transracial2 <- ifelse(NY.ff.1$Transracial==2, 1, 0)
NY.ff.1$Transracial3 <- ifelse(NY.ff.1$Transracial==3, 1, 0)

NY.ff.1.clean <- select(NY.ff.1, RecNumbr, F1Age, F2Age, FOSFAMST1, FOSFAMST2, FOSFAMST3, FOSFAMST4, 3:14, 23:26, ado)
names(NY.ff.1.clean) <- names(NY.ff.2.clean)

NY.ff <- rbind(NY.ff.1.clean, NY.ff.2.clean)
NY.ff$HOFCCTK1 <- ifelse(NY.ff$HOFCCTK1 == 1, 1, ifelse(NY.ff$HOFCCTK1 == 2, 0, NA))
NY.ff$HOFCCTK2 <- ifelse(NY.ff$HOFCCTK2 == 1, 1, ifelse(NY.ff$HOFCCTK2 == 2, 0, NA))
names(NY.ff) <- c("RecNumbe", "F1Age", "F2Age", "MarriedCouple", "UnmarriedCouple", "SingleFemale", "SingleMale", "F1White", "F1Black", "F1NativeAmer", "F1Asian", "F1HawaiiP", "F1Hispanic", "F2White", "F2Black", "F2NativeAmer", "F2Asian", "F2HawaiiP", "F2Hispanic", "SameRaces", "KidNotWhParentWh", "KidWhParentNotWh", "OtherTrans", "Adopt")

smp_size_ff <- floor(0.75 * nrow(NY.ff))
set.seed(123)
train_ind_ff <- sample(seq_len(nrow(NY.ff)), size = smp_size_ff)
## get the training set and the test set
train2 <- NY.ff[train_ind_ff, ]
test2 <- NY.ff[-train_ind_ff, ]

c.tree.ff <- ctree(factor(Adopt) ~ F1Age + F2Age + MarriedCouple + UnmarriedCouple + SingleFemale + SingleMale + F1White + F1Black + F1NativeAmer + F1Asian + F1HawaiiP + F1Hispanic + F2White + F2Black + F2NativeAmer + F2Asian + F2HawaiiP + F2Hispanic + SameRaces + KidNotWhParentWh + KidWhParentNotWh + OtherTrans, train2) # build the tree
plot(c.tree.ff)
train2$prediction <- predict(c.tree.ff, train2)
mean(train2$Adopt == train2$prediction) * 100 #78.0708
test2$prediction <- predict(c.tree.ff, test2)
mean(test2$Adopt == test2$prediction) * 100 #75.33856

png("c.tree.ff.png", res=80, height=1000, width=1900) 
plot(c.tree.ff) 
dev.off()

c.tree.ff2 <- rpart(factor(Adopt) ~ F1Age+F2Age+MarriedCouple+UnmarriedCouple+SingleFemale+SingleMale+F1White+F1Black+F1NativeAmer+F1Asian+F1HawaiiP+F1Hispanic+F2White+F2Black+F2NativeAmer+F2Asian+F2HawaiiP+F2Hispanic+SameRaces+KidNotWhParentWh+KidWhParentNotWh+OtherTrans, method="class", data=train2)
post(c.tree.ff2, file = "c.tree.ff2.ps", title = "the classification tree")
train2$prediction2 <- predict(c.tree.ff2, train2,  type = "class")
mean(train2$Adopt == train2$prediction2) * 100 # 77.7857
test2$prediction2 <- predict(c.tree.ff2, test2,  type = "class")
mean(test2$Adopt == test2$prediction2) * 100 # 75.55239




c.tree.ff.new <- ctree(factor(Adopt) ~ F1Age + MarriedCouple + UnmarriedCouple + SingleFemale + SingleMale + F1White + F1Black + F1NativeAmer + F1Asian + F1HawaiiP + F1Hispanic, train2) # build the tree
plot(c.tree.ff.new)
train2$prediction.new <- predict(c.tree.ff.new, train2)
mean(train2$Adopt == train2$prediction.new) * 100 # 69.30387
test2$prediction.new <- predict(c.tree.ff.new, test2)
mean(test2$Adopt == test2$prediction.new) * 100 # 65.28867

png("c.tree.ff.new.png", res=80, height=1000, width=1900) 
plot(c.tree.ff.new) 
dev.off()

c.tree.ff2.new <- rpart(factor(Adopt) ~ F1Age + MarriedCouple + UnmarriedCouple + SingleFemale + SingleMale + F1White + F1Black + F1NativeAmer + F1Asian + F1HawaiiP + F1Hispanic, method="class", data=train2)
post(c.tree.ff2.new, file = "c.tree.ff2.new.ps", title = "the classification tree", horizontal = FALSE)
train2$prediction2.new <- predict(c.tree.ff2.new, train2,  type = "class")
mean(train2$Adopt == train2$prediction2.new) * 100 # 75.26728
test2$prediction2.new <- predict(c.tree.ff2.new, test2,  type = "class")
mean(test2$Adopt == test2$prediction2.new) * 100 # 72.5588


#cluster (just try)
library(klaR)
adoNY.c <- filter(adoNY.c, ChildRace != 99) # delete rows with uncertain values
adoNY.c <- select(adoNY.c.f.noNA, 2:6) # select variables related to Child Profile
WDiffvector <- c(0,0,0,0,0,0,0,0,0,0)
for (i in 1:10)
{ cl.c <- kmodes(adoNY.c, i, iter.max = 20, weighted = FALSE)
WDiffvector[i] <- cl.c$withindiff
}
plot(WDiffvector,type="l", xlab="# clusters", ylab="WDiff")

