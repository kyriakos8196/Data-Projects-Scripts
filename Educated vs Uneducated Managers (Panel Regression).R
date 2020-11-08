##Jing Cai, Adam Szeidl, Interfirm Relationships and Business Performance, The Quarterly Journal of Economics, Volume 133, Issue 3, August 2018, Pages 1229-1282, https://doi.org/10.1093/qje/qjx049 

##Role of education on interfirm relationships and business performance
rm(list=ls())

### Packages
library(gridExtra)
library(foreign)
library(stargazer)
library(haven)
library(Hmisc)
library(lattice)
library(dummies)
library(plm)

### load data
setwd("C:/Users/Kyriakos/Desktop/Business")
main = read_dta("Main.dta")
main_start=subset(main,after1==0 & after2==0)
main_mid = subset(main,round==2)

#T-Tests

#Before - Control vs treatment

t.test(subset(main_start$lnwfixasset,main_start$treatment==1&main_start$educ_college==1),
       subset(main_start$lnwfixasset,main_start$treatment==0&main_start$educ_college==1),alternative="two.sided")

t.test(subset(main_start$lnwfixasset,main_start$treatment==1&main_start$educ_college==0),
       subset(main_start$lnwfixasset,main_start$treatment==0&main_start$educ_college==0),alternative="two.sided")

t.test(subset(main_start$lnwnum_clients,main_start$treatment==1&main_start$educ_college==1),
       subset(main_start$lnwnum_clients,main_start$treatment==0&main_start$educ_college==1),alternative="two.sided")

t.test(subset(main_start$lnwnum_clients,main_start$treatment==1&main_start$educ_college==0),
       subset(main_start$lnwnum_clients,main_start$treatment==0&main_start$educ_college==0),alternative="two.sided")

t.test(subset(main_start$lnwpart5revenue,main_start$treatment==1&main_start$educ_college==1),
       subset(main_start$lnwpart5revenue,main_start$treatment==0&main_start$educ_college==1),alternative="two.sided")

t.test(subset(main_start$lnwpart5revenue,main_start$treatment==1&main_start$educ_college==0),
       subset(main_start$lnwpart5revenue,main_start$treatment==0&main_start$educ_college==0),alternative="two.sided")

#After

#Control vs Treatment
t.test(subset(main_mid$lnwpart5revenue,main_mid$treatment==1&main_mid$educ_college==1),
       subset(main_mid$lnwpart5revenue,main_mid$treatment==0&main_mid$educ_college==1),alternative="two.sided")

t.test(subset(main_mid$lnwpart5revenue,main_mid$treatment==1&main_mid$educ_college==0),
       subset(main_mid$lnwpart5revenue,main_mid$treatment==0&main_mid$educ_college==0),alternative="two.sided")

t.test(subset(main_mid$lnwfixasset,main_mid$treatment==1&main_mid$educ_college==1),
       subset(main_mid$lnwfixasset,main_mid$treatment==0&main_mid$educ_college==1),alternative="two.sided")

t.test(subset(main_mid$lnwfixasset,main_mid$treatment==1&main_mid$educ_college==0),
       subset(main_mid$lnwfixasset,main_mid$treatment==0&main_mid$educ_college==0),alternative="two.sided")

t.test(subset(main_mid$lnwnum_clients,main_mid$treatment==1&main_mid$educ_college==1),
       subset(main_mid$lnwnum_clients,main_mid$treatment==0&main_mid$educ_college==1),alternative="two.sided")

t.test(subset(main_mid$lnwnum_clients,main_mid$treatment==1&main_mid$educ_college==0),
       subset(main_mid$lnwnum_clients,main_mid$treatment==0&main_mid$educ_college==0),alternative="two.sided")

#Educated vs uneducated
mean(subset(main_mid$lnwpart5revenue,main_mid$treatment==1&main_mid$educ_college==1))
mean(subset(main_mid$lnwpart5revenue,main_mid$treatment==1&main_mid$educ_college==0))

t.test(subset(main_mid$lnwpart5revenue,main_mid$treatment==1&main_mid$educ_college==1),
       subset(main_mid$lnwpart5revenue,main_mid$treatment==1&main_mid$educ_college==0),alternative = "two.sided" )


mean(subset(main_mid$lnwfixasset,main_mid$treatment==1&main_mid$educ_college==1))
mean(subset(main_mid$lnwfixasset,main_mid$treatment==1&main_mid$educ_college==0))

t.test(subset(main_mid$lnwfixasset,main_mid$treatment==1&main_mid$educ_college==1),
       subset(main_mid$lnwfixasset,main_mid$treatment==1&main_mid$educ_college==0),alternative = "two.sided")

mean(subset(main_mid$lnwnum_clients,main_mid$treatment==1&main_mid$educ_college==1),na.rm=TRUE)
mean(subset(main_mid$lnwnum_clients,main_mid$treatment==1&main_mid$educ_college==0),na.rm=TRUE)

t.test(subset(main_mid$lnwnum_clients,main_mid$treatment==1&main_mid$educ_college==1),
       subset(main_mid$lnwnum_clients,main_mid$treatment==1&main_mid$educ_college==0),alternative = "two.sided")


main_educ = subset(main,main$educ_college==1)
main_noeduc = subset(main,main$educ_college==0)
main_start_educ=subset(main_educ,after1==0 & after2==0)
main_start_noeduc=subset(main_noeduc,after1==0 & after2==0)
main_mid_educ=subset(main_educ,round==2)
main_mid_noeduc=subset(main_noeduc,round==2)


#Histograms of before
main_educ_start=within(main_start_educ,{D_group=ifelse(main_start_educ$treatment==1, "Educated-treatment", "Educated-control")})
par(mfrow=c(2,1))
hist1 = histogram(~ lnwpart5revenue | D_group, data=subset(main_educ_start,treatment==1), nint =50)
hist2 = histogram(~ lnwpart5revenue | D_group, data=subset(main_educ_start,treatment==0), nint =50)
grid.arrange(hist1,hist2, ncol=2)

main_noeduc_start=within(main_start_noeduc,{D_group=ifelse(main_start_noeduc$treatment==1, "Uneducated-treatment", "Uneducated-control")})
par(mfrow=c(2,1))
hist3 = histogram(~ lnwpart5revenue |D_group, data=subset(main_noeduc_start,treatment==1), nint =50)
hist4 = histogram(~ lnwpart5revenue | D_group, data=subset(main_noeduc_start,treatment==0), nint =50)
grid.arrange(hist3,hist4, ncol=2)


#Density plots of midline
main_educ_mid=within(main_mid_educ,{D_group=ifelse(main_mid_educ$treatment==1, "Educated-treatment", "Educated-control")})
densityplot(~ lnwpart5revenue | D_group, data=main_educ_mid, layout=c(1,2),
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red") 
              panel.abline(v=mean(x), col.line="green")},na.rm = TRUE)

main_noeduc_mid=within(main_mid_noeduc,{D_group=ifelse(main_mid_noeduc$treatment==1, "Uneducated-treatment", "Uneducated-control")})
densityplot(~ lnwpart5revenue | D_group, data=main_noeduc_mid, layout=c(1,2),
            panel=function(x,...){
              panel.densityplot(x,...)
              panel.abline(v=quantile(x,.5), col.line="red") 
              panel.abline(v=mean(x), col.line="green")},na.rm = TRUE)


#Preparing data for regressions
main=within(main,{post=ifelse(round>1, 1, 0)})
main=within(main,{intervention1=treatment*after1})
main=within(main,{intervention2=treatment*after2})
main=within(main,{intervention=treatment*post})
main_educ = subset(main,main$educ_college==1)
main_noeduc = subset(main,main$educ_college==0)
main_educ_nobefore = main_educ[!(main_educ$round<=1),]
main_noeduc_nobefore = main_noeduc[!(main_noeduc$round<=1),]


#Regressions for educated managers

main_educ$firmid = factor(main_educ$firmid)

ed1=plm(data=main_educ,model = "within", lnwpart5revenue ~ after1
        +after2+intervention1+intervention2, effect="twoways",
        index=c("firmid","round","round1county"))

ed2=plm(data=main_educ,model = "within", lnwfixasset ~ after1
        +after2+intervention1+intervention2, effect="twoways",
        index=c("firmid","round","round1county"))

ed3=plm(data=main_educ,model = "within", lnwnum_clients ~ after1
        +after2+intervention1+intervention2, effect="twoways",
        index=c("firmid","round","round1county"))

ed4=plm(data=main_educ_nobefore,model = "within", manage_z ~ after2+
          +intervention1+intervention2,
        index=c("round"))

ed5=plm(data=main_educ_nobefore,model = "within", zmanage_incentive ~ after2+
          +intervention1+intervention2,
        index=c("round"))

stargazer(ed1,ed2,ed3,ed4,ed5, type="text",align=TRUE,omit=c("firmid","round","round1county"))
stargazer(ed1,ed2,ed3,ed4,ed5, type = "latex",align=TRUE,omit=c("firmid","round","round1county"), out="C:/Users/Kyriakos/Desktop/")


#Regressions for uneducated managers

main_noeduc$firmid = factor(main_noeduc$firmid)

un1=plm(data=main_noeduc,model = "within", lnwpart5revenue ~ after1
        +after2+intervention1+intervention2, effect="twoways",
        index=c("firmid","round","round1county"))

un2=plm(data=main_noeduc,model = "within", lnwfixasset ~ after1
        +after2+intervention1+intervention2, effect="twoways",
        index=c("firmid","round","round1county"))

un3=plm(data=main_noeduc,model = "within", lnwnum_clients ~ after1
        +after2+intervention1+intervention2, effect="twoways",
        index=c("firmid","round","round1county"))

un4=plm(data=main_noeduc_nobefore,model = "within", manage_z ~ after2
        +intervention1+intervention2,
        index=c("round"))

un5=plm(data=main_noeduc_nobefore,model = "within", zmanage_incentive ~ after2+
          +intervention1+intervention2,
        index=c("round"))

stargazer(un1,un2,un3,un4,un5, type="text",align=TRUE,omit=c("firmid","round","round1county"))
stargazer(un1,un2,un3,un4,un5, type = "latex",align=TRUE,omit=c("firmid","round","round1county"), out="C:/Users/Kyriakos/Desktop/")



#Regressions for testing difference

main$firmid = factor(main$firmid)

diff1=plm(data=main,model = "within", lnwpart5revenue ~ after1 +after2+intervention1+intervention2 + 
            (after1 +after2+intervention1+intervention2)*educ_college, effect="twoways",index=c("firmid","round","round1county"))

diff2=plm(data=main,model = "within", lnwfixasset ~ after1 +after2+intervention1+intervention2 + 
            (after1 +after2+intervention1+intervention2)*educ_college, effect="twoways",index=c("firmid","round","round1county"))

diff3=plm(data=main,model = "within", lnwnum_clients ~ after1 +after2+intervention1+intervention2 + 
            (after1 +after2+intervention1+intervention2)*educ_college, effect="twoways",index=c("firmid","round","round1county"))

diff4=plm(data=main,model = "within", manage_z ~  after2+intervention1+intervention2 + 
            (after2+intervention1+intervention2)*educ_college,index=c("round"))

diff5=plm(data=main,model = "within", zmanage_incentive ~  after2+intervention1+intervention2 + 
            (after2+intervention1+intervention2)*educ_college
          ,index=c("round"))

stargazer(diff1,diff2,diff3,diff4,diff5, type="text",align=TRUE,omit=c("firmid","round","round1county"))
stargazer(diff1,diff2,diff3,diff4,diff5, type = "latex",align=TRUE,omit=c("firmid","round","round1county"), out="C:/Users/Kyriakos/Desktop/")


#Descriptive table
mainvar = data.frame(main$firmid,main$educ_college,main$after1,main$after2,main$round,main$round1county,main$treatment,main$zmanage_incentive,main$manage_z,main$num_clients,main$lnwnum_clients,main$part5revenue,main$lnwpart5revenue,main$lnwfixasset)
stargazer(mainvar,type="text")
stargazer(mainvar,type="latex",out="C:/Users/Kyriakos/Desktop")
