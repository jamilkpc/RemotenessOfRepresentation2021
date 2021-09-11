dfBase <- readRDS("~/Downloads/Regs1to3.rds")
dfCSES <- readRDS("~/Downloads/Regs4.rds")
dfVDEM <- readRDS("~/Downloads/Regs5to6.rds")
dfAppendix1 <- readRDS("~/Downloads/AppendixA.rds")

library(plm)
library(lmtest)
library(sandwich)

# Table 1

DispropOLS = plm(LSq ~ Remoteness, data = dfBase, model = 'pooling')
summary(DispropOLS)
coeftest(DispropOLS, vcov.=vcovBK)

wipOLS = plm(wip ~ Remoteness, data = dfBase, model = 'pooling')
summary(wipOLS)
coeftest(wipOLS, vcov.=vcovBK)

EffNsOLS = plm(Eff_Ns ~ Remoteness, data = dfBase, model = 'pooling')
summary(EffNsOLS)
coeftest(EffNsOLS, vcov.=vcovBK)

IdeoOLS = plm(proximateToParty4 ~ Remoteness, data = dfCSES, model = 'pooling', index = c('Country.Code', 'Year'))
summary(IdeoOLS)
coeftest(IdeoOLS, vcov.=vcovHC)

egalOLS = plm(v2x_egal ~ Remoteness*democracy, data = dfVDEM, model = 'pooling')
summary(egalOLS)
coeftest(egalOLS, vcov.=vcovBK)

LegOLS = plm(v2xlg_legcon ~ Remoteness*democracy, data = dfVDEM, model = 'pooling')
summary(LegOLS)
coeftest(LegOLS, vcov.=vcovBK)


# Table 2

DispropRE = plm(LSq ~ devRemoteness + avgRemoteness +
                  Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
                  Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
                data = dfBase, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(DispropRE)
coeftest(DispropRE, vcov. = function(x) vcovHC(x, cluster = 'group'))

wipRE = plm(wip ~ devRemoteness + avgRemoteness +
              Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
              Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
            data = dfBase, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(wipRE)
coeftest(wipRE, vcov.=vcovHC)

EffNsRE = plm(Eff_Ns ~ devRemoteness + avgRemoteness +
                Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
                Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
              data = dfBase, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(EffNsRE)
coeftest(EffNsRE, vcov.=vcovHC)

IdeoRE = plm(proximateToParty4 ~ devRemoteness + avgRemoteness, data = dfCSES, model = 'random', index = c('Country.Code', 'Year'), random.method = 'walhus')
summary(IdeoRE)
coeftest(IdeoRE, vcov.=vcovHC)

# Table 3

library(ivmodel)
library(ivreg)
library(ivpack)
library(lmtest)
library(sandwich)


dfCauses <- readRDS("~/Downloads/RegsTable3.rds")

sec4eq1 = lm(log(nos) ~ log(wdiPop) + gini_disp, data = subset(dfCauses, democracia))
summary(sec4eq1)
coeftest(sec4eq1, vcov = vcovHC(sec4eq1, type="HC1"))

sec4eq2 = ivreg(log(nos) ~ log(wdiPop) + gini_disp | lwheatsugar + log(wdiPop), data=subset(dfCauses, democracia))
summary(sec4eq2)
robust.se(sec4eq2)

regIV = ivmodelFormula(log(nos) ~ gini_disp + log(wdiPop) | lwheatsugar + log(wdiPop), data=subset(dfCauses, democracia))
summary(regIV)
CLR(regIV)


sec4eq3 = lm(log(nos) ~ log(wdiPop) + wdiAgr, data = subset(dfCauses, democracia))
summary(sec4eq3)
coeftest(sec4eq3, vcov = vcovHC(sec4eq3, type="HC1"))

sec4eq4 = ivreg(log(nos) ~ log(wdiPop) + wdiAgr | lwheatsugar + log(wdiPop), data=subset(dfCauses, democracia))
summary(sec4eq4)
robust.se(sec4eq4)

regIV = ivmodelFormula(log(nos) ~ wdiAgr + log(wdiPop) | lwheatsugar + log(wdiPop), data=subset(dfCauses, democracia))
summary(regIV)
CLR(regIV)


# Appendix A - Table A1
EffNsOLS = plm(Eff_Ns ~ Remoteness + al_ethnic2000 + as.factor(Decade), data = dfAppendix1, model = 'pooling')
summary(EffNsOLS)
coeftest(EffNsOLS, vcov.=vcovBK)

EffNvOLS = plm(Eff_Nv ~ Remoteness + al_ethnic2000 + as.factor(Decade), data = dfAppendix1, model = 'pooling', index = c("Country.Code","Year"))
summary(EffNvOLS)
coeftest(EffNvOLS, vcov.=vcovBK)

IdeoOLS = plm(proximateToParty1 ~ Remoteness, data = dfCSES, model = 'pooling', index = c('Country.Code', 'Year'))
summary(IdeoOLS)
coeftest(IdeoOLS, vcov.=vcovHC)

IdeoOLS = plm(proximateToParty2 ~ Remoteness, data = dfCSES, model = 'pooling', index = c('Country.Code', 'Year'))
summary(IdeoOLS)
coeftest(IdeoOLS, vcov.=vcovHC)


# Appendix A - Table A2
EffNsRE = plm(Eff_Ns ~ devRemoteness + avgRemoteness + al_ethnic2000 +
                Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
                Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
              data = dfAppendix1, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(EffNsRE)
coeftest(EffNsRE, vcov.=vcovHC)

EffNvRE = plm(Eff_Nv ~ devRemoteness + avgRemoteness + al_ethnic2000 +
                Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
                Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
              data = dfAppendix1, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(EffNvRE)
coeftest(EffNvRE, vcov. = vcovHC)

IdeoRE = plm(proximateToParty1 ~ devRemoteness + avgRemoteness, data = dfCSES, model = 'random', index = c('Country.Code', 'Year'), random.method = 'walhus')
summary(IdeoRE)
coeftest(IdeoRE, vcov.=vcovHC)

IdeoRE = plm(proximateToParty2 ~ devRemoteness + avgRemoteness, data = dfCSES, model = 'random', index = c('Country.Code', 'Year'), random.method = 'walhus')
summary(IdeoRE)
coeftest(IdeoRE, vcov.=vcovHC)

# Appendix B

load('/home/jamil/Downloads/AppendixB.RData')

summary(lm(Remoteness ~ log(Population), data = dfBaseCS))
summary(lm(Remoteness ~ wdi_gini, data = dfBaseCS))
summary(lm(Remoteness ~ al_ethnic2000, data = dfBaseCS))

library(sensemakr)

dispropBW = lm(LSq ~ Remoteness, data = dfBaseCS)
summary(dispropBW)
sensemakr(dispropBW,'Remoteness', alpha = 0.05)
ovb_minimal_reporting(sensemakr(dispropBW,'Remoteness', alpha = 0.05), format = "html")
plot(sensemakr(dispropBW,'Remoteness', alpha = 0.05), type = 'extreme')

wipBW = lm(wip ~ Remoteness, data = dfBaseCS)
summary(wipBW)
sensemakr(wipBW,'Remoteness', alpha = 0.05)
ovb_minimal_reporting(sensemakr(wipBW,'Remoteness', alpha = 0.05), format = "html")
summary(sensemakr(wipBW,'Remoteness', alpha = 0.05))

EffsBW = lm(Eff_Ns ~ Remoteness, data = dfBaseCS)
summary(EffsBW)
sensemakr(EffsBW,'Remoteness', alpha = 0.05)
ovb_minimal_reporting(sensemakr(EffsBW,'Remoteness', alpha = 0.05), format = "html")

csesBW = lm(proximateToParty4 ~ Remoteness, data = dfCsesCS)
summary(csesBW)
sensemakr(csesBW,'Remoteness', alpha = 0.05)
ovb_minimal_reporting(sensemakr(csesBW,'Remoteness', alpha = 0.05), format = "html")

egalBW = lm(v2x_egal ~ Remoteness, data = dfVdemS)
summary(egalBW)
sensemakr(egalBW,'Remoteness', alpha = 0.05)
plot(sensemakr(egalBW,'Remoteness', alpha = 0.05), type = 'extreme')
ovb_minimal_reporting(sensemakr(egalBW,'Remoteness', alpha = 0.05), format = "html")

legconBW = lm(v2xlg_legcon ~ Remoteness, data = dfVdemS)
summary(legconBW)
sensemakr(legconBW,'Remoteness', alpha = 0.05)
plot(sensemakr(legconBW,'Remoteness', alpha = 0.05))
ovb_minimal_reporting(sensemakr(legconBW,'Remoteness', alpha = 0.05), format = "html")


# Appendix C - Table C1

dfBaseAC <- readRDS("~/Downloads/AC - Regs1to3.rds")
dfCsesAC <- readRDS("~/Downloads/AC - Regs4.rds")
dfVdemAC <- readRDS("~/Downloads/AC - Regs5to6.rds")


library(plm)
library(lmtest)
library(sandwich)

# Appendix C - Table C1

DispropOLS = plm(LSq ~ Remoteness, data = dfBaseAC, model = 'pooling')
summary(DispropOLS)
coeftest(DispropOLS, vcov.=vcovBK)

wipOLS = plm(wip ~ Remoteness, data = dfBaseAC, model = 'pooling')
summary(wipOLS)
coeftest(wipOLS, vcov.=vcovBK)

EffNsOLS = plm(Eff_Ns ~ Remoteness, data = dfBaseAC, model = 'pooling')
summary(EffNsOLS)
coeftest(EffNsOLS, vcov.=vcovBK)

IdeoOLS = plm(proximateToParty4 ~ Remoteness, data = dfCsesAC, model = 'pooling', index = c('Country.Code', 'Year'))
summary(IdeoOLS)
coeftest(IdeoOLS, vcov.=vcovHC)

egalOLS = plm(v2x_egal ~ Remoteness*democracy, data = dfVdemAC, model = 'pooling')
summary(egalOLS)
coeftest(egalOLS, vcov.=vcovBK)

LegOLS = plm(v2xlg_legcon ~ Remoteness*democracy, data = dfVdemAC, model = 'pooling')
summary(LegOLS)
coeftest(LegOLS, vcov.=vcovBK)


# Appendix C - Table C2

DispropRE = plm(LSq ~ devRemoteness + avgRemoteness +
                  Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
                  Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
                data = dfBaseAC, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(DispropRE)
coeftest(DispropRE, vcov. = function(x) vcovHC(x, cluster = 'group'))

wipRE = plm(wip ~ devRemoteness + avgRemoteness +
              Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
              Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
            data = dfBaseAC, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(wipRE)
coeftest(wipRE, vcov.=vcovHC)

EffNsRE = plm(Eff_Ns ~ devRemoteness + avgRemoteness +
                Decade_1 + Decade_2 + Decade_3 + Decade_4 + Decade_5 +
                Decade_1:avgDecade + Decade_2:avgDecade + Decade_3:avgDecade + Decade_4:avgDecade + Decade_5:avgDecade,
              data = dfBaseAC, model = 'random', index = c("Country.Code","Year"), random.method = 'walhus')
summary(EffNsRE)
coeftest(EffNsRE, vcov.=vcovHC)

IdeoRE = plm(proximateToParty4 ~ devRemoteness + avgRemoteness, data = dfCsesAC, model = 'random', index = c('Country.Code', 'Year'), random.method = 'walhus')
summary(IdeoRE)
coeftest(IdeoRE, vcov.=vcovHC)

# Appendix D

dfDem = readRDS('/home/jamil/Downloads/AppendixD.rds')

sec4eq1 = lm(log(NumberOfSeats) ~ log(Population) + gini_disp, data = dfDem)
summary(sec4eq1)
coeftest(sec4eq1, vcov = vcovHC(sec4eq1, type="HC1"))

sec4eq2 = lm(log(NumberOfSeats) ~ log(Population) + Agriculture, data = dfDem)
summary(sec4eq2)
coeftest(sec4eq2, vcov = vcovHC(sec4eq2, type="HC1"))
