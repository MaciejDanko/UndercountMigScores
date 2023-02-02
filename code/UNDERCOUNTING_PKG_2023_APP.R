# The part of this code will be incorporated into the package
# Model result constructors will be added here

#rm(list=ls())
#
#Sys.setlocale("LC_ALL","en_US.UTF-8")
#install.packages(c('Cairo','DT','shiny','colourpicker','shinyhelper','magicaxis','data.table','countrycodes'))

#library(Cairo)
#library(devtools)
#library(DT)
#library(shiny)
#library(usethis)
#library(colourpicker)
#library(shinyhelper)
#library(magicaxis)
#library(data.table)
#library(countrycode)

#options(bitmapType="cairo")

LEVELS7=c('extremely low','very low','low','medium','high','very high','extremely high')
LEVELS6=c('extremely low','very low','low','high','very high','extremely high')
LEVELS5=c('very low','low','medium','high','very high')
LEVELS4=c('very low','low','high','very high')
LEVELS3=c('low','medium','high')
LEVELS2=c('low','high')

loadaslist<-function(Name, norder=c('NORDIC4',
                                    'NORDIC',
                                    'NORDIC_PLUS_BE',
                                    'NORDIC_PLUS_CH',
                                    'NORDIC_PLUS_NL',
                                    'NORDIC_PLUS_BE_CH',
                                    'NORDIC_PLUS_BE_NL',
                                    'NORDIC_PLUS_CH_NL',
                                    'NORDIC_PLUS_BE_CH_NL',
                                    'NORDIC_PLUS_AT_BE_CH_NL',
                                    'NORDIC_PLUS_AT_BE_CH_DE_NL',
                                    'NORDIC_PLUS_AT_BE_CH_DE_FR_NL',
                                    'NORDIC_PLUS_AT_BE_CH_DE_FR_IE_NL',
                                    'NORDIC_PLUS_AT_BE_CH_DE_FR_IE_NL_UK',
                                    'ALL_COUNTRIES')){
  G<-new.env();
  load(Name,G);
  LG<-as.list(G)
  NLG<-names(LG)
  LG[norder[norder %in% NLG]]
}

sortisoyear<-function(z) {
  lapply(z, function(x) {
    x$iso2[x$iso2=='EL']<-'GR'
    #x$iso2[x$iso2=='GB']<-'UK'
    x$ind<-paste(x$iso2,x$year)
    x<-x[order(x$ind),]
    x$ind<-NULL
    x
  })
}

DAT_IMEM<-sortisoyear(loadaslist('./data/UndercountingIndex_IMEM.rda'))

# DAT_IMEM_OLD<-sortisoyear(loadaslist('./data/UndercountMigScores/data/old/UndercountingIndex_IMEM.rda'))
# test1<-DAT_IMEM$NORDIC_PLUS_AT_BE_CH_NL[(DAT_IMEM$NORDIC_PLUS_AT_BE_CH_NL$iso2=='CZ')&(DAT_IMEM$NORDIC_PLUS_AT_BE_CH_NL$year==2002),]
# test2<-DAT_IMEM_OLD$NORDIC_PLUS_AT_BE_CH_NL[(DAT_IMEM_OLD$NORDIC_PLUS_AT_BE_CH_NL$iso2=='CZ')&(DAT_IMEM_OLD$NORDIC_PLUS_AT_BE_CH_NL$year==2002),]
# test1[,c('EC','ECn','ECd')]
# test2[,c('EC','ECnraw','ECdraw')]
# test1[,c('EUC','EUCn','EUCd')]
# test2[,c('EUC','EUCnraw','EUCdraw')] #Czech had different definition of duration before

DAT_POIS<-sortisoyear(loadaslist('./data/UndercountingIndex_Willekens_Poisson.rda'))
DAT_EXPERT<-sortisoyear(loadaslist('./data/UndercountingIndex_Willekens_Expert.rda'))
DAT_MIXED<-sortisoyear(loadaslist('./data/UndercountingIndex_Willekens_Mixture.rda'))

#DAT_ADD_OPT_B_W_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicCHNLBE.rda'))
DAT_ADD_OPT_B_UW_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicCHNLBE.rda'))
DAT_ADD_OPT_IE_UW_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicCHNLBE.rda'))
#DAT_ADD_OPT_IE_W_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicCHNLBE.rda'))

#DAT_MUL_OPT_B_W_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicCHNLBE.rda'))
DAT_MUL_OPT_B_UW_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicCHNLBE.rda'))
DAT_MUL_OPT_IE_UW_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicCHNLBE.rda'))
#DAT_MUL_OPT_IE_W_CHNLBE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicCHNLBE.rda'))

#DAT_ADD_OPT_B_W_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicCH.rda'))
DAT_ADD_OPT_B_UW_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicCH.rda'))
DAT_ADD_OPT_IE_UW_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicCH.rda'))
#DAT_ADD_OPT_IE_W_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicCH.rda'))

#DAT_MUL_OPT_B_W_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicCH.rda'))
DAT_MUL_OPT_B_UW_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicCH.rda'))
DAT_MUL_OPT_IE_UW_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicCH.rda'))
#DAT_MUL_OPT_IE_W_CH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicCH.rda'))

#DAT_ADD_OPT_B_W_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicBE.rda'))
DAT_ADD_OPT_B_UW_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicBE.rda'))
DAT_ADD_OPT_IE_UW_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicBE.rda'))
#DAT_ADD_OPT_IE_W_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicBE.rda'))

#DAT_MUL_OPT_B_W_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicBE.rda'))
DAT_MUL_OPT_B_UW_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicBE.rda'))
DAT_MUL_OPT_IE_UW_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicBE.rda'))
#DAT_MUL_OPT_IE_W_BE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicBE.rda'))

#DAT_ADD_OPT_B_W_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicCHNLBE.rda'))
DAT_ADD_OPT_B_UW_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicCHNLBE.rda'))
DAT_ADD_OPT_IE_UW_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicCHNLBE.rda'))
#DAT_ADD_OPT_IE_W_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicCHNLBE.rda'))

#DAT_MUL_OPT_B_W_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicCHNL.rda'))
DAT_MUL_OPT_B_UW_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicCHNL.rda'))
DAT_MUL_OPT_IE_UW_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicCHNL.rda'))
#DAT_MUL_OPT_IE_W_CHNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicCHNL.rda'))

#DAT_ADD_OPT_B_W_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicNL.rda'))
DAT_ADD_OPT_B_UW_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicNL.rda'))
DAT_ADD_OPT_IE_UW_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicNL.rda'))
#DAT_ADD_OPT_IE_W_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicNL.rda'))

#DAT_MUL_OPT_B_W_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicNL.rda'))
DAT_MUL_OPT_B_UW_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicNL.rda'))
DAT_MUL_OPT_IE_UW_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicNL.rda'))
#DAT_MUL_OPT_IE_W_NL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicNL.rda'))

#DAT_ADD_OPT_B_W_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicBECH.rda'))
DAT_ADD_OPT_B_UW_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicBECH.rda'))
DAT_ADD_OPT_IE_UW_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicBECH.rda'))
#DAT_ADD_OPT_IE_W_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicBECH.rda'))

#DAT_MUL_OPT_B_W_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicBECH.rda'))
DAT_MUL_OPT_B_UW_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicBECH.rda'))
DAT_MUL_OPT_IE_UW_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicBECH.rda'))
#DAT_MUL_OPT_IE_W_BECH<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicBECH.rda'))

#DAT_ADD_OPT_B_W_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicBENL.rda'))
DAT_ADD_OPT_B_UW_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicBENL.rda'))
DAT_ADD_OPT_IE_UW_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicBENL.rda'))
#DAT_ADD_OPT_IE_W_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicBENL.rda'))

#DAT_MUL_OPT_B_W_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicBENL.rda'))
DAT_MUL_OPT_B_UW_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicBENL.rda'))
DAT_MUL_OPT_IE_UW_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicBENL.rda'))
#DAT_MUL_OPT_IE_W_BENL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicBENL.rda'))

#DAT_ADD_OPT_B_W_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_AllCountries.rda'))
DAT_ADD_OPT_B_UW_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_AllCountries.rda'))
DAT_ADD_OPT_IE_UW_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_AllCountries.rda'))
#DAT_ADD_OPT_IE_W_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_AllCountries.rda'))

#DAT_MUL_OPT_B_W_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_AllCountries.rda'))
DAT_MUL_OPT_B_UW_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_AllCountries.rda'))
DAT_MUL_OPT_IE_UW_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_AllCountries.rda'))
#DAT_MUL_OPT_IE_W_ALL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_AllCountries.rda'))

#DAT_ADD_OPT_B_W_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_Nordic.rda'))
DAT_ADD_OPT_B_UW_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_Nordic.rda'))
DAT_ADD_OPT_IE_UW_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_Nordic.rda'))
#DAT_ADD_OPT_IE_W_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_Nordic.rda'))

#DAT_MUL_OPT_B_W_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_Nordic.rda'))
DAT_MUL_OPT_B_UW_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_Nordic.rda'))
DAT_MUL_OPT_IE_UW_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_Nordic.rda'))
#DAT_MUL_OPT_IE_W_NORDIC<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_Nordic.rda'))

#DAT_ADD_OPT_B_W_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_Nordic4.rda'))
DAT_ADD_OPT_B_UW_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_Nordic4.rda'))
DAT_ADD_OPT_IE_UW_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_Nordic4.rda'))
#DAT_ADD_OPT_IE_W_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_Nordic4.rda'))

#DAT_MUL_OPT_B_W_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_Nordic4.rda'))
DAT_MUL_OPT_B_UW_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_Nordic4.rda'))
DAT_MUL_OPT_IE_UW_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_Nordic4.rda'))
#DAT_MUL_OPT_IE_W_NORDIC4<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_Nordic4.rda'))

#DAT_ADD_OPT_B_W_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicCHNLBEAT.rda'))
DAT_ADD_OPT_B_UW_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicCHNLBEAT.rda'))
DAT_ADD_OPT_IE_UW_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicCHNLBEAT.rda'))
#DAT_ADD_OPT_IE_W_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicCHNLBEAT.rda'))

#DAT_MUL_OPT_B_W_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicCHNLBEAT.rda'))
DAT_MUL_OPT_B_UW_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicCHNLBEAT.rda'))
DAT_MUL_OPT_IE_UW_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicCHNLBEAT.rda'))
#DAT_MUL_OPT_IE_W_CHNLBEAT<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicCHNLBEAT.rda'))

#DAT_ADD_OPT_B_W_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicCHNLBEATDE.rda'))
DAT_ADD_OPT_B_UW_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicCHNLBEATDE.rda'))
DAT_ADD_OPT_IE_UW_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicCHNLBEATDE.rda'))
#DAT_ADD_OPT_IE_W_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicCHNLBEATDE.rda'))

#DAT_MUL_OPT_B_W_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicCHNLBEATDE.rda'))
DAT_MUL_OPT_B_UW_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicCHNLBEATDE.rda'))
DAT_MUL_OPT_IE_UW_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicCHNLBEATDE.rda'))
#DAT_MUL_OPT_IE_W_CHNLBEATDE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicCHNLBEATDE.rda'))

#DAT_ADD_OPT_B_W_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicATDEBECHFRNL.rda'))
DAT_ADD_OPT_B_UW_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicATDEBECHFRNL.rda'))
DAT_ADD_OPT_IE_UW_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicATDEBECHFRNL.rda'))
#DAT_ADD_OPT_IE_W_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicATDEBECHFRNL.rda'))

#DAT_MUL_OPT_B_W_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicATDEBECHFRNL.rda'))
DAT_MUL_OPT_B_UW_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicATDEBECHFRNL.rda'))
DAT_MUL_OPT_IE_UW_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicATDEBECHFRNL.rda'))
#DAT_MUL_OPT_IE_W_ATDEBECHFRNL<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicATDEBECHFRNL.rda'))

#DAT_ADD_OPT_B_W_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicATDEBECHFRNLIEUK.rda'))
DAT_ADD_OPT_B_UW_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicATDEBECHFRNLIEUK.rda'))
DAT_ADD_OPT_IE_UW_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicATDEBECHFRNLIEUK.rda'))
#DAT_ADD_OPT_IE_W_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicATDEBECHFRNLIEUK.rda'))

#DAT_MUL_OPT_B_W_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicATDEBECHFRNLIEUK.rda'))
DAT_MUL_OPT_B_UW_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicATDEBECHFRNLIEUK.rda'))
DAT_MUL_OPT_IE_UW_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicATDEBECHFRNLIEUK.rda'))
#DAT_MUL_OPT_IE_W_ATDEBECHFRNLIEUK<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicATDEBECHFRNLIEUK.rda'))

#DAT_ADD_OPT_B_W_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_W_NordicATDEBECHFRNLIE.rda'))
DAT_ADD_OPT_B_UW_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_BOTH_UNW_NordicATDEBECHFRNLIE.rda'))
DAT_ADD_OPT_IE_UW_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_UNW_NordicATDEBECHFRNLIE.rda'))
#DAT_ADD_OPT_IE_W_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_ADD_SEPARATE_I_E_W_NordicATDEBECHFRNLIE.rda'))

#DAT_MUL_OPT_B_W_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_W_NordicATDEBECHFRNLIE.rda'))
DAT_MUL_OPT_B_UW_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_BOTH_UNW_NordicATDEBECHFRNLIE.rda'))
DAT_MUL_OPT_IE_UW_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_UNW_NordicATDEBECHFRNLIE.rda'))
#DAT_MUL_OPT_IE_W_ATDEBECHFRNLIE<-sortisoyear(loadaslist('./data/UndercountingIndex_OPT_MUL_SEPARATE_I_E_W_NordicATDEBECHFRNLIE.rda'))

colnames(DAT_IMEM$NORDIC)==colnames(DAT_MUL_OPT_B_UW_NORDIC$NORDIC)

NORDIC<-DAT_IMEM$NORDIC
NORDIC4<-DAT_IMEM$NORDIC4

load('./data/OPT_CORRECTION_ADDITIVE.RDA')
load('./data/OPT_CORRECTION_MULTIPLICATIVE.RDA')
load('./data/DURATION.RDA')

#source('./rawdata/TOOLS.R')

load('./data/MetaDataJAN2023.rda')
IMEM$Country[IMEM$Country=='EL']<-'GR'
QUANTMIG$Country[QUANTMIG$Country=='EL']<-'GR'
IMEM<-IMEM[!is.na(IMEM$Country),]
IMEM<-IMEM[order(IMEM$Country),]
QUANTMIG<-QUANTMIG[order(QUANTMIG$Country),]
Meta_DeReg$iso2[Meta_DeReg$iso2=='EL']<-'GR'
Meta_DeReg<-Meta_DeReg[order(Meta_DeReg$iso2),]
Meta_Reg$iso2[Meta_Reg$iso2=='EL']<-'GR'
Meta_Reg<-Meta_Reg[order(Meta_Reg$iso2),]

Meta_Reg$comment[Meta_Reg$iso2=='EE']<-'No sanctions'
colnames(Meta_Reg)<-c("iso2", "country", "registration obligation", "time limit", "comment", "score" )
colnames(Meta_DeReg)<-c('iso2','country', "de-registration obligation", "de-registration obligation third country nationals",
                        "monitoring third country nationals",
                        "administrative corrections",'comment')

# REMOVE LI
# Meta_Reg<-Meta_Reg[Meta_Reg$iso2!='LI',]
# Meta_DeReg<-Meta_DeReg[Meta_DeReg$iso2!='LI',] # remove it when LI is added to the model
# IMEM<-IMEM[IMEM$Country!='LI',]

get_correction<-function(direction, corrected, additive, separated){

  #cat(direction, corrected, additive, separated,'\n')
  if (additive) CORR<-CORRECTION_PAR_ADD else CORR<-CORRECTION_PAR_MULT
  print(length(CORR))
  #print(sapply(CORR, function(k) length(k$corr.b)))
  corrected<-as.numeric(corrected)
  cat(corrected,corrected-4)

  if (corrected<5) {
    res<-switch(as.character(corrected),
         '0' = c(1,1,1,1),
         '1' = c(0.53,0.63,0.73,2.26),
         '2' = c(0.51,0.61,0.81,1.61),
         '3' = c(0.79,0.84,0.89,2.61),
         '4' = c(0.51,0.64,0.71,1.80))
  } else {
    if (direction=='I' && separated) {
      print('i')
      res<-CORR[[corrected-4]]$corr.i
    } else if (direction=='E' && separated) {
      print('e')
      res<-CORR[[corrected-4]]$corr.e
    } else {
      print('b')
      res<-CORR[[corrected-4]]$corr.b
    }
  }
  res<-unlist(res)
  res<-round(c(res[1:3],1,res[4]),2)
  print(res)
  res<-data.frame(t(as.matrix(format(res,nsmall=2,digits=2))))
  colnames(res)<-c(0,3,6,12,'P')
  res<-cbind(data.frame(Duration='Correction'),res)
  print(res)
  res
}

#get_correction('I',5,TRUE,FALSE)
#get_correction('I',4,TRUE,FALSE)
#get_correction('I',19,TRUE,FALSE)
#get_correction('E',5,TRUE,FALSE)

# DAT_IMEM<-sortisoyear(DAT_IMEM)
# DAT_POIS<-sortisoyear(DAT_POIS)
# DAT_EXPERT<-sortisoyear(DAT_EXPERT)
# DAT_MIXED<-sortisoyear(DAT_MIXED)
# DAT_OPT_B_UW_9<-sortisoyear(DAT_OPT_B_UW_9)
# DAT_OPT_B_W_9<-sortisoyear(DAT_OPT_B_W_9)
# DAT_OPT_IE_UW_9<-sortisoyear(DAT_OPT_IE_UW_9)
# DAT_OPT_IE_W_9<-sortisoyear(DAT_OPT_IE_W_9)
#NORDIC<-DAT_IMEM$NORDIC

# Meta_Reg$iso2<-correct_eurostat_iso2(Meta_Reg$iso2)
# Meta_DeReg$iso2<-correct_eurostat_iso2(Meta_DeReg$iso2)
# IMEM<-IMEM[!is.na(IMEM$Country),]
# IMEM$Country<-correct_eurostat_iso2(IMEM$Country)
DT2DF<-function(x) if (class(x)[1]=='datatables') {
  z<-x$x$data
  if (colnames(z)[1]%in%c('',' ','  ')) z<-z[,-1]
  data.frame(z, stringsAsFactors = FALSE, check.names = FALSE, check.rows = FALSE)
} else data.frame(x, stringsAsFactors = FALSE, check.names = FALSE, check.rows = FALSE)

colMedians <- function(x) apply(x,2,median, na.rm=TRUE)
colSd <- function(x) apply(x,2,sd, na.rm=TRUE)
colQlo <- function(x) apply(x,2,quantile, probs=0.025, na.rm=TRUE)
colQhi <- function(x) apply(x,2,quantile, probs=0.975, na.rm=TRUE)

Recalc_Meta_DeReg_Raw<-function(MetaDeReg,w1,w2,w3,w4,t1,t2, trustnordic){
  cat(w1,w2,w3,w4,t1,t2,trustnordic,'\n')
  #MetaDeReg<-DT2DF(MetaDeReg)
  L3col<-c('Low','Medium','High')
  nominator<- w1*(tolower(paste(MetaDeReg[,3]))=='yes')+
    w2*(tolower(paste(MetaDeReg[,4]))=='yes')+
    w3*(tolower(paste(MetaDeReg[,5]))=='yes')+
    w4*(tolower(paste(MetaDeReg[,6]))=='yes')
  denominator<-w1*(tolower(paste(MetaDeReg[,3]))!='unknown')+
    w2*(tolower(paste(MetaDeReg[,4]))!='unknown')+
    w3*(tolower(paste(MetaDeReg[,5]))!='unknown')+
    w4*(tolower(paste(MetaDeReg[,6]))!='unknown')
  score_num<- 1 - nominator / denominator
  score<-paste(cut(score_num,c(0,t1,t2,1),L3col,include.lowest = TRUE))
  isNORDIC<-MetaDeReg$iso2 %in%c('IS','SE','NO','DK','FI')
  if (trustnordic){
    score[isNORDIC]<-L3col[1]
    score_num[(is.nan(score_num)|is.na(score_num))&isNORDIC]<-0
  }
  MetaDeReg$`score num`<-round(score_num,3)
  MetaDeReg$score<-score
  MetaDeReg$score[is.na(score_num)]<-'Unknown'
  MetaDeReg
}

Recalc_Meta_Reg_Raw<-function(MetaReg, trustnordic=TRUE){
  cat(trustnordic,class(MetaReg),'\n')
  #MetaReg<-DT2DF(MetaReg)
  MetaReg$score<-NULL
  MetaReg$`score num`<-NULL
  L3col<-c('Low','Medium','High')
  isNORDIC<-MetaReg$iso2 %in%c('IS','SE','NO','DK','FI')
  MetaReg$`score num`<-(tolower(MetaReg[,3])=='no') + 0.5*(tolower(MetaReg$`time limit`)=='no limit')+ 0.5*(tolower(MetaReg$comment)=='no sanctions')
  MetaReg$score<-L3col[MetaReg$`score num`*2+1]
  unk<-tolower(MetaReg[,3])=='unknown'
  MetaReg$score[unk]<-'Unknown'
  MetaReg$`score num`[unk]<-NA
  if (trustnordic){
    MetaReg$score[isNORDIC] <- L3col[1]
    MetaReg$`score num`[isNORDIC] <- 0
  }
  MetaReg
}

firstCap<-function(x) {
  x<-paste(x)
  tmp<-paste(toupper(substr(x,1,1)),substr(x,2,nchar(x)),sep='')
  tmp[tmp=='NA']<-NA
  tmp
}

weightedmean3<-function(m1, m2, m3, w1, w2, w3){
  n1<-!is.na(m1)
  n2<-!is.na(m2)
  n3<-!is.na(m3)
  w<-(n1*w1+n2*w2+n3*w3)
  (na2zero(m1*w1)+na2zero(m2*w2)+na2zero(m3*w3))/w
}

getDataType<-function(refcountry=9,  corrected=1){
  corrected<-as.character(corrected)
  CORRE<-switch(corrected,
                 '0' = 'NONE',
                 '1' = 'IMEM',
                 '2' = 'EXPERT',
                 '3' = 'POIS',
                 '4' = 'MIXED',
                 '5' = 'NORDIC4',
                 '6' = 'NORDIC',
                 '7' = 'NORDIC+BE',
                 '8' = 'NORDIC+CH',
                 '9' = 'NORDIC+NL',
                 '10' = 'NORDIC+BE+CH',
                 '11' = 'NORDIC+BE+NL',
                 '12' = 'NORDIC+CH+NL',
                 '13' = 'NORDIC+BE+CH+NL',
                 '14' = 'NORDIC+AT+BE+CH+NL',#DAT_ADD_OPT_B_UW_CHNLBEAT,
                 '15' = 'NORDIC+AT+BE+CH+DE+NL',#DAT_ADD_OPT_B_UW_CHNLBEATDE,
                 '16' = 'NORDIC+AT+BE+CH+DE+FR+NL',#DAT_ADD_OPT_B_UW_ATDEBECHFRNL,
                 '17' = 'NORDIC+AT+BE+CH+DE+FR+IE+NL',#DAT_ADD_OPT_B_UW_ATDEBECHFRNLIE,
                 '18' = 'NORDIC+AT+BE+CH+DE+FR+IE+NL+UK',#DAT_ADD_OPT_B_UW_ATDEBECHFRNLIEUK,
                 '19' = 'ALL COUNTRIES')#DAT_ADD_OPT_B_UW_ALL)
  
  REFF<-switch(as.character(refcountry),
               '1' = 'NORDIC4',
               '2' = 'NORDIC',
               '3' = 'NORDIC+BE',
               '4' = 'NORDIC+CH',
               '5' = 'NORDIC+NL',
               '6' = 'NORDIC+BE+CH',
               '7' = 'NORDIC+BE+NL',
               '8' = 'NORDIC+CH+NL',
               '9' = 'NORDIC+BE+CH+NL',
               '10' = 'NORDIC+AT+BE+CH+NL',#DAT_ADD_OPT_B_UW_CHNLBEAT,
               '11' = 'NORDIC+AT+BE+CH+DE+NL',#DAT_ADD_OPT_B_UW_CHNLBEATDE,
               '12' = 'NORDIC+AT+BE+CH+DE+FR+NL',#DAT_ADD_OPT_B_UW_ATDEBECHFRNL,
               '13' = 'NORDIC+AT+BE+CH+DE+FR+IE+NL',#DAT_ADD_OPT_B_UW_ATDEBECHFRNLIE,
               '14' = 'NORDIC+AT+BE+CH+DE+FR+IE+NL+UK',#DAT_ADD_OPT_B_UW_ATDEBECHFRNLIEUK,
               '15' = 'ALL COUNTRIES')
  c(CORRE, REFF)
}

getUCCont<-function(refcountry=9, direction='E', corrected=1,
                    imputmax=7, ncp=1, additive=TRUE,
                    weighted=FALSE, separated=FALSE){

  corrected<-as.character(corrected)
  #if(!weighted && !separated) {
  if(!separated && additive) {
    DAT0<-switch(corrected,
                 '0' = DAT_IMEM,
                 '1' = DAT_IMEM,
                 '2' = DAT_EXPERT,
                 '3' = DAT_POIS,
                 '4' = DAT_MIXED,
                 '5' = DAT_ADD_OPT_B_UW_NORDIC4,
                 '6' = DAT_ADD_OPT_B_UW_NORDIC,
                 '7' = DAT_ADD_OPT_B_UW_BE,
                 '8' = DAT_ADD_OPT_B_UW_CH,
                 '9' = DAT_ADD_OPT_B_UW_NL,
                 '10' = DAT_ADD_OPT_B_UW_BECH,
                 '11' = DAT_ADD_OPT_B_UW_BENL,
                 '12' = DAT_ADD_OPT_B_UW_CHNL,
                 '13' = DAT_ADD_OPT_B_UW_CHNLBE,
                 '14' = DAT_ADD_OPT_B_UW_CHNLBEAT,
                 '15' = DAT_ADD_OPT_B_UW_CHNLBEATDE,
                 '16' = DAT_ADD_OPT_B_UW_ATDEBECHFRNL,
                 '17' = DAT_ADD_OPT_B_UW_ATDEBECHFRNLIE,
                 '18' = DAT_ADD_OPT_B_UW_ATDEBECHFRNLIEUK,
                 '19' = DAT_ADD_OPT_B_UW_ALL)
  } else if(!separated && !additive) {
    DAT0<-switch(corrected,
                 '0' = DAT_IMEM,
                 '1' = DAT_IMEM,
                 '2' = DAT_EXPERT,
                 '3' = DAT_POIS,
                 '4' = DAT_MIXED,
                 '5' = DAT_MUL_OPT_B_UW_NORDIC4,
                 '6' = DAT_MUL_OPT_B_UW_NORDIC,
                 '7' = DAT_MUL_OPT_B_UW_BE,
                 '8' = DAT_MUL_OPT_B_UW_CH,
                 '9' = DAT_MUL_OPT_B_UW_NL,
                 '10' = DAT_MUL_OPT_B_UW_BECH,
                 '11' = DAT_MUL_OPT_B_UW_BENL,
                 '12' = DAT_MUL_OPT_B_UW_CHNL,
                 '13' = DAT_MUL_OPT_B_UW_CHNLBE,
                 '14' = DAT_MUL_OPT_B_UW_CHNLBEAT,
                 '15' = DAT_MUL_OPT_B_UW_CHNLBEATDE,
                 '16' = DAT_MUL_OPT_B_UW_ATDEBECHFRNL,
                 '17' = DAT_MUL_OPT_B_UW_ATDEBECHFRNLIE,
                 '18' = DAT_MUL_OPT_B_UW_ATDEBECHFRNLIEUK,
                 '19' = DAT_MUL_OPT_B_UW_ALL)

  } else if (separated && additive) {
    DAT0<-switch(corrected,
                 '0' = DAT_IMEM,
                 '1' = DAT_IMEM,
                 '2' = DAT_EXPERT,
                 '3' = DAT_POIS,
                 '4' = DAT_MIXED,
                 '5' = DAT_ADD_OPT_IE_UW_NORDIC4,
                 '6' = DAT_ADD_OPT_IE_UW_NORDIC,
                 '7' = DAT_ADD_OPT_IE_UW_BE,
                 '8' = DAT_ADD_OPT_IE_UW_CH,
                 '9' = DAT_ADD_OPT_IE_UW_NL,
                 '10' = DAT_ADD_OPT_IE_UW_BECH,
                 '11' = DAT_ADD_OPT_IE_UW_BENL,
                 '12' = DAT_ADD_OPT_IE_UW_CHNL,
                 '13' = DAT_ADD_OPT_IE_UW_CHNLBE,
                 '14' = DAT_ADD_OPT_IE_UW_CHNLBEAT,
                 '15' = DAT_ADD_OPT_IE_UW_CHNLBEATDE,
                 '16' = DAT_ADD_OPT_IE_UW_ATDEBECHFRNL,
                 '17' = DAT_ADD_OPT_IE_UW_ATDEBECHFRNLIE,
                 '18' = DAT_ADD_OPT_IE_UW_ATDEBECHFRNLIEUK,
                 '19' = DAT_ADD_OPT_IE_UW_ALL)

  } else if (separated && !additive) {
    DAT0<-switch(corrected,
                 '0' = DAT_IMEM,
                 '1' = DAT_IMEM,
                 '2' = DAT_EXPERT,
                 '3' = DAT_POIS,
                 '4' = DAT_MIXED,
                 '5' = DAT_MUL_OPT_IE_UW_NORDIC4,
                 '6' = DAT_MUL_OPT_IE_UW_NORDIC,
                 '7' = DAT_MUL_OPT_IE_UW_BE,
                 '8' = DAT_MUL_OPT_IE_UW_CH,
                 '9' = DAT_MUL_OPT_IE_UW_NL,
                 '10' = DAT_MUL_OPT_IE_UW_BECH,
                 '11' = DAT_MUL_OPT_IE_UW_BENL,
                 '12' = DAT_MUL_OPT_IE_UW_CHNL,
                 '13' = DAT_MUL_OPT_IE_UW_CHNLBE,
                 '14' = DAT_MUL_OPT_IE_UW_CHNLBEAT,
                 '15' = DAT_MUL_OPT_IE_UW_CHNLBEATDE,
                 '16' = DAT_MUL_OPT_IE_UW_ATDEBECHFRNL,
                 '17' = DAT_MUL_OPT_IE_UW_ATDEBECHFRNLIE,
                 '18' = DAT_MUL_OPT_IE_UW_ATDEBECHFRNLIEUK,
                 '19' = DAT_MUL_OPT_IE_UW_ALL)
  }


  RES<-switch(as.character(refcountry),
              '1' = DAT0$NORDIC4,
              '2' = DAT0$NORDIC,
              '3' = DAT0$NORDIC_PLUS_BE,
              '4' = DAT0$NORDIC_PLUS_CH,
              '5' = DAT0$NORDIC_PLUS_NL,
              '6' = DAT0$NORDIC_PLUS_BE_CH,
              '7' = DAT0$NORDIC_PLUS_BE_NL,
              '8' = DAT0$NORDIC_PLUS_CH_NL,
              '9' = DAT0$NORDIC_PLUS_BE_CH_NL,
              '10' = DAT0$NORDIC_PLUS_AT_BE_CH_NL,
              '11' = DAT0$NORDIC_PLUS_AT_BE_CH_DE_NL,
              '12' = DAT0$NORDIC_PLUS_AT_BE_CH_DE_FR_NL,
              '13' = DAT0$NORDIC_PLUS_AT_BE_CH_DE_FR_IE_NL,
              '14' = DAT0$NORDIC_PLUS_AT_BE_CH_DE_FR_IE_NL_UK,
              '15' = DAT0$ALL_COUNTRIES)

  if (direction=='E' && corrected>0){
    RES$Y<-RES$EC
    RES$Ysd<-RES$EC_sd
    RES$Yq.lo<-RES$EC_q.lo
    RES$Yq.med<-RES$EC_q.med
    RES$Yq.hi<-RES$EC_q.hi
    RES$W<-RES$POPEn
  } else if (direction=='E' && corrected==0){
    RES$Y<-RES$EUC
    RES$Ysd<-RES$EUC_sd
    RES$Yq.lo<-RES$EUC_q.lo
    RES$Yq.med<-RES$EUC_q.med
    RES$Yq.hi<-RES$EUC_q.hi
    RES$W<-RES$POPEn
  } else if (direction=='I' && corrected>0){
    RES$Y<-RES$IC
    RES$Ysd<-RES$IC_sd
    RES$Yq.lo<-RES$IC_q.lo
    RES$Yq.med<-RES$IC_q.med
    RES$Yq.hi<-RES$IC_q.hi
    RES$W<-RES$POPIn
  } else if (direction=='I' && corrected==0){
    RES$Y<-RES$IUC
    RES$Ysd<-RES$IUC_sd
    RES$Yq.lo<-RES$IUC_q.lo
    RES$Yq.med<-RES$IUC_q.med
    RES$Yq.hi<-RES$IUC_q.hi
    RES$W<-RES$POPIn
  }

  D<-reshape(RES[,c('iso2','year','Y')],
             idvar='iso2', timevar = 'year', direction = 'wide')
  rownames(D)<-D$iso2
  D<-D[,-1]
  D<-D[!apply(D, 1, function(k) all(is.na(k))),]
  D<-D[,!apply(D, 2, function(k) all(is.na(k))),]
  Y<-gsub('[[:alpha:]]','',colnames(D))
  Y<-gsub('.','',fixed=TRUE,Y)
  print(Y)
  Ye<-as.numeric(substr(colnames(D),3,9))
  if (any(is.na(D))) {
    if (length(ncp)==0) ncp <- missMDA::estim_ncpPCA(log(D), method.cv = "kfold", ncp.max=imputmax, ncp.min=1)$ncp
    res.comp <- missMDA::imputePCA(log(D),ncp=ncp, maxiter = 1e5)
    imputed_data <-exp(res.comp$completeObs)
  } else {
    imputed_data <- D
    warning('Something wrong')
  }
  list(original=D, imputed=imputed_data, raw=RES[RES$year%in%Y,c('iso2','year','Y','Ysd','Yq.lo','Yq.med','Yq.hi','W')])
}

#getUCCont(additive = TRUE)$raw$Y
#getUCCont(additive = !TRUE)$raw$Y

plot_ui_result_<-function(DAT, country, stats=1, logscale=FALSE, extrapol=TRUE, plotCI=TRUE,toplab=''){
  if (stats==1) {
    #DAT$Y<-DAT[,Draw]
    #DAT$YE<-DAT[,Dextr]
    DAT$Ylo<-DAT$Y-DAT$Ysd*1.96
    K<-0.000001
    DAT$Ylo[DAT$Ylo<=K]<-K
    DAT$Yhi<-DAT$Y+DAT$Ysd*1.96
  } else if (stats==2){
    #DAT$Y<-DAT[,Dqmed]
    #DAT$YE<-DAT[,Dextr]
    DAT$Ylo<-DAT$Yq.lo
    DAT$Yhi<-DAT$Yq.hi
    K<-0.000001
    DAT$Ylo[DAT$Ylo<=K]<-K
  }
  if (logscale) {
    DAT$Y<-log10(DAT$Y)
    DAT$Ylo<-log10(DAT$Ylo)
    DAT$Yhi<-log10(DAT$Yhi)
    DAT$YE <- log10(DAT$YE)
    minY<-log10(0.008)
    minmaxY<-log10(2.1)
  } else {
    minY<-0.00
    minmaxY<-2.1
  }
  layout(matrix(c(rep(1,4),2),1,5))
  par(mar=c(4.6,2.5,0.5,0),oma=c(0,2.5,2,0))
  XR<-range(NORDIC$year[!is.na(NORDIC$IUC)| !is.na(NORDIC$EUC)])
  inf2NA<-function(x){ x[is.infinite(x)]<-NA; x}
  YLIM<-c(minY,max(inf2NA(c(DAT$Ylo,DAT$Yhi,DAT$Y,minmaxY)),na.rm = TRUE))
  plot(DAT$year,DAT$Y,col=DAT$col,pch=19,type='p',xlim=XR,ylim=YLIM, xlab='',las=3, ylab='',axes=FALSE)
  mtext('Bilateral flows ratio',2,3,cex=1.5)
  mtext(toplab,3,0.5,cex=1.5)
  axis(1,at=unique(DAT$year),cex.axis=1.5,las=3); box(); box();
  if (logscale) {
    magicaxis::magaxis(2,unlog = TRUE,cex.axis=2,las=3)
    abline(h=0,col='gray',lty=1)
  } else {
    axis(2,cex.axis=2,las=3)
    abline(h=1,col='gray',lty=1)
  }
  abline(v=unique(DAT$year),col='lightgray',lty='dotted')
  country.col<-sapply(country, function(k) DAT$col[DAT$iso2== k][1])
  for (k in country) {
    CDAT<-DAT[DAT$iso2==k,]
    YE<-CDAT$YE
    YY<-CDAT$year
    CDAT<-CDAT[!is.na(CDAT$Y),]
    if (!all(is.na(CDAT$Ylo)))
      if (plotCI) polygon(c(CDAT$year,rev(CDAT$year)),c(CDAT$Yhi,rev(CDAT$Ylo)),col=adjustcolor(CDAT$col,0.4), border=NA)
    lines(CDAT$year,CDAT$Y,col=CDAT$col,type='l')
    if (extrapol) {
      lines(YY,YE,col=CDAT$col,type='l',lty=2)
      lines(YY,YE,col=CDAT$col,type='p')
    }
  }
  plot(1:2,1:2,axes=FALSE,type='n',xlab='',ylab=''); #box()
  legend('topleft',country,col=country.col,bty='n',lty=1,cex=1.75,lwd=2)
  legend('topleft',country,col=adjustcolor(country.col,0.4),bty='n',lty=1,cex=1.75,lwd=15.5)
}

#NORDIC_PLUS_AT_BE_CH_NL[NORDIC_PLUS_AT_BE_CH_NL$iso2=='LT',]



# direction='E'
# country=c('PL','DE')
# refcountry=9
# stats=2
# extrapol=1
# raymer=13
# logscale=TRUE
# plotCI=TRUE
# additive=TRUE
# separated=FALSE
# ncp=1

plot_ui_result<-function(direction, country, refcountry, stats, extrapol, raymer, logscale, plotCI, additive, separated, ncp){
  cat(direction, country, refcountry, stats, extrapol, raymer, logscale, plotCI, additive, separated, ncp,'\n')
  if (length(country)){
    DAT<-getUCCont(refcountry=refcountry, direction=direction, corrected=raymer, imputmax=7, ncp=ncp, additive=additive,
                   weighted=FALSE, separated=separated)

    DATi<-DAT$imputed
    DAT<-DAT$raw
    validC<-DAT$iso2[!is.na(DAT$Y)]
    DAT<-DAT[DAT$iso2%in%validC,]
    DAT$col<-as.numeric(as.factor(DAT$iso2))

    PAL<-palette.colors(max(DAT$col),'polychrome 36')
    PAL <- c(
      "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black","deeppink1", "skyblue2", "#FB9A99", "palegreen2",
      "gold1", "#FDBF6F", "#CAB2D6","gray70", "khaki2", "maroon", "orchid1",  "blue1", "steelblue4",
      "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown","orange","green2")
    #PAL<-PAL[order(nchar(PAL))]
    DAT$col<-PAL[DAT$col]
    DATicol<-PAL
    DAT<-DAT[DAT$iso2%in%country,]
    DATi<-DATi[rownames(DATi)%in%country,]
    DAT$YE<-as.vector(t(as.matrix(DATi)))
    print(country)
    if (direction=='I'){
      plot_ui_result_(DAT, country,
                      stats=stats, logscale=logscale, extrapol=extrapol, plotCI=plotCI,'Immigration')
    } else if (direction=='E'){
      plot_ui_result_(DAT, country,
                      stats=stats, logscale=logscale, extrapol=extrapol, plotCI=plotCI,'Emigration')
    } else stop()
  }
}

get_ui_result<-function(direction, refcountry, raymer, additive, separated, ncp){
  cat(direction, refcountry, raymer,  additive, separated, ncp,'\n')
  DAT<-getUCCont(refcountry=refcountry, direction=direction, corrected=raymer, imputmax=7, ncp=ncp, additive=additive,
                   weighted=FALSE, separated=separated)
  
  DAT$type<-getDataType(refcountry, raymer)  
  DAT$direction = direction
  DAT$additive = additive
  DAT$separated = separated
  DAT$ncp = ncp
  print(names(DAT))
  return(DAT)
}

# Threshold - where metadata has stronger impact
# 17, 23, 24, 3, 5, 45
#
# immi_meta_options=list(w1 = 0.5,
#                        w2 = 0.1,
#                        w3 = 0.1,
#                        w4 = 0.3,
#                        t1 = 0.3,
#                        t2 = 0.6,
#                        trustNordic = TRUE)
# emi_meta_options=list(trustNordic = TRUE)
# direction='I';
# groups = 5;
# model_options = list(ncp = 1, useimputation = TRUE, weighted=FALSE, separated=FALSE, additive=TRUE, refcountries = 9, durationCorrection = 13);
# mixing_options = list(Thresholds = NA, threshyear = 2008, IgnoreOverCounting = TRUE, w_imemA = 0.1, w_imemB = 0.25, w_metaA = 0.1, w_metaB = 0.15, w_modelA = 0.8, w_modelB = 0.6)
#
# tmp<-get_undercounting('I',  model_options = list(ncp = 1, useimputation = FALSE, weighted=FALSE, separated=FALSE, additive=TRUE, refcountries = 9, durationCorrection = 13))
# tmp$R.RawScore['PL',]
# tmp<-get_undercounting('I',  model_options = list(ncp = 1, useimputation = FALSE, weighted=FALSE, separated=FALSE, additive=!TRUE, refcountries = 9, durationCorrection = 13))
# tmp$R.RawScore['PL',]

fast.merge.df<-function(DF1, DF2, by, all.x =TRUE, all.y=TRUE){
  DT1 <- data.table::data.table(DF1, key=by, stringsAsFactors = FALSE, check.names = FALSE)
  DT2 <- data.table::data.table(DF2, key=by, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(data.table:::merge.data.table(DT1, DT2, all.x=all.x, all.y=all.y),
             stringsAsFactors = FALSE, check.names = FALSE)
}

# copy_matrix_rows<-function(prim,seco){
#   seco<-seco[rownames(seco) %in% rownames(prim),colnames(seco) %in% colnames(prim)]
#   dp<-rownames(prim)
#   ds<-rownames(seco)
#   add<-length(dp)-length(ds)
#   if (add>0) {
#     seco<-rbind(seco,matrix(NA,add,ncol(seco)))
#     rownames(seco)<-c(ds, dp[!dp %in% ds])
#   }
#   seco[rownames(prim),]
# }
#
# copy_matrix_cols<-function(prim,seco){
#   seco<-seco[rownames(seco) %in% rownames(prim),colnames(seco) %in% colnames(prim)]
#   dp<-colnames(prim)
#   ds<-colnames(seco)
#   add<-length(dp)-length(ds)
#   if (add>0) {
#     seco<-cbind(seco,matrix(NA,nrow(seco),add))
#     colnames(seco)<-c(ds, dp[!dp %in% ds])
#   }
#   seco[,colnames(prim)]
# }
#
# copy_array_dims<-function(pattern, x){
#   pd<-dimnames(pattern)
#   xd<-dimnames(x)
#   L<-sapply(pd,length)
#   d1<-xd[[1]] %in% pd[[1]]
#   d2<-xd[[2]] %in% pd[[2]]
#   d3<-xd[[3]] %in% pd[[3]]
#   res<-x[d1,d2,d3]
#   for (k in 1:length(xd)){
#     xd<-dimnames(res)
#     Lx<-sapply(xd,length)
#     missp<-pd[[k]][!pd[[k]]%in%xd[[k]]]
#     addd<-length(missp)
#     if (addd>0) {
#       dimms<-Lx
#       dimms[k]<-addd
#       dimmn<-xd
#       dimmn[[k]]<-missp
#       adda<-array(NA,dimms,dimmn)
#       res<-abind::abind(res,adda,along = k)
#     }
#   }
#   res[pd[[1]],pd[[2]],pd[[3]]]
# }
#
# array2df<-function(a,varsn=c('origin2','dest2','year')){
#   d<-dimnames(a)
#   mat<-expand.grid(d,stringsAsFactors = FALSE)
#   colnames(mat)<-varsn
#   mat$value<-as.vector(a)
#   mat
# }

na2zero<-function(x) {x[is.na(x)]<-0; x}

factor.matrix<-function(mat, levels){
  rn<-rownames(mat); cn<-colnames(mat); d<-dim(mat)
  res<-factor(mat, levels)
  dim(res)<-d; rownames(res)<-rn; colnames(res)<-cn
  res
}

as.numeric.matrix<-function(mat){
  rn<-rownames(mat); cn<-colnames(mat); d<-dim(mat)
  res<-as.numeric(mat)
  dim(res)<-d; rownames(res)<-rn; colnames(res)<-cn
  res
}

cut_matrix<-function(mat, breaks, labels = NULL, include.lowest = FALSE){
  rn<-rownames(mat); cn<-colnames(mat); d<-dim(mat)
  res<-base:::cut(mat, breaks=breaks, labels=labels, include.lowest=include.lowest)
  dim(res)<-d; rownames(res)<-rn; colnames(res)<-cn
  res
}

level2num<-function(x, LEVELS, bounds=NULL){
  mat<-as.numeric.matrix(factor.matrix(x,LEVELS))
  if (!is.null(bounds)) {
    num<-seq(bounds[1],bounds[2],length.out=length(LEVELS))
    for (k in seq_along(LEVELS)) mat[paste(mat)==paste(k)]<-num[k]
  }
  mat
}

my2dplot<-function(mat, LEVELS, namat=NULL, cexx=1, cexy=1, lox=1, loy=1,
                   groups=length(LEVELS),colors=rev(brewer.pal(n = groups, name = "Spectral"))[1:groups],
                   nodata='no data', naalpha=0.2){
  if (length(LEVELS)!=groups) stop('Nb of levels and groups differ.')
  ThreshCol = seq(min(mat,na.rm = TRUE),max(mat,na.rm = TRUE), length.out=groups+1)
  mat<-cut_matrix(mat,breaks=ThreshCol,labels=FALSE,include.lowest = TRUE)
  dimm<-dim(mat)
  rn<-rownames(mat)
  cn<-colnames(mat)
  lcolors<-adjustcolor(colors,alpha.f = naalpha)
  plot(NA,NA, xlim=c(1-0.5,length(cn)+0.5), ylim=c(1-0.5,length(rn)+0.5),
       xaxs='i',yaxs='i',axes=FALSE, xlab='',ylab='')
  axis(1, at=seq_along(cn), cn,las=3, cex.axis=cexx)
  axis(2, at=seq_along(rn), rn,las=1, cex.axis=cexy)
  box()
  for(cl in seq_along(cn)) for (ro in seq_along(rn)){
    if (is.na(mat[ro,cl])) {
      lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
      lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
    } else {
      if (!is.null(namat) && namat[ro,cl]){
        rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=lcolors[mat[ro,cl]])
        lines(c(cl-0.4,cl+0.4),c(ro-0.4,ro+0.4))
        lines(c(cl-0.4,cl+0.4),c(ro+0.4,ro-0.4))
      } else
        rect(cl-0.45,ro-0.45,cl+0.45,ro+0.45, border = NA, col=colors[mat[ro,cl]])
    }
  }
  if (length(namat)) {
    l<-legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors,NA), border = NA, legend = c(LEVELS,nodata))
    legend(l$rect$left, min(l$text$y)+abs(min(diff(l$text$y)))*1.45,'',pch=4,bty='n',xpd=TRUE,cex=1.45)
  } else {
    legend(length(cn)+lox,length(rn)+loy,xpd=TRUE,bty='n',fill=c(colors), border = NA, legend = LEVELS)
  }
}

# Thresholds specific to either emigration or immigration
get_undercounting<-function(direction='E',
                            immi_meta_options=list(w1 = 0.5,
                                                   w2 = 0.1,
                                                   w3 = 0.1,
                                                   w4 = 0.3,
                                                   t1 = 0.3,
                                                   t2 = 0.6,
                                                   trustNordic = TRUE),
                            emi_meta_options=list(trustNordic = TRUE),
                            model_options = list(ncp=1,
                                                 useimputation=TRUE,
                                                 weighted=FALSE,
                                                 separated=FALSE,
                                                 additive=TRUE,
                                                 refcountries=3,
                                                 durationCorrection = 1),
                            model_classification_options = list(
                              UserThresholds=NA,
                              UseQuantiles=TRUE,
                              IgnoreOverCounting = TRUE,
                              TranslateGroups = 7
                            ),
                            mixing_options=list(
                              threshyear = 2008,
                              FinalGroups = 5,
                              w_imemA = 0.1,
                              w_imemB = 0.25,
                              w_metaA = 0.1,
                              w_metaB = 0.15,
                              w_modelA = 0.8,
                              w_modelB = 0.6)

) {


  mixing_options$FinalGroups[mixing_options$FinalGroups>7]<-7
  mixing_options$FinalGroups[mixing_options$FinalGroups<2]<-2
  LEVELS<-switch(paste(mixing_options$FinalGroups), '2'=LEVELS2, '3'=LEVELS3, '4'=LEVELS4, '5'=LEVELS5, '6'=LEVELS6, '7'=LEVELS7)

  ##############################################################################
  # Metadata and expert opinion
  
  QuantMigScore<-data.frame(iso2=QUANTMIG$Country, ExpertScore=1-as.numeric(QUANTMIG$score))
  
  if (direction=='E') {
    resMODEL.E<-getUCCont(refcountry=model_options$refcountries,
                          direction='E',
                          corrected=model_options$durationCorrection,
                          imputmax=7,
                          ncp=model_options$ncp,
                          weighted=model_options$weighted,
                          separated=model_options$separated,
                          additive=model_options$additive)

    resMODEL<-resMODEL.E
    MetaScores<-Recalc_Meta_DeReg_Raw(Meta_DeReg,
                                  immi_meta_options$w1,
                                  immi_meta_options$w2,
                                  immi_meta_options$w3,
                                  immi_meta_options$w4,
                                  immi_meta_options$t1,
                                  immi_meta_options$t2,
                                  immi_meta_options$trustNordic)
    MetaScore<-data.frame(iso2=MetaScores$iso2, MetaScore=1-MetaScores$`score num`, stringsAsFactors = FALSE, check.names = FALSE) ###
    #ImemScore<-data.frame(iso2=IMEM$Country, ImemScore=1*(tolower(IMEM$Undercount.emi.IMEM)=='low'), stringsAsFactors = FALSE, check.names = FALSE) ###
    ImemScore<-data.frame(iso2=IMEM$Country, ExpertScore=1*(tolower(IMEM$Undercount.emi.IMEM)=='low'), stringsAsFactors = FALSE, check.names = FALSE) ###
    
  } else {
    resMODEL.I<-getUCCont(refcountry=model_options$refcountries,
                          direction='I',
                          corrected=model_options$durationCorrection,
                          imputmax=7,
                          ncp=model_options$ncp,
                          weighted=model_options$weighted,
                          separated=model_options$separated,
                          additive=model_options$additive)

    resMODEL<-resMODEL.I
    MetaScores<-Recalc_Meta_Reg_Raw(Meta_Reg, emi_meta_options$trustNordic)
    MetaScore<-data.frame(iso2=MetaScores$iso2, MetaScore=1-MetaScores$`score num`, stringsAsFactors = FALSE, check.names = FALSE) ###
    #ImemScore<-data.frame(iso2=IMEM$Country,ImemScore=1*(tolower(IMEM$Undercount.imm.IMEM)=='low'), stringsAsFactors = FALSE, check.names = FALSE)###
    ImemScore<-data.frame(iso2=IMEM$Country,ExpertScore=1*(tolower(IMEM$Undercount.imm.IMEM)=='low'), stringsAsFactors = FALSE, check.names = FALSE)###
  }

  MetaExpertScorePre<-fast.merge.df(MetaScore,ImemScore,'iso2')
  MetaExpertScorePost<-fast.merge.df(MetaScore,QuantMigScore,'iso2')
  
  #MetaImemScore<-fast.merge.df(MetaScore,ImemScore,'iso2')

  ##############################################################################
  # Classification of the model

  NoData<-is.na(resMODEL$original)

  if (model_options$useimputation) {
    ModelScore<-log10(resMODEL$imputed)
    #VecDat<-c(log10(unlist(resMODEL.I$imputed)) ,log10(unlist(resMODEL.E$imputed)))
    VecDat<-ModelScore
    VecDat<-VecDat[VecDat<=0]
  } else {
    ModelScore<-log10(resMODEL$original)
    VecDat<-ModelScore
    #VecDat<-c(log10(unlist(resMODEL.I$original)) ,log10(unlist(resMODEL.E$original)))
    VecDat<-VecDat[VecDat<=0]
  }
  colnames(ModelScore)<-gsub('Y.','',colnames(ModelScore))

  ####

  if (is.na( model_classification_options$UserThresholds[1]) || length( model_classification_options$UserThresholds)!=model_classification_options$TranslateGroups-1) {
    if (model_classification_options$UseQuantiles){
      if ( model_classification_options$IgnoreOverCounting) {
        #overcounting is combined with the second highest group
        model_classification_options$UserThresholds<-c(max(ModelScore, na.rm = TRUE),quantile(VecDat,seq(1,0,length.out=model_classification_options$TranslateGroups+1), na.rm=TRUE))[-2]
        UserThresholds<- model_classification_options$UserThresholds
      } else {
        #overcounting is a separate group
        model_classification_options$UserThresholds<-c(max(ModelScore, na.rm = TRUE),quantile(VecDat,seq(1,0,length.out=model_classification_options$TranslateGroups), na.rm=TRUE))
        UserThresholds<- model_classification_options$UserThresholds
      }
    } else {
      if ( model_classification_options$IgnoreOverCounting) {
        #overcounting is combined with the second highest group
        model_classification_options$UserThresholds<-c(
          max(ModelScore, na.rm = TRUE),
          seq(max(VecDat, na.rm = TRUE),min(VecDat, na.rm = TRUE),length.out=model_classification_options$TranslateGroups+1))[-2]
        UserThresholds<- model_classification_options$UserThresholds
      } else {
        #overcounting is a separate group
        model_classification_options$UserThresholds<-c(
          max(ModelScore, na.rm = TRUE),
          seq(max(VecDat, na.rm = TRUE),min(VecDat, na.rm = TRUE),length.out=model_classification_options$TranslateGroups))
        UserThresholds<- model_classification_options$UserThresholds
      }

    }
  } else {
    UserThresholds<- model_classification_options$UserThresholds
    UserThresholds<-(c(max(ModelScore, na.rm = TRUE), rev(UserThresholds), min(ModelScore, na.rm = TRUE)))
  }

  mLEVELS<-paste(1:model_classification_options$TranslateGroups)
  R.YearUser<-t(apply(ModelScore,1,cut,breaks=UserThresholds,include.lowest=TRUE,labels=rev(mLEVELS)))
  colnames(R.YearUser)<-gsub('Y.','',colnames(ModelScore))
  R.YearUser.num<-level2num(R.YearUser,mLEVELS,bounds=c(1,0))
  print(R.YearUser.num)
  # ModelScore['AT',]
  # R.YearUser['AT',]
  # R.YearUser.num['AT',]
  # plot(10^ModelScore['AT',],ylim=c(0,1))
  # lines(R.YearUser.num['AT',],type='p',pch=19,col=2)
  # abline(h=10^UserThresholds,lty=3)

  tmp_pre<-fast.merge.df(MetaExpertScorePre, data.frame(iso2=rownames(R.YearUser.num),R.YearUser.num,stringsAsFactors = FALSE, check.names = FALSE),'iso2')
  NoData_pre<-fast.merge.df(data.frame(iso2=MetaExpertScorePre$iso2,stringsAsFactors = FALSE, check.names = FALSE),
                        data.frame(iso2=rownames(NoData),NoData,stringsAsFactors = FALSE, check.names = FALSE),'iso2')
  tmp_post<-fast.merge.df(MetaExpertScorePost, data.frame(iso2=rownames(R.YearUser.num),R.YearUser.num,stringsAsFactors = FALSE, check.names = FALSE),'iso2')
  NoData_post<-fast.merge.df(data.frame(iso2=MetaExpertScorePost$iso2,stringsAsFactors = FALSE, check.names = FALSE),
                            data.frame(iso2=rownames(NoData),NoData,stringsAsFactors = FALSE, check.names = FALSE),'iso2')
  
  rownames(NoData_pre)<-NoData_pre[,1]; rownames(NoData_post)<-NoData_post[,1]
  cnnd_pre<-colnames(NoData_pre)[-1]; cnnd_post<-colnames(NoData_post)[-1]
  NoData_pre<-NoData_pre[,-1]; NoData_post<-NoData_post[,-1]
  NoData_pre[is.na(NoData_pre)]<-TRUE; NoData_post[is.na(NoData_post)]<-TRUE
  colnames(NoData_pre)<- cnnd_pre; colnames(NoData_post)<- cnnd_post

  rownames(tmp_pre)<-tmp_pre$iso2; rownames(tmp_post)<-tmp_post$iso2
  
  C.YearUser_pre<-tmp_pre[,-(1:3)]; 
  C.YearUser_post<-tmp_post[,-(1:3)]; 
  
  YY<-as.numeric(gsub('Y.','',(colnames(C.YearUser_pre))))
  C.YearUser.B<-C.YearUser_pre[,YY< mixing_options$threshyear]
  C.YearUser.A<-C.YearUser_post[,YY>=mixing_options$threshyear]

  ImemScore<-tmp_pre[,3]
  QuantMigScore<-tmp_post[,3]
  MetaScore_pre<-tmp_pre[,2]
  MetaScore_post<-tmp_post[,2]
  names(MetaScore_post) <- names(MetaScore_pre) <- names(ImemScore) <- rownames(C.YearUser_pre)

  CombinedScoreA<-weightedmean3(m1=C.YearUser.A, m2=QuantMigScore, m3=MetaScore_pre,
                                w1=mixing_options$w_modelA, w2=mixing_options$w_imemA, w3=mixing_options$w_metaA)
  CombinedScoreB<-weightedmean3(m1=C.YearUser.B, m2=ImemScore, m3=MetaScore_post,
                                w1=mixing_options$w_modelB, w2=mixing_options$w_imemB, w3=mixing_options$w_metaB)

  # CombinedScoreA<-na2zero(ModelScore.A * mixing_options$w_modelA) +
  #   na2zero(ImemScore * mixing_options$w_imemA) +
  #   na2zero(MetaScore * mixing_options$w_metaA)
  # CombinedScoreB<-na2zero(ModelScore.B * mixing_options$w_modelB) +
  #   na2zero(ImemScore * mixing_options$w_imemB) +
  #   na2zero(MetaScore * mixing_options$w_metaB)

  CombinedScore<-cbind(CombinedScoreB,CombinedScoreA)
  C.UserThresholds<-seq(0, 1, length.out=mixing_options$FinalGroups+1)
  C.YearUser<-t(apply(CombinedScore,1,cut,breaks=C.UserThresholds,include.lowest=TRUE,labels=rev(LEVELS)))
  colnames(C.YearUser)<-gsub('Y.','',colnames(ModelScore))

  C.YearUser.num<-level2num(C.YearUser,LEVELS,bounds=c(1,0))

  list(
    R.Score=R.YearUser,
    R.Score.Num=R.YearUser.num,
    C.Score=C.YearUser,
    C.Score.Num=C.YearUser.num,
    R.UserThresholds=UserThresholds,
    C.UserThresholds=C.UserThresholds,
    R.RawScore = ModelScore,
    C.RawScore = CombinedScore,
    MIpre.Score=MetaExpertScorePre,
    MIpost.Score=MetaExpertScorePost,
    NoData=NoData,
    ResModel=resMODEL,
    threshyear=mixing_options$threshyear,
    direction=direction,
    final.groups=mixing_options$FinalGroups,
    model.groups=model_classification_options$TranslateGroups,
    LEVELS=LEVELS)
}

# get_undercounting_old<-function(direction='E',
#                             immi_meta_options=list(w1 = 0.5,
#                                                    w2 = 0.1,
#                                                    w3 = 0.1,
#                                                    w4 = 0.3,
#                                                    t1 = 0.3,
#                                                    t2 = 0.6,
#                                                    trustNordic = TRUE),
#                             emi_meta_options=list(trustNordic = TRUE),
#                             model_options = list(ncp=1,
#                                                  useimputation=TRUE,
#                                                  weighted=FALSE,
#                                                  separated=FALSE,
#                                                  additive=TRUE,
#                                                  refcountries=3,
#                                                  durationCorrection = 1),
#                             model_classification_options = list(
#                               UserThresholds=NA,
#                               IgnoreOverCounting = TRUE,
#                               TranslateGroups = 7
#                             ),
#                             mixing_options=list(
#                               threshyear = 2008,
#                               FinalGroups = 5,
#                               w_imemA = 0.1,
#                               w_imemB = 0.25,
#                               w_metaA = 0.1,
#                               w_metaB = 0.15,
#                               w_modelA = 0.8,
#                               w_modelB = 0.6)
# 
# ) {
# 
# 
#   mixing_options$FinalGroups[mixing_options$FinalGroups>7]<-7
#   mixing_options$FinalGroups[mixing_options$FinalGroups<2]<-2
#   LEVELS<-switch(paste(mixing_options$FinalGroups), '2'=LEVELS2, '3'=LEVELS3, '4'=LEVELS4, '5'=LEVELS5, '6'=LEVELS6, '7'=LEVELS7)
# 
#   ##############################################################################
#   # Metadata and expert opinion
#   resMODEL.I<-getUCCont(refcountry=model_options$refcountries,
#                         direction='I',
#                         corrected=model_options$durationCorrection,
#                         imputmax=7,
#                         ncp=model_options$ncp,
#                         weighted=model_options$weighted,
#                         separated=model_options$separated,
#                         additive=model_options$additive)
# 
#   # resMODEL.I.t1<-getUCCont(refcountry=model_options$refcountries,
#   #                       direction='I',
#   #                       corrected=model_options$durationCorrection,
#   #                       imputmax=7,
#   #                       ncp=model_options$ncp,
#   #                       weighted=model_options$weighted,
#   #                       separated=TRUE,
#   #                       additive=model_options$additive)
#   # resMODEL.I.t2<-getUCCont(refcountry=model_options$refcountries,
#   #                          direction='I',
#   #                          corrected=model_options$durationCorrection,
#   #                          imputmax=7,
#   #                          ncp=model_options$ncp,
#   #                          weighted=model_options$weighted,
#   #                          separated=FALSE,
#   #                          additive=model_options$additive)
#   # resMODEL.I.t1$imputed-resMODEL.I.t2$imputed
#   #
# 
#   resMODEL.E<-getUCCont(refcountry=model_options$refcountries,
#                         direction='E',
#                         corrected=model_options$durationCorrection,
#                         imputmax=7,
#                         ncp=model_options$ncp,
#                         weighted=model_options$weighted,
#                         separated=model_options$separated,
#                         additive=model_options$additive)
# 
#   if (direction=='E') {
#     resMODEL<-resMODEL.E
#     MetaScores<-Recalc_Meta_DeReg_Raw(Meta_DeReg,
#                                       immi_meta_options$w1,
#                                       immi_meta_options$w2,
#                                       immi_meta_options$w3,
#                                       immi_meta_options$w4,
#                                       immi_meta_options$t1,
#                                       immi_meta_options$t2,
#                                       immi_meta_options$trustNordic)
#     MetaScore<-data.frame(iso2=MetaScores$iso2, MetaScore=1-MetaScores$`score num`, stringsAsFactors = FALSE, check.names = FALSE) ###
#     ImemScore<-data.frame(iso2=IMEM$Country, ImemScore=1*(tolower(IMEM$Undercount.emi.IMEM)=='low'), stringsAsFactors = FALSE, check.names = FALSE) ###
#   } else {
#     resMODEL<-resMODEL.I
#     MetaScores<-Recalc_Meta_Reg_Raw(Meta_Reg, emi_meta_options$trustNordic)
#     MetaScore<-data.frame(iso2=MetaScores$iso2, MetaScore=1-MetaScores$`score num`, stringsAsFactors = FALSE, check.names = FALSE) ###
#     ImemScore<-data.frame(iso2=IMEM$Country,ImemScore=1*(tolower(IMEM$Undercount.imm.IMEM)=='low'), stringsAsFactors = FALSE, check.names = FALSE)###
#   }
# 
#   MetaImemScore<-fast.merge.df(MetaScore,ImemScore,'iso2')
# 
#   ##############################################################################
#   # Classification of the model
# 
#   NoData<-is.na(resMODEL$original)
# 
#   if (model_options$useimputation) {
#     ModelScore<-log10(resMODEL$imputed)
#     VecDat<-c(log10(unlist(resMODEL.I$imputed)) ,log10(unlist(resMODEL.E$imputed)))
#     VecDat<-VecDat[VecDat<=0]
#   } else {
#     ModelScore<-log10(resMODEL$original)
#     VecDat<-c(log10(unlist(resMODEL.I$original)) ,log10(unlist(resMODEL.E$original)))
#     VecDat<-VecDat[VecDat<=0]
#   }
#   colnames(ModelScore)<-gsub('Y.','',colnames(ModelScore))
# 
#   ####
# 
#   if (is.na( model_classification_options$UserThresholds[1]) || length( model_classification_options$UserThresholds)!=model_classification_options$TranslateGroups-1) {
#     if ( model_classification_options$IgnoreOverCounting) {
#       model_classification_options$UserThresholds<-c(max(ModelScore, na.rm = TRUE),quantile(VecDat,seq(1,0,length.out=model_classification_options$TranslateGroups+1), na.rm=TRUE))[-2]
#       UserThresholds<- model_classification_options$UserThresholds
#     } else {
#       model_classification_options$UserThresholds<-c(max(ModelScore, na.rm = TRUE),quantile(VecDat,seq(1,0,length.out=model_classification_options$TranslateGroups), na.rm=TRUE))
#       UserThresholds<- model_classification_options$UserThresholds
#     }
#   } else {
#     UserThresholds<- model_classification_options$UserThresholds
#     UserThresholds<-(c(max(ModelScore, na.rm = TRUE), rev(UserThresholds), min(ModelScore, na.rm = TRUE)))
#   }
# 
#   mLEVELS<-paste(1:model_classification_options$TranslateGroups)
#   R.YearUser<-t(apply(ModelScore,1,cut,breaks=UserThresholds,include.lowest=TRUE,labels=rev(mLEVELS)))
#   colnames(R.YearUser)<-gsub('Y.','',colnames(ModelScore))
#   R.YearUser.num<-level2num(R.YearUser,mLEVELS,bounds=c(1,0))
# 
#   # ModelScore['AT',]
#   # R.YearUser['AT',]
#   # R.YearUser.num['AT',]
#   # plot(10^ModelScore['AT',],ylim=c(0,1))
#   # lines(R.YearUser.num['AT',],type='p',pch=19,col=2)
#   # abline(h=10^UserThresholds,lty=3)
# 
#   tmp<-fast.merge.df(MetaImemScore, data.frame(iso2=rownames(R.YearUser.num),R.YearUser.num,stringsAsFactors = FALSE, check.names = FALSE),'iso2')
#   NoData<-fast.merge.df(data.frame(iso2=MetaImemScore$iso2,stringsAsFactors = FALSE, check.names = FALSE),
#                         data.frame(iso2=rownames(NoData),NoData,stringsAsFactors = FALSE, check.names = FALSE),'iso2')
#   rownames(NoData)<-NoData[,1]
#   cnnd<-colnames(NoData)[-1]
#   NoData<-NoData[,-1]
#   NoData[is.na(NoData)]<-TRUE
#   colnames(NoData)<- cnnd
# 
#   rownames(tmp)<-tmp$iso2
#   C.YearUser<-tmp[,-(1:3)]
# 
#   YY<-as.numeric(gsub('Y.','',(colnames(C.YearUser))))
#   C.YearUser.B<-C.YearUser[,YY< mixing_options$threshyear]
#   C.YearUser.A<-C.YearUser[,YY>=mixing_options$threshyear]
# 
#   ImemScore<-tmp[,3]
#   MetaScore<-tmp[,2]
#   names(MetaScore) <- names(ImemScore) <- rownames(C.YearUser)
# 
#   CombinedScoreA<-weightedmean3(m1=C.YearUser.A, m2=ImemScore, m3=MetaScore,
#                                 w1=mixing_options$w_modelA, w2=mixing_options$w_imemA, w3=mixing_options$w_metaA)
#   CombinedScoreB<-weightedmean3(m1=C.YearUser.B, m2=ImemScore, m3=MetaScore,
#                                 w1=mixing_options$w_modelB, w2=mixing_options$w_imemB, w3=mixing_options$w_metaB)
# 
#   # CombinedScoreA<-na2zero(ModelScore.A * mixing_options$w_modelA) +
#   #   na2zero(ImemScore * mixing_options$w_imemA) +
#   #   na2zero(MetaScore * mixing_options$w_metaA)
#   # CombinedScoreB<-na2zero(ModelScore.B * mixing_options$w_modelB) +
#   #   na2zero(ImemScore * mixing_options$w_imemB) +
#   #   na2zero(MetaScore * mixing_options$w_metaB)
# 
#   CombinedScore<-cbind(CombinedScoreB,CombinedScoreA)
#   C.UserThresholds<-seq(0, 1, length.out=mixing_options$FinalGroups+1)
#   C.YearUser<-t(apply(CombinedScore,1,cut,breaks=C.UserThresholds,include.lowest=TRUE,labels=rev(LEVELS)))
#   colnames(C.YearUser)<-gsub('Y.','',colnames(ModelScore))
# 
#   C.YearUser.num<-level2num(C.YearUser,LEVELS,bounds=c(1,0))
# 
#   list(
#     R.Score=R.YearUser,
#     R.Score.Num=R.YearUser.num,
#     C.Score=C.YearUser,
#     C.Score.Num=C.YearUser.num,
#     R.UserThresholds=UserThresholds,
#     C.UserThresholds=C.UserThresholds,
#     R.RawScore = ModelScore,
#     C.RawScore = CombinedScore,
#     MI.Score=MetaImemScore,
#     NoData=NoData,
#     ResModel=resMODEL,
#     threshyear=mixing_options$threshyear,
#     direction=direction,
#     final.groups=mixing_options$FinalGroups,
#     model.groups=model_classification_options$TranslateGroups,
#     LEVELS=LEVELS)
# }



toeurostat<-function(x) {x[x=='GR']<-'EL'; x[x=='GB']<-'UK'; x}

Meta_Reg$comment[Meta_Reg$iso2=='EE']<-'No sanctions'

colnames(Meta_Reg)<-c("iso2", "country", "registration obligation", "time limit", "comment", "score" )
colnames(Meta_DeReg)<-c('iso2','country', "de-registration obligation", "de-registration obligation third country nationals",
                        "monitoring third country nationals",
                        "administrative corrections",'comment')



Countries<-CountriesS<-unique(NORDIC4$iso2[!is.na(NORDIC4$ICn)|!is.na(NORDIC4$ECn)])
YearS<-sort(unique(NORDIC4$year[!is.na(NORDIC4$ICn)|!is.na(NORDIC4$ECn)]))

Recalc_Meta_DeReg<-function(MetaDeReg,w1,w2,w3,w4,t1,t2, trustnordic){
  cat(w1,w2,w3,w4,t1,t2,trustnordic,'\n')
  MetaDeReg<-DT2DF(MetaDeReg)
  MetaDeReg$`IMEM num`<-NULL
  MetaDeReg$`IMEM score`<-NULL
  MetaDeReg$`QuantMig num`<-NULL
  MetaDeReg$`QuantMig score`<-NULL
  L3col<-c('Low','Medium','High')
  nominator<- w1*(tolower(paste(MetaDeReg[,3]))=='yes')+
    w2*(tolower(paste(MetaDeReg[,4]))=='yes')+
    w3*(tolower(paste(MetaDeReg[,5]))=='yes')+
    w4*(tolower(paste(MetaDeReg[,6]))=='yes')
  denominator<-w1*(tolower(paste(MetaDeReg[,3]))!='unknown')+
    w2*(tolower(paste(MetaDeReg[,4]))!='unknown')+
    w3*(tolower(paste(MetaDeReg[,5]))!='unknown')+
    w4*(tolower(paste(MetaDeReg[,6]))!='unknown')
  score_num<- 1 - nominator / denominator
  score<-paste(cut(score_num,c(0,t1,t2,1),L3col,include.lowest = TRUE))
  isNORDIC<-MetaDeReg$iso2 %in%c('IS','SE','NO','DK','FI')
  if (trustnordic){
    score[isNORDIC]<-L3col[1]
    score_num[(is.nan(score_num)|is.na(score_num))&isNORDIC]<-0
  }
  MetaDeReg$`score num`<-round(score_num,3)
  MetaDeReg$score<-score
  MetaDeReg$score[is.na(score_num)]<-'Unknown'
  IMEM_num<-2-as.numeric(as.factor(IMEM$Undercount.emi.IMEM))
  IMEM_score<-firstCap(IMEM$Undercount.emi.IMEM)
  QuantMig_num<-QUANTMIG$score
  QuantMig_score<-firstCap(QUANTMIG$class)
  MetaDeReg<-fast.merge.df(MetaDeReg,
                           data.frame(iso2=unname(IMEM$Country),
                                      'IMEM num'=unname(IMEM_num),
                                      'IMEM score'=unname(IMEM_score),
                                      'QuantMig num'= unname(QuantMig_num),
                                      'QuantMig score' = unname(QuantMig_score),
                                      stringsAsFactors = FALSE, check.names = FALSE),
                           "iso2")
  MetaDeReg<-datatable(MetaDeReg, options=list(pageLength=nrow(MetaDeReg), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  MetaDeReg<-formatStyle(MetaDeReg, columns = "score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
  MetaDeReg<-formatStyle(MetaDeReg, columns = "QuantMig score", color=styleEqual(c('Excellent', 'Low','High'), c("#008000", "#FFA500","#FF0000")))
  MetaDeReg<-formatStyle(MetaDeReg, columns = "IMEM score", color=styleEqual(c('Low','High'), c("#008000","#FF0000")))
  MetaDeReg<-formatStyle(MetaDeReg, c(2,7,9), "border-right" = "solid 1px", "border-right-color"='black')
  MetaDeReg
}

Recalc_Meta_Reg<-function(MetaReg, trustnordic=TRUE){
  cat(trustnordic,class(MetaReg),'\n')
  MetaReg<-DT2DF(MetaReg)
  MetaReg$score<-NULL
  MetaReg$`score num`<-NULL
  MetaReg$`IMEM num`<-NULL
  MetaReg$`IMEM score`<-NULL
  MetaReg$`QuantMig num`<-NULL
  MetaReg$`QuantMig score`<-NULL
  L3col<-c('Low','Medium','High')
  isNORDIC<-MetaReg$iso2 %in%c('IS','SE','NO','DK','FI')
  MetaReg$`score num`<-(tolower(MetaReg[,3])=='no') + 0.5*(tolower(MetaReg$`time limit`)=='no limit')+ 0.5*(tolower(MetaReg$comment)=='no sanctions')
  MetaReg$score<-L3col[MetaReg$`score num`*2+1]
  unk<-tolower(MetaReg[,3])=='unknown'
  MetaReg$score[unk]<-'Unknown'
  MetaReg$`score num`[unk]<-NA
  if (trustnordic){
    MetaReg$score[isNORDIC] <- L3col[1]
    MetaReg$`score num`[isNORDIC] <- 0
  }
  IMEM_num<-2-as.numeric(as.factor(IMEM$Undercount.imm.IMEM))
  QuantMig_num<-QUANTMIG$score
  QuantMig_score<-QUANTMIG$class
  MetaReg<-fast.merge.df(MetaReg,
                         data.frame(iso2=IMEM$Country,
                                    'IMEM num'=IMEM_num,
                                    'IMEM score'=firstCap(IMEM$Undercount.imm.IMEM),
                                    'QuantMig num'= unname(QuantMig_num),
                                    'QuantMig score' = firstCap(unname(QuantMig_score)),
                                    stringsAsFactors = FALSE,check.names = FALSE),
                         "iso2")
  MetaReg<-datatable(MetaReg, rownames=FALSE, options=list(pageLength=nrow(MetaReg), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  MetaReg<-formatStyle(MetaReg, columns = "score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
  MetaReg<-formatStyle(MetaReg, columns = "IMEM score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
  MetaReg<-formatStyle(MetaReg, columns = "QuantMig score", color=styleEqual(c('Excellent', 'Low','High'), c("#008000", "#FFA500","#FF0000")))
  MetaReg<-formatStyle(MetaReg, columns = "time limit", fontWeight = styleEqual('No limit', c("bold")))
  MetaReg<-formatStyle(MetaReg, columns = "comment", fontWeight = styleEqual('No sanctions', c("bold")))
  MetaReg<-formatStyle(MetaReg, c(2,5,7), "border-right" = "solid 1px", "border-right-color"='black')
  MetaReg
}

# direction='I';
# #metadata
# w1=0.5; w2=0.1; w3=0.1; w4=0.3; t1=0.3; t2=0.6; ItrustNordic = TRUE; EtrustNordic = TRUE;
# #model
# ncp=1; separated=FALSE; additive=TRUE; refcountries=9; durationCorrection = 13;
# IgnoreOverCounting = TRUE;
# TranslateGroups = 5;
# #mixing
# useimputation=TRUE;
# threshyear = 2008; FinalGroups = 5; w_imemA = 0.1; w_imemB = 0.25; w_metaA = 0.1; w_metaB = 0.15; w_modelA = 0.8;w_modelB = 0.6


CalcModel<-function(#META, MODEL, thr1=0.25, thr2=0.6, wimema=0.25,
  direction,
  #metadata
  w1, w2, w3, w4, t1, t2, ItrustNordic, EtrustNordic,
  #model
  ncp, separated, additive, refcountries, durationCorrection,
  IgnoreOverCounting,
  UseQuantiles,
  TranslateGroups,
  #mixing
  useimputation,
  threshyear, FinalGroups, w_imemA, w_imemB , w_metaA , w_metaB , w_modelA ,w_modelB ){


  RES<-get_undercounting(direction=direction,immi_meta_options=list(w1 = w1,
                                                                    w2 = w2,
                                                                    w3 = w3,
                                                                    w4 = w4,
                                                                    t1 = t1,
                                                                    t2 = t2,
                                                                    trustNordic = ItrustNordic),
                         emi_meta_options=list(trustNordic = EtrustNordic),
                         model_options = list(ncp=ncp,
                                              useimputation=useimputation,
                                              weighted=FALSE,
                                              separated=separated,
                                              additive=additive,
                                              refcountries=refcountries,
                                              durationCorrection = durationCorrection),
                         model_classification_options = list(
                           UserThresholds=NA,
                           IgnoreOverCounting = IgnoreOverCounting,
                           UseQuantiles = UseQuantiles,
                           TranslateGroups = TranslateGroups

                         ),
                         mixing_options=list(
                           threshyear = threshyear,
                           FinalGroups = FinalGroups,
                           w_imemA = w_imemA,
                           w_imemB = w_imemB,
                           w_metaA = w_metaA,
                           w_metaB = w_metaB,
                           w_modelA = w_modelA,
                           w_modelB = w_modelB))
  RES
}

################################################################################
# if I add option to change the threshold they have to be reset on every change of reference country and other paprameters
# RES$R.UserThresholds
################################################################################

plotModel<-function(RES, shownodat=TRUE) {
  par(mar=c(4,3,1.5,8),oma=c(0,0,0,0))
  LEVELS<-1:RES$model.groups
  LEVELS<-format(round((LEVELS-1)/(RES$model.groups-1),2),digits=2, nsmall=2)
  #LEVELS[1]<-paste(LEVELS[1],'(lowest)')
  #LEVELS[RES$model.groups]<-paste(LEVELS[RES$model.groups],'(highest)')
  if (shownodat) {
    my2dplot(1-RES$R.Score.Num, LEVELS=LEVELS, namat =  RES$NoData[rownames(RES$R.Score.Num),])
  } else {
    my2dplot(1-RES$R.Score.Num, LEVELS=LEVELS)
  }
}

getDuration<-function(direction, country){
  print(country)
  if (direction=='I'){
    res<-rbind(duration_immi_array[CountriesS,country,paste(YearS)])
    res[res==60]<-'P'
    res<-data.frame(res,check.names = FALSE,stringsAsFactors = FALSE)
  } else if (direction=='E'){
    res<-rbind(duration_emi_array[country,CountriesS,paste(YearS)])
    res[res==60]<-'P'
    res<-data.frame(res,check.names = FALSE,stringsAsFactors = FALSE)
  }
  res[rownames(res)!=country,]
}

getModel<-function(RES) { #to be xported as xlsx or rdata

  list(
    #score = data.frame((1-RES$R.Score.Num)*(RES$model.groups-1)+1,check.names = FALSE, stringsAsFactors = FALSE), #RES$R.Score
    score = data.frame((1-RES$R.Score.Num),check.names = FALSE, stringsAsFactors = FALSE), #RES$R.Score
    nodata = data.frame(RES$NoData[rownames(RES$R.Score.Num),],check.names = FALSE, stringsAsFactors = FALSE),
    logindex = data.frame(RES$R.RawScore,check.names = FALSE, stringsAsFactors = FALSE),
    logindexthresholds = data.frame('Threshold value'=unname(RES$R.UserThresholds), check.names = FALSE)
  )
}

saveModel<-function(filename, RES){
  print(filename)
  RES<-getModel(RES)
  openxlsx::write.xlsx(RES, filename, FALSE, FALSE, colNames = TRUE, rowNames=TRUE)
  # xlsx::write.xlsx(RES$score, file = filename, sheetName = 'score', append = FALSE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$nodata, file = filename, sheetName = 'no data', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$logindex, file = filename, sheetName = 'log ratio', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$logindexthresholds, file = filename, sheetName = 'log ratio thresholds', append = TRUE, row.names = TRUE, col.names = TRUE)
}


plotCombined<-function(RES, shownodat=TRUE) {
  par(mar=c(4,3,1.5,9.2),oma=c(0,0,0,0))
  if (shownodat) {
    my2dplot(1-RES$C.Score.Num, LEVELS=RES$LEVELS, namat =  RES$NoData[rownames(RES$C.Score.Num),])
  } else {
    my2dplot(1-RES$C.Score.Num, LEVELS=RES$LEVELS)
  }
}

getCombined<-function(RES) {
  list(score=data.frame(RES$C.Score,check.names = FALSE, stringsAsFactors = FALSE),
       scorenum=data.frame((1-RES$C.Score.Num)*(RES$final.groups-1)+1,check.names = FALSE, stringsAsFactors = FALSE),
       nodata=data.frame(RES$NoData,check.names = FALSE, stringsAsFactors = FALSE),
       raw=data.frame(1-RES$C.RawScore,check.names = FALSE, stringsAsFactors = FALSE),
       rawthresholds = data.frame('Threshold value'=unname(RES$C.UserThresholds), check.names = FALSE)
  )
}

saveCombined<-function(filename, RES){
  RES<-getCombined(RES)
  openxlsx::write.xlsx(RES, filename, FALSE, FALSE, colNames = TRUE, rowNames=TRUE)
  # xlsx::write.xlsx(RES$score, file = filename, sheetName = 'score', append = FALSE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$scorenum, file = filename, sheetName = 'scorenum', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$nodata, file = filename, sheetName = 'no data', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$raw, file = filename, sheetName = 'raw', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$rawthresholds, file = filename, sheetName = 'thresholds', append = TRUE, row.names = TRUE, col.names = TRUE)
}

saveBFR<-function(filename, RES){
  #RES
  print('XLSX SAVE')
  print(names(RES))
  print(filename)
  wb <- createWorkbook()
  addWorksheet(wb, "Used options")
  addWorksheet(wb, "Original")
  addWorksheet(wb, "Imputed")
  addWorksheet(wb, "Raw")
  Options<-data.frame(Parameter=c('Migration type','Reference countries','Duration of stay correction reference countries','Optimization criterion',
                                  'Method of estimation','ncp for imputation method'),
                      Value=c(c('Immigration','Emigration')[1+(RES$direction=='E')], 
                              RES$type[2], RES$type[1], 
                              c('Multiplicative','Additive')[1+(RES$additive)],
                              c('Together for immigration and emigration','Separated for immigration and emigration')[1+(RES$separated)],
                              RES$ncp))
  writeData(wb, "Used options", Options, startRow = 1, startCol = 1)
  writeData(wb, "Original", RES$original, startRow = 1, startCol = 1,rowNames = TRUE)
  writeData(wb, "Imputed", RES$imputed, startRow = 1, startCol = 1, rowNames = TRUE)
  writeData(wb, "Raw", RES$raw, startRow = 1, startCol = 1)
  
  saveWorkbook(wb, file = filename, overwrite = FALSE)
  
#  openxlsx::write.xlsx(RES$original, filename, asTable=FALSE, overwrite = FALSE, colNames = TRUE, rowNames=TRUE, sheetName = 'Original',append = FALSE)
#  openxlsx::write.xlsx(RES$imputed, filename, asTable=FALSE, overwrite = TRUE, colNames = TRUE, rowNames=TRUE, sheetName = 'Imputed',append = TRUE)
#  openxlsx::write.xlsx(RES$raw, filename, asTable=FALSE, overwrite = TRUE, colNames = TRUE, rowNames=TRUE, sheetName = 'Raw',append = TRUE)
  print('SAVED')
  #original=D, imputed=imputed_data, raw
  # xlsx::write.xlsx(RES$score, file = filename, sheetName = 'score', append = FALSE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$scorenum, file = filename, sheetName = 'scorenum', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$nodata, file = filename, sheetName = 'no data', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$raw, file = filename, sheetName = 'raw', append = TRUE, row.names = TRUE, col.names = TRUE)
  # xlsx::write.xlsx(RES$rawthresholds, file = filename, sheetName = 'thresholds', append = TRUE, row.names = TRUE, col.names = TRUE)
}

