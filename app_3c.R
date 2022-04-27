rm(list=ls())

#################################################
# To do
# what with LI
# check help boxes names 
# check/set init values
# further clean code
# check code functionality
# describe ignore overcounting
# check new vs old undercounting calcualtion (print screen)
################################################

#Sys.setlocale("LC_ALL","en_US.UTF-8")
#install.packages(c('Cairo','DT','shiny','colourpicker','shinyhelper','magicaxis','data.table','countrycodes'))

library(Cairo)
library(RColorBrewer)
library(devtools)
library(DT)
library(shiny)
library(shinyWidgets)
library(usethis)
#library(colourpicker)
library(shinyhelper)
library(magicaxis)
library(data.table)
library(countrycode)
library(openxlsx)
source('./code/UNDERCOUNTING_PKG_APP.R')


options(bitmapType="cairo")

#loadaslist<-function(Name){G<-new.env(); load(Name,G); as.list(G)}
toeurostat<-function(x) {x[x=='GR']<-'EL'; x[x=='GB']<-'UK'; x}

#load('./data/MetaData.rda')

# IMEM$Country[IMEM$Country=='EL']<-'GR'
# IMEM<-IMEM[!is.na(IMEM$Country),]
# IMEM<-IMEM[order(IMEM$Country),]
# Meta_DeReg$iso2[Meta_DeReg$iso2=='EL']<-'GR'
# Meta_DeReg<-Meta_DeReg[order(Meta_DeReg$iso2),]
# Meta_Reg$iso2[Meta_Reg$iso2=='EL']<-'GR'
# Meta_Reg<-Meta_Reg[order(Meta_Reg$iso2),]
# 
# 
# DAT_IMEM<-loadaslist('./data/UndercountingIndex_IMEM.rda')
# DAT_POIS<-loadaslist('./data/UndercountingIndex_Willekens_Poisson.rda')
# DAT_EXPERT<-loadaslist('./data/UndercountingIndex_Willekens_Expert.rda')
# DAT_MIXED<-loadaslist('./data/UndercountingIndex_Willekens_Mixture.rda')

# sortisoyear<-function(z) {
#   lapply(z, function(x) {
#   x$iso2[x$iso2=='EL']<-'GR'
#   x$ind<-paste(x$iso2,x$year)
#   x<-x[order(x$ind),]
#   x$ind<-NULL
#   x
#   })
# }
# 
# DAT_IMEM<-sortisoyear(DAT_IMEM)
# DAT_POIS<-sortisoyear(DAT_POIS)
# DAT_EXPERT<-sortisoyear(DAT_EXPERT)
# DAT_MIXED<-sortisoyear(DAT_MIXED)


Meta_Reg$comment[Meta_Reg$iso2=='EE']<-'No sanctions'

colnames(Meta_Reg)<-c("iso2", "country", "registration obligation", "time limit", "comment", "score" )
colnames(Meta_DeReg)<-c('iso2','country', "de-registration obligation", "de-registration obligation third country nationals",
                        "monitoring third country nationals",
                        "administrative corrections",'comment')

Countries<-CountriesS<-unique(NORDIC4$iso2[!is.na(NORDIC4$ICn)|!is.na(NORDIC4$ECn)])

BB <- c(1, 0.75, 0.5, 0.25, 0.125, 1e-5)

DT2DF<-function(x) if (class(x)[1]=='datatables') {
  z<-x$x$data
  if (colnames(z)[1]%in%c('',' ','  ')) z<-z[,-1]
  data.frame(z, stringsAsFactors = FALSE, check.names = FALSE, check.rows = FALSE)
} else data.frame(x, stringsAsFactors = FALSE, check.names = FALSE, check.rows = FALSE)

colMedians <- function(x) apply(x,2,median, na.rm=TRUE)
colSd <- function(x) apply(x,2,sd, na.rm=TRUE)
colQlo <- function(x) apply(x,2,quantile, probs=0.025, na.rm=TRUE)
colQhi <- function(x) apply(x,2,quantile, probs=0.975, na.rm=TRUE)

# firstCap<-function(x) {
#   x<-paste(x)
#   tmp<-paste(toupper(substr(x,1,1)),substr(x,2,nchar(x)),sep='')
#   tmp[tmp=='NA']<-NA
#   tmp
# }

Recalc_Meta_DeReg<-function(MetaDeReg,w1,w2,w3,w4,t1,t2, trustnordic){
  cat(w1,w2,w3,w4,t1,t2,trustnordic,'\n')
  MetaDeReg<-DT2DF(MetaDeReg)
  MetaDeReg$`IMEM num`<-NULL
  MetaDeReg$`IMEM score`<-NULL
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
  MetaDeReg<-fast.merge.df(MetaDeReg, 
                           data.frame(iso2=unname(IMEM$Country),
                                      'IMEM num'=unname(IMEM_num), 
                                      'IMEM score'=unname(IMEM_score),stringsAsFactors = FALSE,check.names = FALSE),
                           "iso2")
  MetaDeReg<-datatable(MetaDeReg, options=list(pageLength=nrow(MetaDeReg), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  MetaDeReg<-formatStyle(MetaDeReg, columns = "score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
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
  MetaReg<-fast.merge.df(MetaReg, 
                         data.frame(iso2=IMEM$Country,
                                    'IMEM num'=IMEM_num, 
                                    'IMEM score'=firstCap(IMEM$Undercount.imm.IMEM),stringsAsFactors = FALSE,check.names = FALSE),
                         "iso2")
  MetaReg<-datatable(MetaReg, rownames=FALSE, options=list(pageLength=nrow(MetaReg), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  MetaReg<-formatStyle(MetaReg, columns = "score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
  MetaReg<-formatStyle(MetaReg, columns = "IMEM score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
  MetaReg<-formatStyle(MetaReg, columns = "time limit", fontWeight = styleEqual('No limit', c("bold")))
  MetaReg<-formatStyle(MetaReg, columns = "comment", fontWeight = styleEqual('No sanctions', c("bold")))
  MetaReg<-formatStyle(MetaReg, c(2,5,7), "border-right" = "solid 1px", "border-right-color"='black')
  MetaReg
}


# reformatIE2tab<-function(tab, chide=TRUE, COLO){
#   tab<-DT2DF(tab)
#   if (chide) tab<-tab[tab[,1]%in%Countries,]
#   cat(COLO,'\n')
#   #COLO<-adjustcolor(COLO,blue.f = 0.9,red.f = 0.9,#008000.f = 0.9)
#   tab$B.score<-firstCap(tab$B.score)
#   tab$A.score<-firstCap(tab$A.score)
#   colnames(tab)<-c('iso2','country','median (B)','lo (B)','hi (B)','score (B)','score num (B)','median (A)','lo (A)','hi (A)','score (A)','score num (A)')
#   LEVELS<-firstCap(c('very low','low', 'medium','high','very high'))
#   tab<-datatable(tab,options=list(pageLength=nrow(tab), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))),
#                  rownames=FALSE)
#   tab<-formatStyle(tab, columns = "score (B)", color=styleEqual(LEVELS, COLO))
#   tab<-formatStyle(tab, c(2,7), "border-right" = "solid 1px", "border-right-color"='black')
#   tab<-formatStyle(tab, columns = "score (A)", color=styleEqual(LEVELS, COLO))
#   tab
# }


# 
# 
# CBA<-function(refcountry=1, threshyear = 2008, direction='E',corrected=1,
#               thr1=BB[2],thr2=BB[3],thr3=BB[4],thr4=BB[5],
#               NBoot=1e5, LEVELS=c('very low','low','medium','high','very high')){
# 
#   DAT0<-switch(corrected, '0' = DAT_IMEM, '1' = DAT_IMEM, '2' = DAT_EXPERT, '3' = DAT_POIS, '4' = DAT_MIXED)
#   RES<-switch(refcountry, '1' = DAT0$NORDIC, '2' = DAT0$NORDIC_PLUS_BE_CH, '3' = DAT0$NORDIC_PLUS_AT_BE_CH_NL,
#               '4' = DAT0$NORDIC_PLUS_AT_DE_BE_CH_FR_NL, '5' = DAT0$ALL_COUNTRIES)
# 
#   # RES<-switch(refcountry, '1' = NORDIC, '2' = NORDIC_PLUS_BE_CH,
#   #             '3' = NORDIC_PLUS_AT_BE_CH_NL,
#   #             '4' = NORDIC_PLUS_AT_DE_BE_CH_FR_NL, '5' = ALL_COUNTRIES)
#   THRESH<-log10(1/c(1,thr1,thr2,thr3,thr4,1e-5))
# 
#   if (direction=='E' && corrected>0){
#     RES$Y<-RES$ECraw
#     RES$YE<-RES$EC
#     RES$Ysd<-RES$EC_sd
#     RES$W<-RES$POPEn
#   } else if (direction=='E' && corrected==0){
#     RES$Y<-RES$EUCraw
#     RES$YE<-RES$EUC
#     RES$Ysd<-RES$EUC_sd
#     RES$W<-RES$POPEn
#   } else if (direction=='I' && corrected>0){
#     RES$Y<-RES$ICraw
#     RES$YE<-RES$IC
#     RES$Ysd<-RES$IC_sd
#     RES$W<-RES$POPIn
#   } else if (direction=='I' && corrected==0){
#     RES$Y<-RES$IUCraw
#     RES$YE<-RES$IUC
#     RES$Ysd<-RES$IUC_sd
#     RES$W<-RES$POPIn
#   }
# 
#   cols<-c('iso2','Y','YE','Ysd','W','year')
#   RESLO<-RES[RES$year<threshyear,cols]
#   RESHI<-RES[RES$year>=threshyear,cols]
# 
#   UG<-unique(RES$iso2)
# 
#   RESHIStat<-lapply(UG, function(k) {
#     cat(k,'\n')
#     tmp<-RESHI[RESHI$iso2==k,]
#     Qua<-c(NA,NA,NA)
#     Qme<-NA
#     if (!all(is.na(tmp$Y))) {
#       org<-tmp<-tmp[!is.na(tmp$Y),]
#       tmp<-tmp[!is.na(tmp$Ysd),]
#       H<-tmp$W/sum(tmp$W)
#       if (length(H)>0){
#         ToBoot<-NBoot
#         YY<-NULL
#         for (j in 1:5) {
#           BOOTini<-sample(seq_along(H), ToBoot, prob=H, replace = TRUE)
#           YYy<-as.vector(rnorm(ToBoot,tmp$Y[BOOTini],tmp$Ysd[BOOTini]))
#           YYy<-YYy[YYy>0]
#           YY<-c(YY,YYy)
#           ToBoot<-ToBoot-length(YY)
#           if ((ToBoot/NBoot)<0.01) break
#         }
#         Qua<-quantile(YY,probs=c(0.025,0.5,0.975))
#         Qme<-mean(YY)
#       } else {
#         Qua<-c(NA,median(org$Y),NA)
#         Qme<-mean(org$Y)
#       }
#     }
#     list(iso2=k, median=Qua[2], q_lo=Qua[1], q_hi=Qua[3], mean=Qme)#, sd=Qsd)
#   })
#   RESHIStat<-data.frame(data.table::rbindlist(RESHIStat),stringsAsFactors = FALSE)
# 
#   RESLOStat<-lapply(UG, function(k) {
#     cat(k,'\n')
#     tmp<-RESLO[RESLO$iso2==k,]
#     Qua<-c(NA,NA,NA)
#     Qme<-NA
#     if (!all(is.na(tmp$Y))) {
#       tmp<-tmp[!is.na(tmp$Y),]
#       tmp<-tmp[!is.na(tmp$Ysd),]
#       H<-tmp$W/sum(tmp$W)
#       if (length(H)>0){
#         ToBoot<-NBoot
#         YY<-NULL
#         for (j in 1:5) {
#           BOOTini<-sample(seq_along(H), ToBoot, prob=H, replace = TRUE)
#           YYy<-as.vector(rnorm(ToBoot,tmp$Y[BOOTini],tmp$Ysd[BOOTini]))
#           YYy<-YYy[YYy>0]
#           YY<-c(YY,YYy)
#           ToBoot<-ToBoot-length(YY)
#           if ((ToBoot/NBoot)<0.01) break
#         }
# 
#         Qua<-quantile(YY, probs=c(0.025,0.5,0.975))
#         Qme<-mean(YY)
#       }
#     }
#     list(iso2=k, median=Qua[2], q_lo=Qua[1], q_hi=Qua[3], mean=Qme)#, sd=Qsd)
#   })
#   RESLOStat<-data.frame(data.table::rbindlist(RESLOStat),stringsAsFactors = FALSE)
# 
#   negi<-function(x) {x[x>=0]<- 1e-9; abs(x)}
#   B <- RESLOStat$median
#   A <- RESHIStat$median
#   RESLOStat<-data.frame(Est = RESLOStat$median, lo.CI = RESLOStat$q_lo, hi.CI = RESLOStat$q_hi)
#   RESHIStat<-data.frame(Est = RESHIStat$median, lo.CI = RESHIStat$q_lo, hi.CI = RESHIStat$q_hi)
# 
#   cat(' >> ',THRESH,'\n')
#   resB <- cut(x=negi(log10(B)), breaks=THRESH,include.lowest=TRUE,labels=LEVELS)
#   resA <- cut(x=negi(log10(A)), breaks=THRESH,include.lowest=TRUE,labels=LEVELS)
# 
#   resBnum <- round((as.numeric(resB)-1)/(length(THRESH)-2),4)
#   resAnum <- round((as.numeric(resA)-1)/(length(THRESH)-2),4)
# 
#   stand<-function(x,r) (x-r[1])/diff(r)
#   
#   resBnum2 <- round(stand(negi(log10(B)),range(negi(log10(B)),negi(log10(A)),na.rm = TRUE)),4)
#   resAnum2 <- round(stand(negi(log10(A)),range(negi(log10(B)),negi(log10(A)),na.rm = TRUE)),4)
#   
#   #stop('UK is missing!')
#   
#   res<-data.frame(iso2=UG,
#                   country=countrycode::countrycode(toeurostat(UG),'eurostat','country.name'),
#                   B=round(RESLOStat,4),
#                   B.score=resB,
#                   #B.score.num=resBnum,
#                   B.score.num=resBnum2,
#                   A=round(RESHIStat,4),
#                   A.score=resA,
#                   #A.score.num=resAnum,
#                   A.score.num=resAnum2,
#                   stringsAsFactors = FALSE,
#                   check.names = FALSE)
#   res
# }
# 
# plot.BA<-function(RES,thr1=BB[2],thr2=BB[3],thr3=BB[4],thr4=BB[5], threshyear=2008, plotci=TRUE, logscale=TRUE, colCI='#FFFFFF',hidec=FALSE,
#                   #COLO=c("#2da70b","#306005","#fff00f","#c05508","#ff9f9f")
#                   COLO){
#   if (hidec) RES<-RES[RES$iso2 %in% Countries,]
#   if (logscale) flog10<-log10 else flog10<-function(x) identity(x)
#   THRESH<-flog10(1/c(1+(!logscale)*99,thr1,thr2,thr3,thr4,1e-5))
#   if (!logscale) THRESH = 1/THRESH
#   #COL=c(rgb(0.8,0.4,0.1),rgb(0.3,0.4,0.9))
#   COL<-c('#D5D505','#000080')
#   COLd<-c('#A0A005','#000080')
#   #COLl<-c('#D5D505','#000090')
#   if (logscale) neg2NA<-function(x) {x[x<=0]<-NA;x} else neg2NA<-function(x) {x[x<=0]<-0;x}
#   X<-rbind(flog10(RES$B.Est),flog10(RES$A.Est))
#   par(oma=c(0,0,2,0),mar=c(2.65,4,2.5,0))
#   YLIM<-flog10(c(min(neg2NA(RES$B.lo.CI),neg2NA(RES$A.lo.CI),neg2NA(RES$B.Est),neg2NA(RES$A.Est),na.rm=TRUE),
#                  max(RES$B.hi.CI,RES$A.hi.CI,RES$A.Est,RES$B.Est,na.rm = TRUE)))
#   if(!logscale) YLIM<-1.1*c(0,max(min(2,YLIM[2]),RES$A.Est,RES$B.Est,na.rm = TRUE))
#   z<-barplot(X,axes=F,beside=TRUE,col=COL, ylim=YLIM)
#   axis(1, at=colMeans(z),labels = RES$iso2,las=3,cex.axis=1.3, font=1);
#   mtext('Bilateral flows ratio',2,2.5,cex=1.5)
#   box();box();
#   # col.pal=c(adjustcolor('green',alpha.f = 0.5),
#   #           adjustcolor('green4',alpha.f = 0.5),
#   #           adjustcolor('yellow2',alpha.f = 0.5),
#   #           adjustcolor('orange',alpha.f = 0.5),
#   #           adjustcolor('red2',alpha.f = 0.25))
#   col.pal<-COLO
#   if(logscale){
#     rect(-10,-THRESH[6],max(z)*2,THRESH[6],col=col.pal[5],border=NA)
#     rect(-10,-THRESH[5],max(z)*2,THRESH[6],col=col.pal[4],border=NA)
#     rect(-10,-THRESH[4],max(z)*2,THRESH[6],col=col.pal[3],border=NA)
#     rect(-10,-THRESH[3],max(z)*2,THRESH[6],col=col.pal[2],border=NA)
#     rect(-10,-THRESH[2],max(z)*2,THRESH[6],col=col.pal[1],border=NA)
#     abline(h=-THRESH,lwd=2)
#   } else {
#     rect(-10,20,max(z)*2,-1,col=col.pal[5],border=NA)
#     rect(-10,THRESH[5],max(z)*2,20,col=col.pal[4],border=NA)
#     rect(-10,THRESH[4],max(z)*2,20,col=col.pal[3],border=NA)
#     rect(-10,THRESH[3],max(z)*2,20,col=col.pal[2],border=NA)
#     rect(-10,THRESH[2],max(z)*2,20,col=col.pal[1],border=NA)
#     abline(h=THRESH[-length(THRESH)],lwd=2)
#   }
#   abline(v=colMeans(z),lty=3,col=rgb(0.6,0.6,0.6))
# 
#   axis(3, at=z[1,]-0.15,labels =toupper(RES$B.score),las=3,cex.axis=0.75,col.axis=COLd[1],padj=0.5)
#   axis(3, at=z[2,]+0.15,labels =toupper(RES$A.score),las=3,cex.axis=0.75,col.axis=COLd[2],padj=0.5)
#   z<-barplot(X,axes=F,beside=TRUE,col=COL,add=TRUE);box();box()
#   if (logscale) magicaxis::magaxis(2,unlog=T,cex.axis=1.5) else axis(2,cex.axis=1.5)
#   if (plotci) {
#     LOCI<-rbind(flog10(RES$B.lo.CI),flog10(RES$A.lo.CI))
#     HICI<-rbind(flog10(RES$B.hi.CI),flog10(RES$A.hi.CI))
#     for (j in 1:2) for(k in 1:ncol(z)){
#       lines(c(z[j,k],z[j,k]),c(LOCI[j,k],HICI[j,k]),lwd=2,col=colCI)
#     }
#   }
#   if(logscale) lpos<-'bottomright' else lpos<-'topright'
#   legend(lpos,bty='o',c(paste('Before',threshyear), paste('After',threshyear-1)),text.col = COL,fill=COL,cex=1.5,
#          bg=adjustcolor('white',0.3),box.col=adjustcolor('white',0.3))
# 
# }
# 

direction='I';
#metadata
w1=0.5; w2=0.1; w3=0.1; w4=0.3; t1=0.3; t2=0.6; ItrustNordic = TRUE; EtrustNordic = TRUE;
#model
ncp=1; separated=FALSE; additive=TRUE; refcountries=9; durationCorrection = 13;
IgnoreOverCounting = TRUE;
TranslateGroups = 5;
#mixing
useimputation=TRUE;
threshyear = 2008; FinalGroups = 5; w_imemA = 0.1; w_imemB = 0.25; w_metaA = 0.1; w_metaB = 0.15; w_modelA = 0.8;w_modelB = 0.6


CalcModel<-function(#META, MODEL, thr1=0.25, thr2=0.6, wimema=0.25,
  #wimemb=0.5, wmetaa=0.2, wmetab=0.2, wmodela=1, wmodelb=1,
  #mirror=TRUE, direction='I',
  
  # direction='I',
  # #metadata
  # w1=0.5, w2=0.1, w3=0.1, w4=0.3, t1=0.3, t2=0.6, ItrustNordic = TRUE, EtrustNordic = TRUE,
  # #model
  # ncp=1, separated=FALSE, additive=TRUE, refcountries=9, durationCorrection = 13,
  # IgnoreOverCounting = TRUE,
  # TranslateGroups = 5,
  # #mixing
  # useimputation=TRUE,
  # threshyear = 2008, FinalGroups = 5, w_imemA = 0.1, w_imemB = 0.25, w_metaA = 0.1, w_metaB = 0.15, w_modelA = 0.8,w_modelB = 0.6){
  # 
  direction,
  #metadata
  w1, w2, w3, w4, t1, t2, ItrustNordic, EtrustNordic,
  #model
  ncp, separated, additive, refcountries, durationCorrection,
  IgnoreOverCounting,
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
  par(mar=c(4,3,1.5,8.5),oma=c(0,0,0,0))
  LEVELS<-1:RES$model.groups
  LEVELS[1]<-paste(LEVELS[1],'(lowest)')
  LEVELS[RES$model.groups]<-paste(LEVELS[RES$model.groups],'(highest)')
  if (shownodat) {
    my2dplot(1-RES$R.Score.Num, LEVELS=LEVELS, namat =  RES$NoData[rownames(RES$R.Score.Num),])
  } else {
    my2dplot(1-RES$R.Score.Num, LEVELS=LEVELS)
  }
}

getModel<-function(RES) { #to be xported as xlsx or rdata
  
  list(
    score = data.frame((1-RES$R.Score.Num)*(RES$model.groups-1)+1,check.names = FALSE, stringsAsFactors = FALSE), #RES$R.Score
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
       raw=data.frame(RES$C.RawScore,check.names = FALSE, stringsAsFactors = FALSE),
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
#   
#   RES$R.Score.Num
#   
#   cat(thr1, thr2, wimema, wimemb, wmetaa, wmetab, wmodela, wmodelb, mirror, '\n')
#   META<-DT2DF(META)
#   Msc<-META$`score num`
# 
#   LIMEM<-c('Low','High')
#   IMEMi<-IMEM[1:nrow(META),]
#   if(!all(IMEMi$Country==META$iso2)) {
#     print(cbind(IMEMi$Country,META$iso2))
#     stop('wrong country names')
#   }
#   if(direction=='I') posI<-2 else if(direction=='E') posI<-3
#   IMEMc<-factor(firstCap(unlist(IMEMi[,posI])),levels=LIMEM)
#   IMEMi<-as.numeric(IMEMc)-1
# 
#   MODEL<-DT2DF(MODEL)
#   Bsc<-MODEL$B.score.num
#   Asc<-MODEL$A.score.num
# 
#   L3<-c('Low' ,'Medium', 'High')
# 
#   namod<-function(x,y){
#     z<-rep(y,length(x))
#     z[is.na(x)]<-0
#     z
#   }
# 
#   na2zero<-function(x) {x[is.na(x)]<-0; x}
# 
#   IAsc<-is.na(Asc)
#   IBsc<-is.na(Bsc)
# 
#   IAsc2<-IAsc&!IBsc
#   IBsc2<-IBsc&!IAsc
# 
#   IAsc<-IAsc2
#   IBsc<-IBsc2
# 
#   if (mirror){
#     Asc[is.na(Asc)]<-Bsc[is.na(Asc)]
#     Bsc[is.na(Bsc)]<-Asc[is.na(Bsc)]
#   }
# 
#   BM<-na2zero(Bsc*wmodelb)
#   BI<-na2zero(IMEMi*wimemb)
#   BE<-na2zero(Msc*wmetab)
#   AM<-na2zero(Asc*wmodela)
#   AI<-na2zero(IMEMi*wimema)
#   AE<-na2zero(Msc*wmetaa)
#   Bres<-(BM+BI+BE)/(namod(Bsc,wmodelb)+namod(IMEMi,wimemb)+namod(Msc,wmetab))
#   Ares<-(AM+AI+AE)/(namod(Asc,wmodela)+namod(IMEMi,wimema)+namod(Msc,wmetaa))
# 
#   Bscore<-cut(Bres,c(0,thr1,thr2,1),L3,include.lowest = TRUE)
#   Ascore<-cut(Ares,c(0,thr1,thr2,1),L3,include.lowest = TRUE)
#   if (!mirror) {
#     RR<-data.frame(iso2=META[,1],country=META[,2], 'IMEM score num' = IMEMi, 'metadata score num' = Msc, 'model score num (B)' = Bsc, 'model score num (A)' = Asc,
#                    'combined score num (B)' = round(Bres,3), 'combined score num (A)' = round(Ares,3), 'combined score (B)' = Bscore, 'combined score (A)' = Ascore,
#                    check.names = FALSE, stringsAsFactors = FALSE)
#     RR<-datatable(RR, rownames=FALSE, options=list(pageLength=nrow(RR), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
#   } else {
# 
#     RR<-data.frame(iso2=META[,1],country=META[,2], 'IMEM score num' = IMEMi, 'metadata score num' = Msc, 'model score num (B)' = Bsc, 'model score num (A)' = Asc,
#                    'combined score num (B)' = round(Bres,3), 'combined score num (A)' = round(Ares,3), 'combined score (B)' = Bscore, 'combined score (A)' = Ascore,
#                    Indb = IBsc, Inda = IAsc,
#                    check.names = FALSE, stringsAsFactors = FALSE)
#     RR<-datatable(RR, rownames=FALSE, options=list(pageLength=nrow(RR), lengthMenu=-1, dom='ft',
#                                                    columnDefs = list(list(className = 'dt-center', targets = '_all'),
#                                                                      list(visible=FALSE, targets=c(11,10)))
#     ))
#     RR<-formatStyle(RR, columns = c("model score num (A)","model score num (B)"), valueColumns = c('Inda','Indb'), color=styleEqual(1,"magenta"))
#   }
#   RR<-formatStyle(RR, columns = "combined score (A)", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
#   RR<-formatStyle(RR, columns = "combined score (B)", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000")))
#   RR<-formatStyle(RR, c(9,10), backgroundColor = "#fff0ee")
#   RR<-formatStyle(RR, c(2,6,8), "border-right" = "solid 1px", "border-right-color"='black')
#   RR
# }

# summaryTable<-function(META, MODEL, COMBI, direction='I', COLO){
#   META<-DT2DF(META)
#   COMBI<-DT2DF(COMBI)
#   MODEL<-DT2DF(MODEL)
#   LIMEM<-c('Low','High')
#   IMEMi<-IMEM[1:nrow(META),]
#   if(!all(IMEMi$Country==META$iso2)) stop()
#   if(direction=='I') posI<-2 else if(direction=='E') posI<-3
#   IMEMc<-factor(firstCap(unlist(IMEMi[,posI])),levels=LIMEM)
# 
#   RED<-"#FF0000"#'#AA0000'
#   GREEN<-"#008000"#'#00AA00'
#   ORANGE<-"#FFA500"#'#FF7F00'
#   # LIGHTRED<-'#FF5555'
#   # LIGHTGREEN<-'#7FEE7F'
#   # COLO<-c(LIGHTGREEN,GREEN,ORANGE,RED,LIGHTRED)
#   LEVELS<-firstCap(c('very low','low', 'medium','high','very high'))
#   RR<-data.frame(iso2=META$iso2, country=META$country,'IMEM score' = IMEMc, 'metadata score' = META$score,
#                  "model score (B)" = firstCap(MODEL$B.score), "model score (A)" = firstCap(MODEL$A.score),
#                  "combined score (B)" = COMBI$`combined score (B)`,"combined score (A)" = COMBI$`combined score (A)`,
#                  stringsAsFactors = FALSE, check.names = FALSE)
#   RR<-datatable(RR, rownames=FALSE, options=list(pageLength=nrow(RR), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
#   RR<-formatStyle(RR, columns = "IMEM score", color=styleEqual(c('Low','High'), c(GREEN,RED)))
#   RR<-formatStyle(RR, columns = "metadata score", color=styleEqual(c('Low', 'Medium','High'), c(GREEN, ORANGE,RED)))
#   RR<-formatStyle(RR, columns = "model score (B)", color=styleEqual(LEVELS, COLO))
#   RR<-formatStyle(RR, columns = "model score (A)", color=styleEqual(LEVELS, COLO))
#   RR<-formatStyle(RR, columns = "combined score (A)", color=styleEqual(c('Low', 'Medium','High'), c(GREEN, ORANGE,RED)))
#   RR<-formatStyle(RR, columns = "combined score (B)", color=styleEqual(c('Low', 'Medium','High'), c(GREEN, ORANGE,RED)))
#   RR<-formatStyle(RR, c(7,8), backgroundColor = "#fff0ee")
#   RR<-formatStyle(RR, c(2,6), "border-right" = "solid 1px", "border-right-color"='black')
#   RR
# }

pie2<-function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
                init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
                col = NULL, border = NULL, lty = NULL, main = NULL, resize=1,...) 
{
  if (!is.numeric(x) || any(is.na(x) | x < 0)) 
    stop("'x' values must be positive.")
  if (is.null(labels)) 
    labels <- as.character(seq_along(x))
  else labels <- as.graphicsAnnot(labels)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  xlim<-xlim*resize
  ylim<-ylim*resize
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  if (is.null(col)) 
    col <- if (is.null(density)) 
      c("white", "lightblue", "mistyrose", "lightcyan", 
        "lavender", "cornsilk")
  else par("fg")
  if (!is.null(col)) 
    col <- rep_len(col, nx)
  if (!is.null(border)) 
    border <- rep_len(border, nx)
  if (!is.null(lty)) 
    lty <- rep_len(lty, nx)
  angle <- rep(angle, nx)
  if (!is.null(density)) 
    density <- rep_len(density, nx)
  twopi <- if (clockwise) 
    -2 * pi
  else 2 * pi
  t2xy <- function(t) {
    t2p <- twopi * t + init.angle * pi/180
    list(x = radius * cos(t2p), y = radius * sin(t2p))
  }
  for (i in 1L:nx) {
    n <- max(2, floor(edges * dx[i]))
    P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
    polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
    P <- t2xy(mean(x[i + 0:1]))
    lab <- as.character(labels[i])
    if (!is.na(lab) && nzchar(lab)) {
      #lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
      text(1.1 * P$x, 1.1 * P$y, eval(substitute(expression(bold(d)),list(d=paste(labels[i])))), xpd = TRUE, 
           adj = ifelse(P$x < 0, 1, 0), cex=1.8, ...)
    }
  }
  title(main = main, ...)
  invisible(NULL)
}

mypie<-function(x1,y1,z1, 
                x2,y2,z2,resize=1){
  options(bitmapType="cairo")
  Z1<-x1+y1+z1
  Z2<-x2+y2+z2
  piecol<-c('#FFAACC','#AACCFF','#90DD90')
  labels1<-paste(format(round(100*c(x1/Z1,y1/Z1,z1/Z1),1),2),'%')
  labels2<-paste(format(round(100*c(x2/Z2,y2/Z2,z2/Z2),1),2),'%')
  #bg<-par('bg')
  #par(bg=adjustcolor('white',0.2))
  par(mar=c(0,3,0,3),oma=c(0,0,0,0),mfrow=c(1,3))
  pie2(c(x1,y1,z1),border=0,col=piecol,labels = labels1,edges=500,resize = resize)
  text(0,0,expression(bold(A)),cex=2.5)
  pie2(c(x2,y2,z2),border=0,col=piecol,labels = labels2,edges=500,resize = resize)
  text(0,0,expression(bold(B)),cex=2.5)
  plot(1:2,1:2,type='n',axes=FALSE)
  legend('left',legend=c('IMEM','Metadata','Model'),bty='o', 
         bg=adjustcolor('white',0.4),fill = piecol,box.lwd=0,cex=2)
  #par(bg=bg)
}

mypie2<-function(x1,y1,z1,w1,resize=1){
  options(bitmapType="cairo")
  Z1<-x1+y1+z1+w1
  piecol<-c('#FFAACC','#AACCFF','#EE9940','#90DD90')
  labels1<-paste(format(round(100*c(x1/Z1,y1/Z1,z1/Z1,w1/Z1),1),2),'%')
  #bg<-par('bg')
  #par(bg=adjustcolor('white',0.2))
  par(mar=c(0,3,0,3),oma=c(0,0,0,0))
  layout(matrix(c(1,2,2),1,3),1,3)
  pie2(c(x1,y1,z1,w1),border=0,col=piecol,labels = labels1,edges=500,resize = resize)
  plot(1:2,1:2,type='n',axes=FALSE)
  legend('left',legend=c('Obligation of registration','Obligation of registration third country nationals',
                         'Monitoring third country nationals', 'Administrative corrections'),bty='o', 
         bg=adjustcolor('white',0.4),fill = piecol,box.lwd=0,cex=1.8)
  #par(bg=bg)
}
mypie2(0.1,0.2,0.52,0.2,0.85)
# d=1:3
# mypie(0.1,0.2,0.52,0.2,0.2,0.52,0.85)
# text(1.5,1,eval(substitute(expression(bold(d)),list(d='5'))),cex=2)
# text(1.5,1,expression(bold("5")),cex=2)

thr1 <- 0.25
thr2 <- 0.5

wimema <- 0.25
wimemb <- 0.3
wmetaa <- 0.15
wmetab <- 0.15
wmodela <- 1
wmodelb <- 1

100*wimema/(wimema+wmetaa+wmodela) # 15%
100*wmetaa/(wimema+wmetaa+wmodela) # 10%
100*wmodela/(wimema+wmetaa+wmodela) # 75%

100*wimemb/(wimemb+wmetab+wmodelb) # 20%
100*wmetab/(wimema+wmetab+wmodelb) # 10%
100*wmodelb/(wimema+wmetab+wmodelb) # 70%

wimema <- 0.20
wimemb <- 0.25
wmetaa <- 0.10
wmetab <- 0.10
wmodela <- 1 - wimema - wmetaa
wmodelb <- 1 - wimemb - wmetab


mirror <- TRUE

RefCntrSel <- 3

Step <-0.005

MWt1 <- 1
MWt2 <- 0.5
MWt3 <- 0.5
MWt4 <- 0.5

MWt1/(MWt1+MWt2+MWt3+MWt4)
MWt2/(MWt1+MWt2+MWt3+MWt4)
MWt3/(MWt1+MWt2+MWt3+MWt4)
MWt4/(MWt1+MWt2+MWt3+MWt4)

MWt1 <- 0.4
MWt2 <- 0.2
MWt3 <- 0.2
MWt4 <- 0.2


MThr1 <- 0.3
MThr2 <- 0.6
TrustNordic<-TRUE
Meta_DeReg<-Recalc_Meta_DeReg(Meta_DeReg,MWt1,MWt2,MWt3,MWt4,MThr1,MThr2,TrustNordic)
Meta_Reg<-Recalc_Meta_Reg(Meta_Reg, TrustNordic)

COLO=c("#00DD00",'#008000','#FFA500','#FF0000','#800000')

WeightsNam<-paste(c('Obligation of de-registration','Obligation of de-registration of third country nationals','Monitoring third country nationals','Administrative corrections'),sep='')
CountriesL<-paste(countrycode::countrycode(toeurostat(Countries),'eurostat','country.name'),' (',Countries,')',sep='')
Countries<-as.list(Countries)
names(Countries)<-CountriesL
PanelNames<-c('About','Immigration metadata','Emigration metadata','Immigration model','Emigration model',
              'X','X','Combined immigration scores','Combined emigration scores','X','X', 'Help')

IMEMc<-function(k) c('The parameter adds a weight to IMEM (<a href="https://www.imem.cpc.ac.uk/About.aspx">Integrated Modeling of European Migration</a>) undercount classification (Raymer et al. 2013) converted to numerical value (<b>IMEM score num</b>), where
         0 denotes <span style="color:#008000">Low</span> undercounting and 1 denotes <span style="color:#FF0000">High</span> undercounting.','',
                     paste('Weighted <b>IMEM score num</b> is used to calculate <b>combined score num (',k,')</b>',sep=''),'',
                     '<b>References</b>','<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F. and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>')

METAwtxt<-'Weight for the metadata <b>score num</b> obtained in <b>Immigration metadata</b> panel.'
MODELwtxt<-'Weight for the model <b>score num</b> obtained in <b>Model classify???</b> page used to calculate <b>combined score num (A)</b>'

version<-'0.7.1'
DOI<-'10.5281/zenodo.5594133'
BADGE<-'<a href="https://zenodo.org/badge/latestdoi/414693180"><img src="https://zenodo.org/badge/414693180.svg" alt="DOI"></a>'
shinyServer <-  function(input, output, session) {
  
  observe_helpers(withMathJax = TRUE, help_dir = 'helpfiles')
  
  ##################################################### E=1
  
  
  output$I3WBPlot <- renderPlot(mypie(input$I3wimemb,input$I3wmetab,input$I3wmodelb,input$I3wimema,input$I3wmetaa,input$I3wmodela))
  
  output$E3WBPlot <- renderPlot(mypie(input$E3wimemb,input$E3wmetab,input$E3wmodelb,input$E3wimema,input$E3wmetaa,input$E3wmodela))
  
  output$EMPlot <- renderPlot(mypie2(input$Emimetaw1,input$Emimetaw2,input$Emimetaw3,input$Emimetaw4))
  
  observeEvent(input$EMrecalc, {
    SA <- input$Emimetaw1 + input$Emimetaw2 + input$Emimetaw3 + input$Emimetaw4
    updateSliderInput(session = session, inputId = "Emimetaw1", value = input$Emimetaw1/SA)
    updateSliderInput(session = session, inputId = "Emimetaw2", value = input$Emimetaw2/SA)
    updateSliderInput(session = session, inputId = "Emimetaw3", value = input$Emimetaw3/SA)
    updateSliderInput(session = session, inputId = "Emimetaw4", value = input$Emimetaw4/SA)
  })
  
  observeEvent(input$I3recalca, {
    SA <- input$I3wimema + input$I3wmetaa + input$I3wmodela
    updateSliderInput(session = session, inputId = "I3wimema", value = input$I3wimema/SA)
    updateSliderInput(session = session, inputId = "I3wmetaa", value = input$I3wmetaa/SA)
    updateSliderInput(session = session, inputId = "I3wmodela", value = input$I3wmodela/SA)
  })
  
  observeEvent(input$E3recalcb, {
    SB <- input$E3wimemb + input$E3wmetab + input$E3wmodelb
    updateSliderInput(session = session, inputId = "E3wimemb", value = input$E3wimemb/SB)
    updateSliderInput(session = session, inputId = "E3wmetab", value = input$E3wmetab/SB)
    updateSliderInput(session = session, inputId = "E3wmodelb", value = input$E3wmodelb/SB)
  })
  
  observeEvent(input$I3recalcb, {
    SB <- input$I3wimemb + input$I3wmetab + input$I3wmodelb
    updateSliderInput(session = session, inputId = "I3wimemb", value = input$I3wimemb/SB)
    updateSliderInput(session = session, inputId = "I3wmetab", value = input$I3wmetab/SB)
    updateSliderInput(session = session, inputId = "I3wmodelb", value = input$I3wmodelb/SB)
  })
  
  observeEvent(input$E3recalca, {
    SA <- input$E3wimema + input$E3wmetaa + input$E3wmodela
    updateSliderInput(session = session, inputId = "E3wimemb", value = input$E3wimemb/SB)
    updateSliderInput(session = session, inputId = "E3wmetab", value = input$E3wmetab/SB)
    updateSliderInput(session = session, inputId = "E3wmodelb", value = input$E3wmodelb/SB)
  })
  
  
  ##################################################### Clonning
  
  observeEvent(input$E3clonea,{
    updateSliderInput(session = session, inputId = "E3wimema", value = input$I3wimema)
    updateSliderInput(session = session, inputId = "E3wmetaa", value = input$I3wmetaa)
    updateSliderInput(session = session, inputId = "E3wmodela", value = input$I3wmodela)
  })
  
  observeEvent(input$E3cloneb,{
    updateSliderInput(session = session, inputId = "E3wimemb", value = input$I3wimemb)
    updateSliderInput(session = session, inputId = "E3wmetab", value = input$I3wmetab)
    updateSliderInput(session = session, inputId = "E3wmodelb", value = input$I3wmodelb)
  })
  
  observeEvent(input$I3cloneb,{
    updateSliderInput(session = session, inputId = "I3wimemb", value = input$E3wimemb)
    updateSliderInput(session = session, inputId = "I3wmetab", value = input$E3wmetab)
    updateSliderInput(session = session, inputId = "I3wmodelb", value = input$E3wmodelb)
  })
  
  observeEvent(input$I3clonea,{
    updateSliderInput(session = session, inputId = "I3wimema", value = input$E3wimema)
    updateSliderInput(session = session, inputId = "I3wmetaa", value = input$E3wmetaa)
    updateSliderInput(session = session, inputId = "I3wmodela", value = input$E3wmodela)
  })
  
  
  ###############################
  
  
  observeEvent(input$Emimetat2,  {
    updateSliderInput(session = session, inputId = "Emimetat1", max = input$Emimetat2)
  })
  
  observeEvent(input$Emimetat1,  {
    updateSliderInput(session = session, inputId = "Emimetat2", min = input$Emimetat1)
  })
  
  observeEvent(input$EMweightsreset, {
    updateSliderInput(session = session, inputId = "Emimetaw1", value = MWt1)
    updateSliderInput(session = session, inputId = "Emimetaw2", value = MWt2)
    updateSliderInput(session = session, inputId = "Emimetaw3", value = MWt3)
    updateSliderInput(session = session, inputId = "Emimetaw4", value = MWt4)
  })
  
  observeEvent(input$EMthreshreset, {
    updateSliderInput(session = session, inputId = "Emimetat1", value = MThr1)
    updateSliderInput(session = session, inputId = "Emimetat2", value = MThr2)
  })
  
  #####################
  
  observeEvent(input$Eall,{
    updateCheckboxGroupInput(session = session, inputId = "Ecountry", selected = CountriesS )
  })
  
  observeEvent(input$Enone,{
    updateCheckboxGroupInput(session = session, inputId = "Ecountry", selected = '' )
  })
  
  #####################
  
  observeEvent(input$Iall,{
    updateCheckboxGroupInput(session = session, inputId = "Icountry", selected = CountriesS )
  })
  
  observeEvent(input$Inone,{
    updateCheckboxGroupInput(session = session, inputId = "Icountry", selected = '' )
  })
  
  
  ######################
  
  ImmiMetaScores<-reactive({
    Recalc_Meta_Reg(Meta_Reg, input$nordicimmi)
  })
  
  EmiMetaScores<-reactive({
    Recalc_Meta_DeReg(Meta_DeReg, input$Emimetaw1, input$Emimetaw2, input$Emimetaw3, input$Emimetaw4,
                      input$Emimetat1, input$Emimetat2, input$nordicemi)
  })
  
  output$table1<-renderDT({
    ImmiMetaScores()
  })
  
  output$table2<-renderDT({
    EmiMetaScores()
  })
  
  output$downloadIMData<- downloadHandler(
    filename = function() {
      paste('Immi_Meta_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(ImmiMetaScores()), file,row.names = FALSE)
    }
  )
  
  output$downloadEMData<- downloadHandler(
    filename = function() {
      paste('Emi_Meta_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(EmiMetaScores()), file,row.names = FALSE)
    }
  )
  
  ###################
  
  observeEvent(input$Iraymer,{
    if(as.numeric(input$Iraymer)<5) {
      updateSelectInput(session = session, inputId = "Irefcountry", selected = 9) 
    } else {
      updateSelectInput(session = session, inputId = "Irefcountry", selected = as.numeric(input$Iraymer)-4) 
    }
  })
  
  observeEvent(input$Eraymer,{
    if(as.numeric(input$Eraymer)<5) {
      updateSelectInput(session = session, inputId = "Erefcountry", selected = 9) 
    } else {
      updateSelectInput(session = session, inputId = "Erefcountry", selected = as.numeric(input$Eraymer)-4) 
    }
  })
  
  output$Isaveplot<- downloadHandler(
    filename = function() {
      paste('Immi_Undercounting_Ratio.', input$Iformat, sep='') },
    content = function(file) {
      ffo <- input$Iformat
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }
      plot_ui_result('I',
                     country=input$Icountry,
                     refcountry=input$Irefcountry,
                     stats=2,
                     extrapol=input$Iimputations,
                     raymer=input$Iraymer,
                     logscale=input$Ilogscale,
                     plotCI=input$IplotCI,
                     additive = input$Iadditive,
                     separated = input$Iseparated,
                     ncp=input$Incp)
      dev.off()
    }
  )
  
  output$Esaveplot<- downloadHandler(
    filename = function() {
      paste('Emi_Undercounting_Ratio.', input$Eformat, sep='') },
    content = function(file) {
      ffo <- input$Eformat
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }
      plot_ui_result('E',
                     country=input$Ecountry,
                     refcountry=input$Erefcountry,
                     stats=2,
                     extrapol=input$Eimputations,
                     raymer=input$Eraymer,
                     logscale=input$Elogscale,
                     plotCI=input$EplotCI,
                     additive = input$Eadditive,
                     separated = input$Eseparated,
                     ncp=input$Encp)
      
      dev.off()
    }
  )
  
  
  output$ImiPlot <- renderPlot({
    plot_ui_result('I',
                   country=input$Icountry,
                   refcountry=input$Irefcountry,
                   stats=2,
                   extrapol=input$Iimputations,
                   raymer=input$Iraymer,
                   logscale=input$Ilogscale,
                   plotCI=input$IplotCI,
                   additive = input$Iadditive,
                   separated = input$Iseparated,
                   ncp=input$Incp
    )
  })
  
  output$EmiPlot <- renderPlot({
    plot_ui_result('E',
                   country=input$Ecountry,
                   refcountry=input$Erefcountry,
                   stats=2,
                   extrapol=input$Eimputations,
                   raymer=input$Eraymer,
                   logscale=input$Elogscale,
                   plotCI=input$EplotCI,
                   additive = input$Eadditive,
                   separated = input$Eseparated,
                   ncp=input$Encp
    )
  })
  
  
  CORRITAB<-reactive({
    get_correction('I', input$Iraymer, input$Iadditive, input$Iseparated)
  })
  
  CORRETAB<-reactive({
    get_correction('E', input$Eraymer, input$Eadditive, input$Eseparated)
  })
  
  output$corrItab <- renderTable({
    CORRITAB()}, 
    bordered = TRUE,  
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )  
  
  output$corrEtab <- renderTable({
    CORRETAB()}, 
    bordered = TRUE,  
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )  
  
  RESI<-reactive({
    CalcModel(direction='I',
              w1=input$Imimetaw1, w2=input$Imimetaw2,
              w3=input$Imimetaw3, w4=input$Imimetaw4,
              t1=input$Imimetat1, t2=input$Imimetat2,
              ItrustNordic = input$nordicemi,
              EtrustNordic = input$nordicimmi,
              
              additive = input$Iadditive,
              separated = input$Iseparated,
              ncp = input$Incp,
              refcountries = input$Irefcountry,
              durationCorrection = input$Iraymer,
              IgnoreOverCounting = input$IIgnoreOverCounting,
              TranslateGroups = input$ITranslateGroups,
              threshyear = input$IYear,
              FinalGroups = input$IFinalGroups,
              useimputation = input$Iimputations,
              #thr1=input$I3t1, thr2=input$I3t2,
              w_imemA = input$I3wimema,
              w_imemB = input$I3wimemb,
              w_metaA = input$I3wmetaa,
              w_metaB = input$I3wmetab,
              w_modelA = input$I3wmodela,
              w_modelB = input$I3wmodelb)#, mirror=input$I3mirror,"I")
  })
  
  RESE<-reactive({
    CalcModel(direction='E',
              w1=input$Emimetaw1, w2=input$Emimetaw2,
              w3=input$Emimetaw3, w4=input$Emimetaw4,
              t1=input$Emimetat1, t2=input$Emimetat2,
              ItrustNordic = input$nordicemi,
              EtrustNordic = input$nordicimmi,
              
              additive = input$Eadditive,
              separated = input$Eseparated,
              ncp = input$Encp,
              refcountries = input$Erefcountry,
              durationCorrection = input$Eraymer,
              IgnoreOverCounting = input$EIgnoreOverCounting,
              TranslateGroups = input$ETranslateGroups,
              threshyear = input$EYear,
              FinalGroups = input$EFinalGroups,
              useimputation = input$Eimputations,
              
              w_imemA = input$E3wimema,
              w_imemB = input$E3wimemb,
              w_metaA = input$E3wmetaa,
              w_metaB = input$E3wmetab,
              w_modelA = input$E3wmodela,
              w_modelB = input$E3wmodelb)
  })
  
  
  output$Ilogratios <- renderTable({
    getModel(RESI())$logindex}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )  
  
  output$Elogratios <- renderTable({
    getModel(RESE())$logindex}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )  
  
  
  output$IlogratiosThresholds <- renderTable({
    getModel(RESI())$logindexthresholds}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )
  
  output$ElogratiosThresholds <- renderTable({
    getModel(RESE())$logindexthresholds}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )
  
  output$IscoresThresholds <- renderTable({
    getCombined(RESI())$rawthresholds}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )
  
  output$EscoresThresholds <- renderTable({
    getCombined(RESE())$rawthresholds}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )
  
  output$Iscores <- renderTable({
    getCombined(RESI())$raw}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )
  
  output$Escores <- renderTable({
    getCombined(RESE())$raw}, 
    bordered = TRUE,  
    rownames = TRUE,
    spacing = 'xs',  
    width = '100%', 
    align = 'c'
  )
  
  output$ImiPlotB <- renderPlot({
    plotModel(RESI(), shownodat=input$INoData)
  })  
  
  output$EmiPlotB <- renderPlot({
    plotModel(RESE(), shownodat=input$ENoData)
  })  
  
  output$IsaveplotB<- downloadHandler(
    filename = function() {
      paste('Immi_Model_Scores.', input$IformatB, sep='') },
    content = function(file) {
      ffo <- input$IformatB
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }
      plotModel(RESI(), shownodat=input$INoData)
      dev.off()
    }
  )
  
  output$EsaveplotB<- downloadHandler(
    filename = function() {
      paste('Emi_Model_Scores.', input$EformatB, sep='') },
    content = function(file) {
      ffo <- input$EformatB
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }
      plotModel(RESE(), shownodat=input$ENoData)
      dev.off()
    }
  )
  
  
  output$IsavedataB<- downloadHandler(
    filename = function() {
      paste('Immi_Model_Scores', '.xlsx', sep='') },
    content = function(filenamer) {
      #print(filenamer)
      saveModel(filenamer, RESI() )
    }
  )
  
  output$EsavedataB<- downloadHandler(
    filename = function() {
      paste('Emi_Model_Scores', '.xlsx', sep='') },
    content = function(filenamer) {
      #print(filenamer)
      saveModel(filenamer, RESE() )
    }
  )
  
  
  #############################
  
  output$ImiPlot2 <- renderPlot({
    plotCombined(RESI(), shownodat=input$INoData2)
  })  
  
  output$EmiPlot2 <- renderPlot({
    plotCombined(RESE(), shownodat=input$ENoData2)
  })  
  
  output$Isaveplot2<- downloadHandler(
    filename = function() {
      paste('Immi_Combined_Scores.', input$Iformat2, sep='') },
    content = function(file) {
      ffo <- input$Iformat2
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }
      plotCombined(RESI(), shownodat=input$INoData2)
      dev.off()
    }
  )
  
  output$Esaveplot2<- downloadHandler(
    filename = function() {
      paste('Emi_Combined_Scores.', input$Eformat2, sep='') },
    content = function(file) {
      ffo <- input$Eformat2
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }
      plotCombined(RESE(), shownodat=input$ENoData2)
      dev.off()
    }
  )
  
  output$Isavedata2<- downloadHandler(
    filename = function() {
      paste('Immi_Combined_Scores', '.xlsx', sep='') },
    content = function(filenamer) {
      #print(filenamer)
      saveCombined(filenamer, RESI() )
    }
  )
  
  output$Esavedata2<- downloadHandler(
    filename = function() {
      paste('Emi_Combined_Scores', '.xlsx', sep='') },
    content = function(filenamer) {
      #print(filenamer)
      saveCombined(filenamer, RESE() )
    }
  )
  
  
  output$I2yearshowA <- renderUI({
    yrange <- paste('Mixing weights before ',input$IYear,' (<b>A</b>)',sep='')
    h4(HTML(yrange))
  })
  
  output$E2yearshowA <- renderUI({
    yrange <- paste('Mixing weights before ',input$EYear,' (<b>A</b>)',sep='')
    h4(HTML(yrange))
  })
  
  
  output$I2yearshowB <- renderUI({
    yrange <- paste('Mixing weights from ',input$IYear,' on (<b>B</b>)',sep='')
    h4(HTML(yrange))
  })
  
  output$E2yearshowB <- renderUI({
    yrange <- paste('Mixing weights from ',input$EYear,' on (<b>B</b>)',sep='')
    h4(HTML(yrange))
  })
  
  output$I2yearshowC <- renderUI({
    yrange <- paste('(<b>A</b>) - before ',input$IYear,', (<b>B</b>) - from ',input$IYear,' on',sep='')
    h4(HTML(yrange))
  })
  
  output$E2yearshowC <- renderUI({
    yrange <- paste('(<b>A</b>) - before ',input$EYear,', (<b>B</b>) - from ',input$EYear,' on',sep='')
    h4(HTML(yrange))
  })
  
  output$downloadBIB<- downloadHandler(
    filename = function() {
      paste('UndercountMigScores', '.bib', sep='') },
    content = function(filename) {
      con <- file('CITATION.bib', encoding = "UTF-8")
      con2 <- file(filename, encoding = "UTF-8")
      z<-readLines(con)
      writeLines(z, con2)
      close(con)
      close(con2)
    }
  )
  
}

colabout="#A9DFBF"
colimmi="#AED6F1"
colemi="#FAD7A0"
coltxt='black'
colsel='#873600'

shinyUI <- fluidPage(
  titlePanel(HTML('<span style="color:#000070;font-family:Serif,Georgia,Serif"><b>UndercountMigScores</b></span>'),'UndercountMigScores'),
  fluidRow(
    column(width = 9,
           tags$head(tags$style("h3 {margin-top:0px;}", media="screen", type="text/css")),
           tags$head(tags$style("h4 {margin-top:0px;}", media="screen", type="text/css")),
           tags$head(tags$style("img {border:1px; border-color: #D5D5D5; border-style: solid;}", media="screen", type="text/css")),
           tags$head(tags$style(".well {border:2px; border-color: #D5D5D5; border-style: solid; padding-bottom: 5px; background-color: #F5F5F5;}", media="screen", type="text/css")),
           
           tags$style(HTML(paste("
                          .tabbable > .nav > li > a {background-color: aqua;  color:black; border-width: medium}
                          .tabbable > .nav > li > a[data-value='",PanelNames[1],"'] {background-color: ",colabout,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[2],"'] {background-color: ",colimmi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[3],"'] {background-color: ",colemi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[4],"'] {background-color: ",colimmi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[5],"'] {background-color: ",colemi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[6],"'] {background-color: ",colimmi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[7],"'] {background-color: ",colemi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[8],"'] {background-color: ",colimmi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[9],"'] {background-color: ",colemi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[10],"'] {background-color: ",colimmi,"; color:",coltxt,"}
                          .tabbable > .nav > li > a[data-value='",PanelNames[11],"'] {background-color: ",colemi,"; color:",coltxt,"}
                          .tabbable > .nav > li[class=active] > a {border-width: medium; color:",colsel,
                                 ";border-color:#775544; text-shadow: 0.7px 0.7px ",colsel,"}",sep=''))),
           br(),
           tabsetPanel(type='tabs',
                       tabPanel(title = PanelNames[1], 
                                column(12,offset=0, align="center",
                                       br(),
                                       h3(HTML(BADGE)),
                                       h3(HTML(paste0('<b>UndercountMigScores v',version,'</b>'))),
                                       h4(HTML('<a href="https://maciej-jan-danko.shinyapps.io/undercountmigscores/"> https://maciej-jan-danko.shinyapps.io/undercountmigscores/ </a>')),
                                       br(),
                                       h4('Assessing the Level of Undercounting in the InternationalMigration Flows Reported by Eurostat'),
                                       br(),
                                       h4('Maciej J. Dańko'),
                                       h4(HTML('email: <a href="mailto:name@email.com"> danko@demogr.mpg.de </a>')),
                                       br(),
                                       h4('Max Planck Institute for Demographic Research'),
                                       h4('Rostock, Germany'),
                                       h4('2021-2022'),
                                       
                                       h5('____________________________________________________________________________'),
                                       h4('How to cite this software?'),
                                       h5(HTML(paste0('Maciej J. Dańko. UndercountMigScores ',version,'. (2021)<br>
                                               Assessing the Level of Undercounting in the InternationalMigration Flows Reported by Eurostat.
                                               <br>DOI: ',DOI,'. URL:https://github.com/MaciejDanko/UndercountMigScore'))),
                                       downloadButton("downloadBIB", "Download citation in .bib format"),
                                       h5('____________________________________________________________________________'),
                                       h5(HTML('The newest version of the app is always available on GitHub. To run it use this R code:<br><span style="font-family: Courier New">shiny::runGitHub("MaciejDanko/UndercountMigScores", launch.browser = TRUE)</span><br>')),
                                       h5(HTML('You may need to update/install some dependencies:<br><span style="font-family: Courier New">install.packages("usethis", "shiny", "Cairo", "openxlsx", "countrycodes", "data.table", <br> "DT", "magicaxis", "shinyWidgets", "RColorBrewer", "shinyhelper")</span><br>')),
                                       h5(HTML('If equations do not display correctly you may need to re-install mathjax on your computer<br>
                                       Linux: <span style="font-family: Courier New">sudo apt-get install -y libjs-mathjax</span>,<br>Windows/Mac/Linux: <a href="https://sourceforge.net/projects/mathjax/"> https://sourceforge.net/projects/mathjax/</a>')),
                                       br(),br(),br(),br()
                                )
                       ),
                       tabPanel(title = PanelNames[2],
                                br(), br(),
                                sidebarPanel(fluid=FALSE,
                                             
                                             h3("Missing metadata options"),
                                             helper(checkboxInput("nordicimmi", "Trust Nordic countries", value = TrustNordic),
                                                    colour='#FF0000',type='inline',title='Score calculation procedure',buttonLabel = 'Close',
                                                    content=c('No obligation of registration = <span style="color:#FF0000">High</span> undercounting, obligation of registration = <span style="color:#008000">Low</span> undercounting,
                                                                                                           but if <b>No limit</b> or <b>No sanctions</b> occur the score is changed to <span style="color:#FFA500">Medium</span>.',
                                                              '','The <span style="font-style:italic">Trust Nordic countries</span> option set <span style="color:#008000">Low</span> score for all Nordic countries ignoring the metadata.','',
                                                              'Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).')),
                                ),
                                sidebarPanel(width=8,
                                             div(id='ZZD',
                                                 h3(HTML('Immigration undercounting related metadata, expert opinions (IMEM), and their classification.'))
                                             ),
                                             tags$head(tags$style("#ZZD h3 {margin-bottom: 19px;}", media="screen", type="text/css")),
                                ),
                                sidebarPanel(width=12,
                                             downloadButton("downloadIMData", "Download table"),
                                             br(),
                                             DTOutput('table1'),
                                             tags$head(tags$style("#table1 table th {background-color: #CCBBFF; border-width:1px;}", media="screen", type="text/css")),
                                             
                                             br(), br()
                                ),
                                sidebarPanel(width=12,
                                             h3("References"),
                                             tags$body('Tab4a (page 20) of'),
                                             tags$a(href="http://quantmig.eu/res/files/QuantMig_Deliverable%206.2%20vf.pdf#page=21", "Jarl Mooyaart, Maciej J. Dańko, Rafael Costa, and Michaël Boissonneault (2021) Quality assessment of European migration data. Deliverable 6.2"),
                                             br(),br(),
                                             tags$a(href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20","Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F., and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819."),
                                             br(),br()
                                ),
                       ),
                       tabPanel(title = PanelNames[3],
                                br(), br(),
                                sidebarPanel(fluid=FALSE,width=6,
                                             helper(h3('Metadata weights'),
                                                    colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                                    content=c('The <b>score num</b> is calculated as a weighted mean which excludes all variables with "Unknown" records.')),
                                             sliderInput(inputId = "Emimetaw1", label = WeightsNam[1], min = 0, max = 1, value = MWt1, step=Step),
                                             sliderInput(inputId = "Emimetaw2", label = WeightsNam[2], min = 0, max = 1, value = MWt2, step=Step),
                                             sliderInput(inputId = "Emimetaw3", label = WeightsNam[3], min = 0, max = 1, value = MWt3, step=Step),
                                             sliderInput(inputId = "Emimetaw4", label = WeightsNam[4], min = 0, max = 1, value = MWt4, step=Step),
                                             helper(tags$span(' '),
                                                    colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                                    content='<b>Reset</b> - restores default values, <b>&#8721 weights = 1</b> makes all weights to sum to 1.'),
                                             
                                             actionButton("EMweightsreset", "Reset"),
                                             actionButton("EMrecalc", HTML("&#8721 weights = 1")),                                  
                                             tags$hr(style="border-color: black;"),
                                             
                                             h3("Missing metadata options"),
                                             div(id='ZZ2',helper(checkboxInput("nordicemi", 'Trust Nordic countries', value = TrustNordic),
                                                                 colour='#FF0000',type='inline',title='Trust Nordic countries',buttonLabel = 'Close',
                                                                 content=c('The <span style="font-style:italic">Trust Nordic countries</span> option set <span style="color:#008000">Low</span> score for all Nordic countries ignoring the metadata.',
                                                                           '','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).'))),
                                             tags$head(tags$style("#ZZ2 .checkbox {margin-bottom: 15px;}", media="screen", type="text/css")),
                                             
                                ),
                                sidebarPanel(width=6,
                                             h3('Normalized weights'),
                                             plotOutput('EMPlot', height='200', width='100%'),
                                             br()
                                ),
                                sidebarPanel(width=6,
                                             h3('Metadata classification thresholds'),
                                             #tags$hr(style="border-color: black;"),
                                             uiOutput(outputId = "dynamicT1"),
                                             sliderInput(inputId = "Emimetat1", label = "Low | Medium", min = 0, max = 1, value = MThr1, step=Step),
                                             sliderInput(inputId = "Emimetat2", label = "Medium | High", min = 0, max = 1, value = MThr2, step=Step),
                                             actionButton("EMthreshreset", "Reset"),
                                             tags$head(tags$style("#EMthreshreset {margin-bottom: 9px;}", media="screen", type="text/css")),
                                ),
                                sidebarPanel(width=12,
                                             h3(HTML('Emigration undercounting related metadata, expert opinions (IMEM), and their classification.')),
                                             
                                             downloadButton("downloadEMData", "Download table"),
                                             br(),
                                             DTOutput('table2'),
                                             tags$head(tags$style("#table2 table th {background-color: #CCBBFF; border-width:1px;}", media="screen", type="text/css")),
                                             
                                             br(), br()
                                ),
                                sidebarPanel(width=12,
                                             h3("References"),
                                             tags$body('Table 4b of'),
                                             tags$a(href="http://quantmig.eu/res/files/QuantMig_Deliverable%206.2%20vf.pdf#page=22", "Jarl Mooyaart, Maciej J. Dańko, Rafael Costa, and Michaël Boissonneault (2021) Quality assessment of European migration data. Deliverable 6.2"),
                                             br(), br(),
                                             tags$body('Table 1.6 of'),
                                             tags$a(href="https://ec.europa.eu/eurostat/ramon/statmanuals/files/KS-CC-03-005-EN.pdf#page=22", "Eurostat (2003) Demographic statistics: Definitions and methods of collection in 31 European Countries. ISSN 1725-065X
                                  ISBN 92-894-6051-2."),
                                             br(),br(),
                                             tags$a(href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20","Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F., and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819."),
                                             br(),br()
                                )
                       ),
                       tabPanel(title = PanelNames[4],
                                br(),br(),
                                sidebarPanel(
                                  helper(h3("General model options"),colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                         content='BilateralModel',size='l'),
                                  
                                  tags$hr(style="border-color: black;"),
                                  
                                  helper(h4("Duration of stay correction"),
                                         colour='#FF0000',type='markdown',title="",buttonLabel = 'Close',
                                         content = c('DurationCorrection')),
                                  
                                  selectInput("Iraymer", label = NULL,
                                              choices = list(
                                                'Uncorrected' = 0,
                                                'IMEM model (Raymer 2013)'=1,
                                                'Eexpert judgment (Willekens 2019)'=2,
                                                'Poisson model (Willekens 2019)'=3,
                                                'Mixture model (Willekens 2019)'=4,
                                                'Optimization: Nordic Coutries (Without IS)'=5,
                                                'Optimization: Nordic Coutries'=6,
                                                'Optimization: Nordic Coutries +BE'=7,
                                                'Optimization: Nordic Coutries +CH'=8,
                                                'Optimization: Nordic Coutries +NL'=9,
                                                'Optimization: Nordic Coutries +BE+CH'=10,
                                                'Optimization: Nordic Coutries +BE+NL'=11,
                                                'Optimization: Nordic Coutries +CH+NL'=12,
                                                'Optimization: Nordic Coutries +BE+CH+NL'=13,
                                                'Optimization: Nordic Coutries +AT+BE+CH+NL'=14,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+NL'=15,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+FR+NL'=16,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+FR+IE+NL'=17,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+FR+IE+NL+UK'=18,
                                                'Optimization: All countries'=19),
                                              selected = 1),
                                  
                                  tableOutput('corrItab'),
                                  tags$head(tags$style("#corrItab table {background-color: white; }", media="screen", type="text/css")),
                                  tags$head(tags$style("#corrItab table th {background-color: #CCBBFF; }", media="screen", type="text/css")),
                                  
                                  conditionalPanel(condition = "input.Iraymer > 4",
                                                   checkboxInput("Iseparated", "Use duration corrrection parameters calculated separately for immigration", value = FALSE),
                                                   checkboxInput("Iadditive", "Use additive optimization criteria (otherwise multiplicative)", value = TRUE),
                                                   
                                                   tags$hr(style="border-color: black;"),
                                                   helper(selectInput("Irefcountry", h4("Reference group of countries"),
                                                                      choices = list('Nordic countries (without IS)'=1,'Nordic countries'=2,'Nordic countries+BE'=3,'Nordic countries+CH'=4,'Nordic countries+NL'=5,
                                                                                     'Nordic countries+BE+CH'=6,'Nordic countries+BE+NL'=7,'Nordic countries+CH+NL'=8,
                                                                                     'Nordic countries+BE+CH+NL'=9,'Nordic countries+AT+BE+CH+NL'=10,
                                                                                     'Nordic countries+AT+BE+CH+DE+NL'=11,'Nordic countries+AT+BE+CH+DE+FR+NL'=12,
                                                                                     'Nordic countries+AT+BE+CH+DE+FR+IE+NL'=13,'Nordic countries+AT+BE+CH+DE+FR+IE+NL+UK'=14,'All countries'=15),
                                                                      selected = RefCntrSel),
                                                          colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                                          content=c('Please set the "Duration of stay correction" first before setting this parameter','','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "Overview" for more information about the bilateral flows ratio model.')),
                                                   
                                  ),
                                  tags$hr(style="border-color: black;"),
                                  h4('Imputations of missing values'),
                                  helper(checkboxInput("Iimputations", "Use PCA imputations", value = TRUE),
                                         colour='#FF0000',type='inline',title='PCA imputations',buttonLabel = 'Close',
                                         content=c('Impute the missing values of the flows ratios matrix with the Principal Components Analysis model using missMDA package.')),
                                  conditionalPanel(condition = "input.Iimputations == true",
                                                   helper(sliderInput(inputId = "Incp", label = 'ncp parameter', min = 1, max = 5, value = 1, step=1, sep=''),
                                                          colour='#FF0000',type='inline',title='ncp parameter',buttonLabel = 'Close',
                                                          content='From imputePCA {missMDA package}: "integer corresponding to the number of components used to to predict the missing entries".')),
                                ),
                                sidebarPanel(width=8,
                                             
                                             h3(HTML('Classification options')),
                                             tags$hr(style="border-color: black;"),
                                             column(width=4,br(),
                                                    checkboxInput("IIgnoreOverCounting", "Ignore over-counting", value = TRUE)),
                                             column(width=8,
                                                    div(id='Izupa',
                                                        sliderInput(inputId = "ITranslateGroups", label = 'Model sensitivity (number of classes)', min = 2, max = 10, value = 5, step=1, sep=''))
                                             ),
                                             span(HTML('&#160;'),style="font-size:1px; align: top;"),
                                             tags$head(tags$style("#Izupa .form-group.shiny-input-container {margin-bottom: 0px;}", media="screen", type="text/css")),
                                ),
                                sidebarPanel(width=8,
                                             style='background-color: #FFFFFF; border-color: #FFFFFF; padding: 0px; margin-bottom: -15px;',
                                             radioGroupButtons(
                                               inputId = "Ipanels",
                                               label = NULL,#"Select the result panel", 
                                               justified= TRUE,
                                               width='100%',
                                               individual=FALSE,
                                               #checkIcon = list(  yes = icon("check-square")),
                                               choiceNames = c("Inspect bilateral flows ratios",HTML("log<sub>10</sub> flows ratios"), "Classification of model undercounting"),
                                               choiceValues = 1:3,
                                               status = "danger"
                                             )),
                                tags$head(tags$style("#Ipanels .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Ipanels .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),
                                conditionalPanel(condition = "input.Ipanels == 1",
                                                 sidebarPanel(width=8,
                                                              helper(h3('Bilateral flows ratios for immigration data'),
                                                                     colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                                                     content='BilateralModel',size='l'),
                                                              tags$hr(style="border-color: black;"),
                                                              checkboxGroupInput("Icountry", h4("Countries selection"),
                                                                                 choices = Countries, selected = c('ES','BG','FI','SK','IT'), inline = TRUE),
                                                              actionButton("Iall", "All"),actionButton("Inone", "None"),
                                                              br(),
                                                              plotOutput(outputId = "ImiPlot", height="600px", width='100%'),
                                                              br(),
                                                              div(style="display:inline-block;vertical-align:top;",
                                                                  column(6,checkboxInput("Ilogscale", "Use log-scale", value = TRUE)),
                                                                  column(6,checkboxInput("IplotCI", "Plot confidence intervals", value = TRUE)),
                                                              ),
                                                              div(style="display:inline-block;vertical-align:top;",
                                                                  h5('Choose a format and save the plot'),
                                                                  column(6,selectInput("Iformat", NULL,
                                                                                       choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  column(6,downloadButton("Isaveplot", "Save plot")))
                                                 )),
                                conditionalPanel(condition = "input.Ipanels == 2",
                                                 sidebarPanel(width=8,
                                                              
                                                              h3(HTML('Estimated log<sub>10</sub> ratios of the bilateral flows')),
                                                              tableOutput('Ilogratios'),
                                                              tags$head(tags$style("#Ilogratios {border-width:4px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Ilogratios table {font-size: 8px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Ilogratios table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Ilogratios table td {padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Quantile-based thresholds used for classification.')),
                                                              tableOutput('IlogratiosThresholds'),
                                                              tags$head(tags$style("#IlogratiosThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#IlogratiosThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),
                                                 )),
                                conditionalPanel(condition = "input.Ipanels == 3",
                                                 sidebarPanel(width=8,
                                                              h3(HTML('Classification of the bilateral flow ratios.')),
                                                              plotOutput(outputId = "ImiPlotB", height="600px", width='100%'),
                                                              
                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),
                                                                         
                                                                         helper(checkboxInput("INoData", "Mark no data", value = TRUE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c('Tick the cases where the calculation of bilateral flows was impossible due to missing flows in the considered country or reference countries.')
                                                                         )
                                                                  ),
                                                                  column(3,h5(HTML('Image format')),selectInput("IformatB", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  
                                                                  column(3,h5(HTML('&#160;')),downloadButton("IsaveplotB", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("IsavedataB", "Save results as xlsx"))
                                                              ),
                                                 ),
                                ),
                                mainPanel(br(),br(),br(),br(),br())
                       ),
                       tabPanel(title = PanelNames[5],
                                br(),br(),
                                sidebarPanel(
                                  helper(h3("General model options"),colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                         content='BilateralModel',size='l'),
                                  
                                  tags$hr(style="border-color: black;"),
                                  
                                  helper(h4("Duration of stay correction"),
                                         colour='#FF0000',type='markdown',title="",buttonLabel = 'Close',
                                         content = c('DurationCorrection')),
                                  
                                  selectInput("Eraymer", label = NULL,
                                              choices = list(
                                                'Uncorrected' = 0,
                                                'IMEM model (Raymer 2013)'=1,
                                                'Eexpert judgment (Willekens 2019)'=2,
                                                'Poisson model (Willekens 2019)'=3,
                                                'Mixture model (Willekens 2019)'=4,
                                                'Optimization: Nordic Coutries (Without IS)'=5,
                                                'Optimization: Nordic Coutries'=6,
                                                'Optimization: Nordic Coutries +BE'=7,
                                                'Optimization: Nordic Coutries +CH'=8,
                                                'Optimization: Nordic Coutries +NL'=9,
                                                'Optimization: Nordic Coutries +BE+CH'=10,
                                                'Optimization: Nordic Coutries +BE+NL'=11,
                                                'Optimization: Nordic Coutries +CH+NL'=12,
                                                'Optimization: Nordic Coutries +BE+CH+NL'=13,
                                                'Optimization: Nordic Coutries +AT+BE+CH+NL'=14,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+NL'=15,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+FR+NL'=16,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+FR+IE+NL'=17,
                                                'Optimization: Nordic Coutries +AT+BE+CH+DE+FR+IE+NL+UK'=18,
                                                'Optimization: All countries'=19),
                                              selected = 1),
                                  
                                  tableOutput('corrEtab'),
                                  tags$head(tags$style("#corrEtab table {background-color: white; }", media="screen", type="text/css")),
                                  tags$head(tags$style("#corrEtab table th {background-color: #CCBBFF; }", media="screen", type="text/css")),
                                  
                                  conditionalPanel(condition = "input.Eraymer > 4",
                                                   checkboxInput("Eseparated", "Use duration corrrection parameters calculated separately for immigration", value = FALSE),
                                                   checkboxInput("Eadditive", "Use additive optimization criteria (otherwise multiplicative)", value = TRUE),
                                                   
                                                   tags$hr(style="border-color: black;"),
                                                   helper(selectInput("Erefcountry", h4("Reference group of countries"),
                                                                      choices = list('Nordic countries (without IS)'=1,'Nordic countries'=2,'Nordic countries+BE'=3,'Nordic countries+CH'=4,'Nordic countries+NL'=5,
                                                                                     'Nordic countries+BE+CH'=6,'Nordic countries+BE+NL'=7,'Nordic countries+CH+NL'=8,
                                                                                     'Nordic countries+BE+CH+NL'=9,'Nordic countries+AT+BE+CH+NL'=10,
                                                                                     'Nordic countries+AT+BE+CH+DE+NL'=11,'Nordic countries+AT+BE+CH+DE+FR+NL'=12,
                                                                                     'Nordic countries+AT+BE+CH+DE+FR+IE+NL'=13,'Nordic countries+AT+BE+CH+DE+FR+IE+NL+UK'=14,'All countries'=15),
                                                                      selected = RefCntrSel),
                                                          colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                                          content=c('Please set the "Duration of stay correction" first before setting this parameter','','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "Overview" for more information about the bilateral flows ratio model.')),
                                                   
                                  ),
                                  tags$hr(style="border-color: black;"),
                                  h4('Imputations of missing values'),
                                  helper(checkboxInput("Eimputations", "Use PCA imputations", value = TRUE),
                                         colour='#FF0000',type='inline',title='PCA imputations',buttonLabel = 'Close',
                                         content=c('Impute the missing values of the flows ratios matrix with the Principal Components Analysis model using missMDA package.')),
                                  conditionalPanel(condition = "input.Eimputations == true",
                                                   helper(sliderInput(inputId = "Encp", label = 'ncp parameter', min = 1, max = 5, value = 1, step=1, sep=''),
                                                          colour='#FF0000',type='inline',title='ncp parameter',buttonLabel = 'Close',
                                                          content='From imputePCA {missMDA package}: "integer corresponding to the number of components used to to predict the missing entries".')),
                                ),
                                sidebarPanel(width=8,
                                             
                                             h3(HTML('Classification options')),
                                             tags$hr(style="border-color: black;"),
                                             column(width=4,br(),
                                                    checkboxInput("EIgnoreOverCounting", "Ignore over-counting", value = TRUE)),
                                             column(width=8,
                                                    div(id='Ezupa',
                                                        sliderInput(inputId = "ETranslateGroups", label = 'Model sensitivity (number of classes)', min = 2, max = 10, value = 5, step=1, sep=''))
                                             ),
                                             span(HTML('&#160;'),style="font-size:1px; align: top;"),
                                             tags$head(tags$style("#Ezupa .form-group.shiny-input-container {margin-bottom: 0px;}", media="screen", type="text/css")),
                                ),
                                sidebarPanel(width=8,
                                             style='background-color: #FFFFFF; border-color: #FFFFFF; padding: 0px; margin-bottom: -15px;',
                                             radioGroupButtons(
                                               inputId = "Epanels",
                                               label = NULL,#"Select the result panel", 
                                               justified= TRUE,
                                               width='100%',
                                               individual=FALSE,
                                               #checkIcon = list(  yes = icon("check-square")),
                                               choiceNames = c("Inspect bilateral flows ratios",HTML("log<sub>10</sub> flows ratios"), "Classification of model undercounting"),
                                               choiceValues = 1:3,
                                               status = "danger"
                                             )),
                                tags$head(tags$style("#Epanels .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Epanels .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),
                                conditionalPanel(condition = "input.Epanels == 1",
                                                 sidebarPanel(width=8,
                                                              helper(h3('Bilateral flows ratios for emigration data'),
                                                                     colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                                                     content='BilateralModel',size='l'),
                                                              tags$hr(style="border-color: black;"),
                                                              checkboxGroupInput("Ecountry", h4("Countries selection"),
                                                                                 choices = Countries, selected = c('ES','BG','FI','SK','IT'), inline = TRUE),
                                                              actionButton("Iall", "All"),actionButton("Inone", "None"),
                                                              br(),
                                                              plotOutput(outputId = "EmiPlot", height="600px", width='100%'),
                                                              br(),
                                                              div(style="display:inline-block;vertical-align:top;",
                                                                  column(6,checkboxInput("Elogscale", "Use log-scale", value = TRUE)),
                                                                  column(6,checkboxInput("EplotCI", "Plot confidence intervals", value = TRUE)),
                                                              ),
                                                              div(style="display:inline-block;vertical-align:top;",
                                                                  h5('Choose a format and save the plot'),
                                                                  column(6,selectInput("Eformat", NULL,
                                                                                       choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  column(6,downloadButton("Esaveplot", "Save plot")))
                                                 )),
                                conditionalPanel(condition = "input.Epanels == 2",
                                                 sidebarPanel(width=8,
                                                              
                                                              h3(HTML('Estimated log<sub>10</sub> ratios of the bilateral flows')),
                                                              tableOutput('Elogratios'),
                                                              tags$head(tags$style("#Elogratios {border-width:4px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Elogratios table {font-size: 8px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Elogratios table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Elogratios table td {padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Quantile-based thresholds used for classification.')),
                                                              tableOutput('ElogratiosThresholds'),
                                                              tags$head(tags$style("#ElogratiosThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#ElogratiosThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),
                                                 )),
                                conditionalPanel(condition = "input.Epanels == 3",
                                                 sidebarPanel(width=8,
                                                              h3(HTML('Classification of the bilateral flow ratios.')),
                                                              plotOutput(outputId = "EmiPlotB", height="600px", width='100%'),
                                                              
                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),
                                                                         
                                                                         helper(checkboxInput("ENoData", "Mark no data", value = TRUE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c('Tick the cases where the calculation of bilateral flows was impossible due to missing flows in the considered country or reference countries.')
                                                                         )
                                                                  ),
                                                                  column(3,h5(HTML('Image format')),selectInput("EformatB", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  
                                                                  column(3,h5(HTML('&#160;')),downloadButton("EsaveplotB", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("EsavedataB", "Save results as xlsx"))
                                                              ),
                                                 ),
                                ),
                                mainPanel(br(),br(),br(),br(),br())
                       ),
                       tabPanel(title = PanelNames[8],
                                br(), br(),
                                sidebarPanel(
                                  h3('Mixing options'),
                                  tags$hr(style="border-color: black;"),
                                  h4('Mixing threshold'),
                                  helper(
                                    sliderInput(inputId = "IYear", label = 'Threshold year', min = 2003, max = 2018, value = 2008, step=1, sep=''),
                                    colour='#FF0000',type='inline',title='Threshold year',buttonLabel = 'Close',
                                    content='Expert opinion (IMEM) and metadata can be of greater importance to earlier years. <b> Threshold year </b> allows you to set two different sets of weights for two separate time periods.'),
                                  
                                  tags$hr(style="border-color: black;"),
                                  
                                  helper(uiOutput('I2yearshowA'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous panels)'),
                                  #tags$hr(style="border-color: black; border-top: dashed 1px"),
                                  
                                  helper(sliderInput(inputId = "I3wimemb", label = "IMEM score num (A)", min = 0, max = 1, value = wimemb, step=Step),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('A')),
                                  
                                  helper(sliderInput(inputId = "I3wmetab", label = "Metadata score num (A)", min = 0, max = 1, value = wmetab, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content=METAwtxt),
                                  
                                  helper(sliderInput(inputId = "I3wmodelb", label = "Model score num (A)", min = 0, max = 1, value = wmodelb, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content=MODELwtxt),
                                  
                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from (E)</b> - replaces current values of parameters with equivalent values of parameters from <b>Combined scores (E)</b> page, <b>&#8721 weights = 1</b> makes all weights to sum to 1.'),
                                  
                                  actionButton("I3weightsresetb", "Reset"),
                                  actionButton("I3cloneb", "Clone from (E)"),
                                  actionButton("I3recalcb", HTML("&#8721 weights = 1")),
                                  
                                  tags$hr(style="border-color: black; border-top: dashed 1px"),
                                  helper(uiOutput('I2yearshowB'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous panels)'),
                                  #tags$hr(style="border-color: black; border-top: dashed 1px"),
                                  
                                  helper(sliderInput(inputId = "I3wimema", label = "IMEM score num (B)", min = 0, max = 1, value = wimema, step=Step),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('B')),
                                  
                                  helper(sliderInput(inputId = "I3wmetaa", label = "Metadata score num (B)", min = 0, max = 1, value = wmetaa, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content=METAwtxt),
                                  
                                  helper(sliderInput(inputId = "I3wmodela", label = "Model score num (B)", min = 0, max = 1, value = wmodela, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content=MODELwtxt),
                                  
                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from (E)</b> - replaces current values of parameters with equivalent values of parmeters from <b>Combined scores (E)</b> page, <b>&#8721 weights = 1</b> makes all weights to sum to 1.'),
                                  
                                  actionButton("I3weightsreseta", "Reset"),
                                  actionButton("I3clonea", "Clone from (E)"),
                                  actionButton("I3recalca", HTML("&#8721 weights = 1")),                                  
                                  br(),
                                  br(),
                                ),
                                sidebarPanel(width=8,
                                             h3('Classification options'),
                                             tags$hr(style="border-color: black;"),
                                             helper(sliderInput(inputId = "IFinalGroups", label = 'Number of undercounting classes', min = 2, max = 7, value = 5, step=1, sep=''),
                                                    colour='#FF0000',type='inline',title='Number of undercounting classes',buttonLabel = 'Close',
                                                    content='Undercounting is categorized according to uniformly spaced thresholds. See <b>Mean weighted scores</b> panel below.'
                                             ),
                                ),
                                sidebarPanel(width=8,
                                             style='background-color: #FFFFFF; border-color: #FFFFFF; padding: 0px; margin-bottom: -15px;',
                                             radioGroupButtons(
                                               inputId = "Ipanels2",
                                               label = NULL,#"Select the result panel", 
                                               justified= TRUE,
                                               width='100%',
                                               individual=FALSE,
                                               choiceNames = c("Normalized mixing weights", "Mean weighted scores","Classification of undercounting"),
                                               choiceValues = 1:3,
                                               status = "danger"
                                             )),
                                tags$head(tags$style("#Ipanels2 .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Ipanels2 .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),
                                
                                sidebarPanel(width=8,
                                             conditionalPanel(condition ="input.Ipanels2 == 1",
                                                              h3('Normalized mixing weights'),
                                                              uiOutput('I2yearshowC'),
                                                              plotOutput('I3WBPlot', height='200', width='100%'), 
                                                              br()
                                             ),
                                             conditionalPanel(condition ="input.Ipanels2 == 2",
                                                              h3(HTML('Mean weighted scores used to classify undercounting.')),
                                                              tableOutput('Iscores'),
                                                              tags$head(tags$style("#Iscores {border-width:4px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Iscores table {font-size: 8px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Iscores table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              
                                                              tags$head(tags$style("#Iscores table td {padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Uniformly spaced thresholds used for classification.')),
                                                              tableOutput('IscoresThresholds'),
                                                              tags$head(tags$style("#IscoresThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#IscoresThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),
                                                              
                                                              br()
                                             ),
                                             conditionalPanel(condition ="input.Ipanels2 == 3",
                                                              h3(HTML('Finall classification of the undercounting.')),
                                                              plotOutput(outputId = "ImiPlot2", height="680px", width='100%'),
                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),
                                                                         
                                                                         helper(checkboxInput("INoData2", "Mark no data", value = TRUE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c('This option refers to the modeled undercounting',
                                                                                          'If selected it ticks the cases where calculation of bilateral flows was impossible due to missing flows in the considered country or reference countries.'
                                                                                )),
                                                                  ),
                                                                  
                                                                  column(3,h5(HTML('Image format')),selectInput("Iformat2", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  
                                                                  column(3,h5(HTML('&#160;')),downloadButton("Isaveplot2", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("Isavedata2", "Save results as xlsx"))
                                                              ),
                                             ),
                                ),
                                
                                mainPanel(br(),br(),br(),br(),br()),
                       ),
                       tabPanel(title = PanelNames[9],
                                br(), br(),
                                sidebarPanel(
                                  h3('Mixing options'),
                                  tags$hr(style="border-color: black;"),
                                  h4('Mixing threshold'),
                                  helper(
                                    sliderInput(inputId = "EYear", label = 'Threshold year', min = 2003, max = 2018, value = 2008, step=1, sep=''),
                                    colour='#FF0000',type='inline',title='Threshold year',buttonLabel = 'Close',
                                    content='Expert opinion (IMEM) and metadata can be of greater importance to earlier years. <b> Threshold year </b> allows you to set two different sets of weights for two separate time periods.'),
                                  
                                  tags$hr(style="border-color: black;"),
                                  
                                  helper(uiOutput('E2yearshowA'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous panels)'),
                                  
                                  helper(sliderInput(inputId = "E3wimemb", label = "IMEM score num (A)", min = 0, max = 1, value = wimemb, step=Step),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('A')),
                                  
                                  helper(sliderInput(inputId = "E3wmetab", label = "Metadata score num (A)", min = 0, max = 1, value = wmetab, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content=METAwtxt),
                                  
                                  helper(sliderInput(inputId = "E3wmodelb", label = "Model score num (A)", min = 0, max = 1, value = wmodelb, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content=MODELwtxt),
                                  
                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from (E)</b> - replaces current values of parameters with equivalent values of parameters from <b>Combined scores (E)</b> page, <b>&#8721 weights = 1</b> makes all weights to sum to 1.'),
                                  
                                  actionButton("E3weightsresetb", "Reset"),
                                  actionButton("E3cloneb", "Clone from (E)"),
                                  actionButton("E3recalcb", HTML("&#8721 weights = 1")),
                                  
                                  tags$hr(style="border-color: black; border-top: dashed 1px"),
                                  helper(uiOutput('E2yearshowB'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous panels)'),
                                  
                                  helper(sliderInput(inputId = "E3wimema", label = "IMEM score num (B)", min = 0, max = 1, value = wimema, step=Step),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('B')),
                                  
                                  helper(sliderInput(inputId = "E3wmetaa", label = "Metadata score num (B)", min = 0, max = 1, value = wmetaa, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content=METAwtxt),
                                  
                                  helper(sliderInput(inputId = "E3wmodela", label = "Model score num (B)", min = 0, max = 1, value = wmodela, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content=MODELwtxt),
                                  
                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from (E)</b> - replaces current values of parameters with equivalent values of parmeters from <b>Combined scores (E)</b> page, <b>&#8721 weights = 1</b> makes all weights to sum to 1.'),
                                  
                                  actionButton("E3weightsreseta", "Reset"),
                                  actionButton("E3clonea", "Clone from (I)"),
                                  actionButton("E3recalca", HTML("&#8721 weights = 1")),                                  
                                  br(),
                                  br(),
                                ),
                                sidebarPanel(width=8,
                                             h3('Classification options'),
                                             tags$hr(style="border-color: black;"),
                                             helper(sliderInput(inputId = "EFinalGroups", label = 'Number of undercounting classes', min = 2, max = 7, value = 5, step=1, sep=''),
                                                    colour='#FF0000',type='inline',title='Number of undercounting classes',buttonLabel = 'Close',
                                                    content='Undercounting is categorized according to uniformly spaced thresholds. See <b>Mean weighted scores</b> panel below.'
                                             ),
                                ),
                                sidebarPanel(width=8,
                                             style='background-color: #FFFFFF; border-color: #FFFFFF; padding: 0px; margin-bottom: -15px;',
                                             radioGroupButtons(
                                               inputId = "Epanels2",
                                               label = NULL,#"Select the result panel", 
                                               justified= TRUE,
                                               width='100%',
                                               individual=FALSE,
                                               choiceNames = c("Normalized mixing weights", "Mean weighted scores","Classification of undercounting"),
                                               choiceValues = 1:3,
                                               status = "danger"
                                             )),
                                tags$head(tags$style("#Epanels2 .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Epanels2 .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),
                                
                                sidebarPanel(width=8,
                                             conditionalPanel(condition ="input.Epanels2 == 1",
                                                              h3('Normalized mixing weights'),
                                                              uiOutput('E2yearshowC'),
                                                              plotOutput('E3WBPlot', height='200', width='100%'), 
                                                              br()
                                             ),
                                             conditionalPanel(condition ="input.Epanels2 == 2",
                                                              h3(HTML('Mean weighted scores used to classify undercounting.')),
                                                              tableOutput('Escores'),
                                                              tags$head(tags$style("#Escores {border-width:4px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Escores table {font-size: 8px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Escores table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              
                                                              tags$head(tags$style("#Escores table td {padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Uniformly spaced thresholds used for classification.')),
                                                              tableOutput('EscoresThresholds'),
                                                              tags$head(tags$style("#EscoresThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#EscoresThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),
                                                              
                                                              br()
                                             ),
                                             conditionalPanel(condition ="input.Epanels2 == 3",
                                                              h3(HTML('Finall classification of the undercounting.')),
                                                              plotOutput(outputId = "EmiPlot2", height="680px", width='100%'),
                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),
                                                                         
                                                                         helper(checkboxInput("ENoData2", "Mark no data", value = TRUE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c('This option refers to the modeled undercounting',
                                                                                          'If selected it ticks the cases where calculation of bilateral flows was impossible due to missing flows in the considered country or reference countries.'
                                                                                )),
                                                                  ),
                                                                  
                                                                  column(3,h5(HTML('Image format')),selectInput("Eformat2", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  
                                                                  column(3,h5(HTML('&#160;')),downloadButton("Esaveplot2", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("Esavedata2", "Save results as xlsx"))
                                                              ),
                                             ),
                                ),
                                
                                mainPanel(br(),br(),br(),br(),br()),
                                
                       )
           )
    )
  )
)

shinyApp(ui=shinyUI, server = shinyServer)

#rsconnect::deployApp()

