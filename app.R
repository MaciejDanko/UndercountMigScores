rm(list=ls())

#Sys.setlocale("LC_ALL","en_US.UTF-8")
#install.packages(c('Cairo','DT','shiny','colourpicker','shinyhelper','magicaxis','data.table','countrycodes'))

library(Cairo)
library(devtools)
library(DT)
library(shiny)
library(colourpicker)
library(shinyhelper)
library(magicaxis)
library(data.table)
library(countrycode)

load('./data/MetaData.rda')
load('./data/UndercountingIndex.rda')

Meta_Reg$comment[Meta_Reg$iso2=='EE']<-'No sanctions'

colnames(Meta_Reg)<-c("iso2", "country", "registration obligation", "time limit", "comment", "score" )
colnames(Meta_DeReg)<-c('iso2','country', "de-registration obligation", "de-registration obligation third country nationals",
                        "monitoring third country nationals",
                        "administrative corrections",'comment')

Countries<-CountriesS<-unique(NORDIC$iso2[!is.na(NORDIC$ICnraw)])

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

Recalc_Meta_DeReg<-function(MetaDeReg,w1,w2,w3,w4,t1,t2, trustnordic){
  cat(w1,w2,w3,w4,t1,t2,trustnordic,'\n')
  MetaDeReg<-DT2DF(MetaDeReg)
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
  MetaDeReg<-datatable(MetaDeReg, options=list(pageLength=nrow(MetaDeReg), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  MetaDeReg<-formatStyle(MetaDeReg, columns = "score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000"))) 
  MetaDeReg<-formatStyle(MetaDeReg, c(2,7), "border-right" = "solid 1px", "border-right-color"='black')
  MetaDeReg
}

Recalc_Meta_Reg<-function(MetaReg, trustnordic=TRUE){
  cat(trustnordic,class(MetaReg),'\n')
  MetaReg<-DT2DF(MetaReg)
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
  MetaReg<-datatable(MetaReg, rownames=FALSE, options=list(pageLength=nrow(MetaReg), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  MetaReg<-formatStyle(MetaReg, columns = "score", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000"))) 
  MetaReg<-formatStyle(MetaReg, columns = "time limit", fontWeight = styleEqual('No limit', c("bold"))) 
  MetaReg<-formatStyle(MetaReg, columns = "comment", fontWeight = styleEqual('No sanctions', c("bold"))) 
  MetaReg<-formatStyle(MetaReg, c(2,5), "border-right" = "solid 1px", "border-right-color"='black')
  MetaReg
}

firstCap<-function(x) {
  x<-paste(x)
  tmp<-paste(toupper(substr(x,1,1)),substr(x,2,nchar(x)),sep='')
  tmp[tmp=='NA']<-NA
  tmp
}

reformatIE2tab<-function(tab, chide=TRUE, COLO){
  tab<-DT2DF(tab)
  if (chide) tab<-tab[tab[,1]%in%Countries,]
  cat(COLO,'\n')
  #COLO<-adjustcolor(COLO,blue.f = 0.9,red.f = 0.9,#008000.f = 0.9)
  tab$B.score<-firstCap(tab$B.score)
  tab$A.score<-firstCap(tab$A.score)
  colnames(tab)<-c('iso2','country','median (B)','lo (B)','hi (B)','score (B)','score num (B)','median (A)','lo (A)','hi (A)','score (A)','score num (A)')
  LEVELS<-firstCap(c('very low','low', 'medium','high','very high'))
  tab<-datatable(tab,options=list(pageLength=nrow(tab), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                 rownames=FALSE)
  tab<-formatStyle(tab, columns = "score (B)", color=styleEqual(LEVELS, COLO)) 
  tab<-formatStyle(tab, c(2,7), "border-right" = "solid 1px", "border-right-color"='black')
  tab<-formatStyle(tab, columns = "score (A)", color=styleEqual(LEVELS, COLO)) 
  tab
}

CBA<-function(refcountry=1, threshyear = 2008, direction='E',corrected=TRUE, 
              thr1=BB[2],thr2=BB[3],thr3=BB[4],thr4=BB[5],
              NBoot=1e5, LEVELS=c('very low','low','medium','high','very high')){
  
  RES<-switch(refcountry, '1' = NORDIC, '2' = NORDIC_PLUS_BE_CH,
              '3' = NORDIC_PLUS_AT_BE_CH_IE_NL, 
              '4' = NORDIC_PLUS_AT_DE_BE_CH_FR_IE_NL, '5' = ALL_COUNTRIES)
  THRESH<-log10(1/c(1,thr1,thr2,thr3,thr4,1e-5))
  
  if (direction=='E' && corrected){
    RES$Y<-RES$ECraw
    RES$YE<-RES$EC
    RES$Ysd<-RES$EC_sd
    RES$W<-RES$POPEn
  } else if (direction=='E' && !corrected){
    RES$Y<-RES$EUCraw
    RES$YE<-RES$EUC
    RES$Ysd<-RES$EUC_sd
    RES$W<-RES$POPEn
  } else if (direction=='I' && corrected){
    RES$Y<-RES$ICraw
    RES$YE<-RES$IC
    RES$Ysd<-RES$IC_sd
    RES$W<-RES$POPIn
  } else if (direction=='I' && !corrected){
    RES$Y<-RES$IUCraw
    RES$YE<-RES$IUC
    RES$Ysd<-RES$IUC_sd
    RES$W<-RES$POPIn
  } 
  
  cols<-c('iso2','Y','YE','Ysd','W','year')
  RESLO<-RES[RES$year<threshyear,cols]
  RESHI<-RES[RES$year>=threshyear,cols]
  
  UG<-unique(RES$iso2)
  
  RESHIStat<-lapply(UG, function(k) {
    cat(k,'\n')
    tmp<-RESHI[RESHI$iso2==k,]
    Qua<-c(NA,NA,NA)
    Qme<-NA
    if (!all(is.na(tmp$Y))) {
      org<-tmp<-tmp[!is.na(tmp$Y),]
      tmp<-tmp[!is.na(tmp$Ysd),]
      H<-tmp$W/sum(tmp$W)
      if (length(H)>0){
        ToBoot<-NBoot
        YY<-NULL 
        for (j in 1:5) {
          BOOTini<-sample(seq_along(H), ToBoot, prob=H, replace = TRUE)
          YYy<-as.vector(rnorm(ToBoot,tmp$Y[BOOTini],tmp$Ysd[BOOTini]))
          YYy<-YYy[YYy>0]
          YY<-c(YY,YYy)
          ToBoot<-ToBoot-length(YY)
          if ((ToBoot/NBoot)<0.01) break
        }
        Qua<-quantile(YY,probs=c(0.025,0.5,0.975))
        Qme<-mean(YY)
      } else {
        Qua<-c(NA,median(org$Y),NA)
        Qme<-mean(org$Y)
      }
    }
    list(iso2=k, median=Qua[2], q_lo=Qua[1], q_hi=Qua[3], mean=Qme)#, sd=Qsd)
  })
  RESHIStat<-data.frame(data.table::rbindlist(RESHIStat),stringsAsFactors = FALSE)
  
  RESLOStat<-lapply(UG, function(k) {
    cat(k,'\n')
    tmp<-RESLO[RESLO$iso2==k,]
    Qua<-c(NA,NA,NA)
    Qme<-NA
    if (!all(is.na(tmp$Y))) {
      tmp<-tmp[!is.na(tmp$Y),]
      tmp<-tmp[!is.na(tmp$Ysd),]
      H<-tmp$W/sum(tmp$W)
      if (length(H)>0){
        ToBoot<-NBoot
        YY<-NULL 
        for (j in 1:5) {
          BOOTini<-sample(seq_along(H), ToBoot, prob=H, replace = TRUE)
          YYy<-as.vector(rnorm(ToBoot,tmp$Y[BOOTini],tmp$Ysd[BOOTini]))
          YYy<-YYy[YYy>0]
          YY<-c(YY,YYy)
          ToBoot<-ToBoot-length(YY)
          if ((ToBoot/NBoot)<0.01) break
        }
        
        Qua<-quantile(YY, probs=c(0.025,0.5,0.975))
        Qme<-mean(YY)
      }
    }
    list(iso2=k, median=Qua[2], q_lo=Qua[1], q_hi=Qua[3], mean=Qme)#, sd=Qsd)
  })
  RESLOStat<-data.frame(data.table::rbindlist(RESLOStat),stringsAsFactors = FALSE)
  
  negi<-function(x) {x[x>=0]<- 1e-9; abs(x)}
  B <- RESLOStat$median
  A <- RESHIStat$median
  RESLOStat<-data.frame(Est = RESLOStat$median, lo.CI = RESLOStat$q_lo, hi.CI = RESLOStat$q_hi)
  RESHIStat<-data.frame(Est = RESHIStat$median, lo.CI = RESHIStat$q_lo, hi.CI = RESHIStat$q_hi)
  
  cat(' >> ',THRESH,'\n')
  resB <- cut(x=negi(log10(B)), breaks=THRESH,include.lowest=TRUE,labels=LEVELS)
  resA <- cut(x=negi(log10(A)), breaks=THRESH,include.lowest=TRUE,labels=LEVELS)
  
  resBnum <- round((as.numeric(resB)-1)/(length(THRESH)-2),4)
  resAnum <- round((as.numeric(resA)-1)/(length(THRESH)-2),4)
  
  res<-data.frame(iso2=UG,
                  country=countrycode::countrycode(UG,'eurostat','country.name'),
                  B=round(RESLOStat,4),
                  B.score=resB,
                  B.score.num=resBnum,
                  A=round(RESHIStat,4),
                  A.score=resA,
                  A.score.num=resAnum,
                  stringsAsFactors = FALSE,
                  check.names = FALSE)
  res
}

plot.BA<-function(RES,thr1=BB[2],thr2=BB[3],thr3=BB[4],thr4=BB[5], threshyear=2008, plotci=TRUE, logscale=TRUE, colCI='#FFFFFF',hidec=FALSE, 
                  #COLO=c("#2da70b","#306005","#fff00f","#c05508","#ff9f9f")
                  COLO){
  if (hidec) RES<-RES[RES$iso2 %in% Countries,]
  if (logscale) flog10<-log10 else flog10<-function(x) identity(x)
  THRESH<-flog10(1/c(1+(!logscale)*99,thr1,thr2,thr3,thr4,1e-5))
  if (!logscale) THRESH = 1/THRESH
  #COL=c(rgb(0.8,0.4,0.1),rgb(0.3,0.4,0.9))
  COL<-c('#D5D505','#000080')
  COLd<-c('#A0A005','#000080')
  #COLl<-c('#D5D505','#000090')
  if (logscale) neg2NA<-function(x) {x[x<=0]<-NA;x} else neg2NA<-function(x) {x[x<=0]<-0;x}
  X<-rbind(flog10(RES$B.Est),flog10(RES$A.Est))
  par(oma=c(0,0,2,0),mar=c(2.65,4,2.5,0))
  YLIM<-flog10(c(min(neg2NA(RES$B.lo.CI),neg2NA(RES$A.lo.CI),neg2NA(RES$B.Est),neg2NA(RES$A.Est),na.rm=TRUE),
                 max(RES$B.hi.CI,RES$A.hi.CI,RES$A.Est,RES$B.Est,na.rm = TRUE)))
  if(!logscale) YLIM<-1.1*c(0,max(min(2,YLIM[2]),RES$A.Est,RES$B.Est,na.rm = TRUE))
  z<-barplot(X,axes=F,beside=TRUE,col=COL, ylim=YLIM)
  axis(1, at=colMeans(z),labels = RES$iso2,las=3,cex.axis=1.3, font=1); 
  mtext('Bilateral flows ratio',2,2.5,cex=1.5)  
  box();box();
  # col.pal=c(adjustcolor('green',alpha.f = 0.5),
  #           adjustcolor('green4',alpha.f = 0.5),
  #           adjustcolor('yellow2',alpha.f = 0.5),
  #           adjustcolor('orange',alpha.f = 0.5),
  #           adjustcolor('red2',alpha.f = 0.25))
  col.pal<-COLO
  if(logscale){
    rect(-10,-THRESH[6],max(z)*2,THRESH[6],col=col.pal[5],border=NA)
    rect(-10,-THRESH[5],max(z)*2,THRESH[6],col=col.pal[4],border=NA)
    rect(-10,-THRESH[4],max(z)*2,THRESH[6],col=col.pal[3],border=NA)
    rect(-10,-THRESH[3],max(z)*2,THRESH[6],col=col.pal[2],border=NA)
    rect(-10,-THRESH[2],max(z)*2,THRESH[6],col=col.pal[1],border=NA)
    abline(h=-THRESH,lwd=2)
  } else {
    rect(-10,20,max(z)*2,-1,col=col.pal[5],border=NA)
    rect(-10,THRESH[5],max(z)*2,20,col=col.pal[4],border=NA)
    rect(-10,THRESH[4],max(z)*2,20,col=col.pal[3],border=NA)
    rect(-10,THRESH[3],max(z)*2,20,col=col.pal[2],border=NA)
    rect(-10,THRESH[2],max(z)*2,20,col=col.pal[1],border=NA)
    abline(h=THRESH[-length(THRESH)],lwd=2)
  }
  abline(v=colMeans(z),lty=3,col=rgb(0.6,0.6,0.6))
  
  axis(3, at=z[1,]-0.15,labels =toupper(RES$B.score),las=3,cex.axis=0.75,col.axis=COLd[1],padj=0.5)
  axis(3, at=z[2,]+0.15,labels =toupper(RES$A.score),las=3,cex.axis=0.75,col.axis=COLd[2],padj=0.5)
  z<-barplot(X,axes=F,beside=TRUE,col=COL,add=TRUE);box();box()
  if (logscale) magicaxis::magaxis(2,unlog=T,cex.axis=1.5) else axis(2,cex.axis=1.5)
  if (plotci) {
    LOCI<-rbind(flog10(RES$B.lo.CI),flog10(RES$A.lo.CI))
    HICI<-rbind(flog10(RES$B.hi.CI),flog10(RES$A.hi.CI))
    for (j in 1:2) for(k in 1:ncol(z)){
      lines(c(z[j,k],z[j,k]),c(LOCI[j,k],HICI[j,k]),lwd=2,col=colCI)
    }
  }
  if(logscale) lpos<-'bottomright' else lpos<-'topright'
  legend(lpos,bty='o',c(paste('Before',threshyear), paste('After',threshyear-1)),text.col = COL,fill=COL,cex=1.5,
         bg=adjustcolor('white',0.3),box.col=adjustcolor('white',0.3))
  
}

plot_ui_result_<-function(DAT, country, Draw='IUCraw', Dextr='IUC',Dsd='IUC_sd',Dqmed='IUC_q.med',Dqlo='IUC_q.lo',Dqhi='IUC_q.hi',
                          stats=1, logscale=FALSE, extrapol=TRUE, plotCI=TRUE,toplab=''){
  if (stats==1) {
    DAT$Y<-DAT[,Draw]
    DAT$YE<-DAT[,Dextr]
    DAT$Ylo<-DAT[,Draw]-DAT[,Dsd]*1.96
    K<-0.000001
    DAT$Ylo[DAT$Ylo<=K]<-K 
    DAT$Yhi<-DAT[,Draw]+DAT[,Dsd]*1.96
  } else if (stats==2){
    DAT$Y<-DAT[,Dqmed]
    DAT$YE<-DAT[,Dextr]
    DAT$Ylo<-DAT[,Dqlo]
    DAT$Yhi<-DAT[,Dqhi]
    K<-0.000001
    DAT$Ylo[DAT$Ylo<=K]<-K 
  }
  if (logscale) {
    DAT$Y<-log10(DAT$Y)
    DAT$Ylo<-log10(DAT$Ylo)
    DAT$Yhi<-log10(DAT$Yhi)
    DAT$YE <- log10(DAT$YE)
    minY<-log10(0.005)
  } else {
    minY<-0.005
  }
  layout(matrix(c(rep(1,4),2),1,5))
  par(mar=c(4,2.5,0.5,0),oma=c(0,2.5,2,0))
  XR<-range(NORDIC$year)
  inf2NA<-function(x){ x[is.infinite(x)]<-NA; x}
  YLIM<-c(minY,max(inf2NA(c(DAT$Ylo,DAT$Yhi,DAT$Y)),na.rm = TRUE))
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
    if (extrapol) lines(YY,YE,col=CDAT$col,type='l',lty=2)
  }
  plot(1:2,1:2,axes=FALSE,type='n',xlab='',ylab=''); #box()
  legend('topleft',country,col=country.col,bty='n',lty=1,cex=1.75,lwd=2)  
  legend('topleft',country,col=adjustcolor(country.col,0.4),bty='n',lty=1,cex=1.75,lwd=15.5)  
}

NORDIC_PLUS_AT_BE_CH_IE_NL[NORDIC_PLUS_AT_BE_CH_IE_NL$iso2=='LT',]

plot_ui_result<-function(direction, country, refcountry, stats, extrapol, raymer, logscale, plotCI){
  if (length(country)){
    DAT<-switch(refcountry, '1' = NORDIC, '2' = NORDIC_PLUS_BE_CH, '3' = NORDIC_PLUS_AT_BE_CH_IE_NL, 
                '4' = NORDIC_PLUS_AT_DE_BE_CH_FR_IE_NL, '5' = ALL_COUNTRIES)
    
    validC<-DAT$iso2[!is.na(DAT$ICnraw)]
    DAT<-DAT[DAT$iso2%in%validC,]
    DAT$col<-as.numeric(as.factor(DAT$iso2))
    PAL<-palette.colors(max(DAT$col),'polychrome 36')
    PAL <- c(
      "dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "#FB9A99", "palegreen2",
      "#CAB2D6", "#FDBF6F", "gray70", "khaki2", "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
      "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4", "brown")
    PAL<-PAL[order(nchar(PAL))]
    DAT$col<-PAL[DAT$col]
    DAT<-DAT[DAT$iso2%in%country,]
    if (direction=='I'  && !raymer){
      plot_ui_result_(DAT, country, Draw='IUCraw', Dextr='IUC',Dsd='IUC_sd',Dqmed='IUC_q.med',Dqlo='IUC_q.lo',Dqhi='IUC_q.hi',
                      stats=stats, logscale=logscale, extrapol=extrapol, plotCI=plotCI,'Immigration')
    } else if (direction=='I'  && raymer){
      plot_ui_result_(DAT, country, Draw='ICraw', Dextr='IC',Dsd='IC_sd',Dqmed='IC_q.med',Dqlo='IC_q.lo',Dqhi='IC_q.hi',
                      stats=stats, logscale=logscale, extrapol=extrapol, plotCI=plotCI,'Immigration')
    } else if (direction=='E'  && !raymer){
      plot_ui_result_(DAT, country, Draw='EUCraw', Dextr='EUC',Dsd='EUC_sd',Dqmed='EUC_q.med',Dqlo='EUC_q.lo',Dqhi='EUC_q.hi',
                      stats=stats, logscale=logscale, extrapol=extrapol, plotCI=plotCI,'Emigration')
    } else if (direction=='E'  && raymer){
      plot_ui_result_(DAT, country, Draw='ECraw', Dextr='EC',Dsd='EC_sd',Dqmed='EC_q.med',Dqlo='EC_q.lo',Dqhi='EC_q.hi',
                      stats=stats, logscale=logscale, extrapol=extrapol, plotCI=plotCI,'Emigration')
    } 
  }
}

CalcCombineThreshols<-function(META, MODEL, thr1=0.25, thr2=0.6, wimema=0.25, 
                               wimemb=0.5, wmetaa=0.2, wmetab=0.2, wmodela=1, wmodelb=1, 
                               mirror=TRUE, direction='I'){
  
  cat(thr1, thr2, wimema, wimemb, wmetaa, wmetab, wmodela, wmodelb, mirror, '\n') 
  META<-DT2DF(META)
  Msc<-META$`score num`
  
  LIMEM<-c('Low','High')
  IMEMi<-IMEM[1:nrow(META),]
  if(!all(IMEMi$Country==META$iso2)) stop()
  if(direction=='I') posI<-2 else if(direction=='E') posI<-3
  IMEMc<-factor(firstCap(unlist(IMEMi[,posI])),levels=LIMEM)
  IMEMi<-as.numeric(IMEMc)-1
  
  MODEL<-DT2DF(MODEL)
  Bsc<-MODEL$B.score.num
  Asc<-MODEL$A.score.num
  
  L3<-c('Low' ,'Medium', 'High')
  
  namod<-function(x,y){
    z<-rep(y,length(x))
    z[is.na(x)]<-0
    z
  }
  
  na2zero<-function(x) {x[is.na(x)]<-0; x}
  
  IAsc<-is.na(Asc)
  IBsc<-is.na(Bsc)
  
  IAsc2<-IAsc&!IBsc
  IBsc2<-IBsc&!IAsc
  
  IAsc<-IAsc2
  IBsc<-IBsc2
  
  if (mirror){
    Asc[is.na(Asc)]<-Bsc[is.na(Asc)]
    Bsc[is.na(Bsc)]<-Asc[is.na(Bsc)]
  }
  
  BM<-na2zero(Bsc*wmodelb)
  BI<-na2zero(IMEMi*wimemb)
  BE<-na2zero(Msc*wmetab)
  AM<-na2zero(Asc*wmodela)
  AI<-na2zero(IMEMi*wimema)
  AE<-na2zero(Msc*wmetaa)
  Bres<-(BM+BI+BE)/(namod(Bsc,wmodelb)+namod(IMEMi,wimemb)+namod(Msc,wmetab))
  Ares<-(AM+AI+AE)/(namod(Asc,wmodela)+namod(IMEMi,wimema)+namod(Msc,wmetaa))
  
  Bscore<-cut(Bres,c(0,thr1,thr2,1),L3,include.lowest = TRUE)
  Ascore<-cut(Ares,c(0,thr1,thr2,1),L3,include.lowest = TRUE)
  if (!mirror) {
    RR<-data.frame(iso2=META[,1],country=META[,2], 'IMEM score num' = IMEMi, 'metadata score num' = Msc, 'model score num (B)' = Bsc, 'model score num (A)' = Asc, 
                   'combined score num (B)' = round(Bres,3), 'combined score num (A)' = round(Ares,3), 'combined score (B)' = Bscore, 'combined score (A)' = Ascore,
                   check.names = FALSE, stringsAsFactors = FALSE)
    RR<-datatable(RR, rownames=FALSE, options=list(pageLength=nrow(RR), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  } else {
    
    RR<-data.frame(iso2=META[,1],country=META[,2], 'IMEM score num' = IMEMi, 'metadata score num' = Msc, 'model score num (B)' = Bsc, 'model score num (A)' = Asc, 
                   'combined score num (B)' = round(Bres,3), 'combined score num (A)' = round(Ares,3), 'combined score (B)' = Bscore, 'combined score (A)' = Ascore, 
                   Indb = IBsc, Inda = IAsc,
                   check.names = FALSE, stringsAsFactors = FALSE)
    RR<-datatable(RR, rownames=FALSE, options=list(pageLength=nrow(RR), lengthMenu=-1, dom='ft', 
                                                   columnDefs = list(list(className = 'dt-center', targets = '_all'),
                                                                     list(visible=FALSE, targets=c(11,10)))
    ))
    RR<-formatStyle(RR, columns = c("model score num (A)","model score num (B)"), valueColumns = c('Inda','Indb'), color=styleEqual(1,"magenta"))
  }
  RR<-formatStyle(RR, columns = "combined score (A)", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000"))) 
  RR<-formatStyle(RR, columns = "combined score (B)", color=styleEqual(c('Low', 'Medium','High'), c("#008000", "#FFA500","#FF0000"))) 
  RR<-formatStyle(RR, c(2,6,8), "border-right" = "solid 1px", "border-right-color"='black')
  RR
}

summaryTable<-function(META, MODEL, COMBI, direction='I', COLO){
  META<-DT2DF(META)
  COMBI<-DT2DF(COMBI)
  MODEL<-DT2DF(MODEL)
  LIMEM<-c('Low','High')
  IMEMi<-IMEM[1:nrow(META),]
  if(!all(IMEMi$Country==META$iso2)) stop()
  if(direction=='I') posI<-2 else if(direction=='E') posI<-3
  IMEMc<-factor(firstCap(unlist(IMEMi[,posI])),levels=LIMEM)
  
  RED<-"#FF0000"#'#AA0000'
  GREEN<-"#008000"#'#00AA00'
  ORANGE<-"#FFA500"#'#FF7F00'
  # LIGHTRED<-'#FF5555'
  # LIGHTGREEN<-'#7FEE7F'
  # COLO<-c(LIGHTGREEN,GREEN,ORANGE,RED,LIGHTRED)
  LEVELS<-firstCap(c('very low','low', 'medium','high','very high'))
  RR<-data.frame(iso2=META$iso2, country=META$country,'IMEM score' = IMEMc, 'metadata score' = META$score, 
                 "model score (B)" = firstCap(MODEL$B.score), "model score (A)" = firstCap(MODEL$A.score),
                 "combined score (B)" = COMBI$`combined score (B)`,"combined score (A)" = COMBI$`combined score (A)`,
                 stringsAsFactors = FALSE, check.names = FALSE)
  RR<-datatable(RR, rownames=FALSE, options=list(pageLength=nrow(RR), lengthMenu=-1, dom='ft', columnDefs = list(list(className = 'dt-center', targets = '_all'))))
  RR<-formatStyle(RR, columns = "IMEM score", color=styleEqual(c('Low','High'), c(GREEN,RED))) 
  RR<-formatStyle(RR, columns = "metadata score", color=styleEqual(c('Low', 'Medium','High'), c(GREEN, ORANGE,RED))) 
  RR<-formatStyle(RR, columns = "model score (B)", color=styleEqual(LEVELS, COLO)) 
  RR<-formatStyle(RR, columns = "model score (A)", color=styleEqual(LEVELS, COLO)) 
  RR<-formatStyle(RR, columns = "combined score (A)", color=styleEqual(c('Low', 'Medium','High'), c(GREEN, ORANGE,RED))) 
  RR<-formatStyle(RR, columns = "combined score (B)", color=styleEqual(c('Low', 'Medium','High'), c(GREEN, ORANGE,RED))) 
  RR<-formatStyle(RR, c(2,6), "border-right" = "solid 1px", "border-right-color"='black')
  RR
}

thr1 <- 0.25
thr2 <- 0.6
wimema <- 0.25
wimemb <- 0.25
wmetaa <- 0.25
wmetab <- 0.25
wmodela <- 1
wmodelb <- 1
mirror <- TRUE

RefCntrSel <- 3

MWt1 <- 1
MWt2 <- 0.5
MWt3 <- 0.5
MWt4 <- 0.5
MThr1 <- 0.3
MThr2 <- 0.6
TrustNordic<-TRUE
Meta_DeReg<-Recalc_Meta_DeReg(Meta_DeReg,MWt1,MWt2,MWt3,MWt4,MThr1,MThr2,TrustNordic)
Meta_Reg<-Recalc_Meta_Reg(Meta_Reg, TrustNordic)

#COLO=c("#2da70b","#306005","#fff00f","#c05508","#ff9f9f")
#COLO=c("#80FF80",'#00DD00','#FFFF00','#FFDD80','#FF8080')
COLO=c("#00DD00",'#008000','#FFA500','#FF0000','#800000')
# col.pal=c(adjustcolor('green',alpha.f = 0.5),
#           adjustcolor('green4',alpha.f = 0.5),
#           adjustcolor('yellow2',alpha.f = 0.5),
#           adjustcolor('orange',alpha.f = 0.5),
#           adjustcolor('red2',alpha.f = 0.25))
#tmp<-colnames(Meta_DeReg$x$data[,4:7])
# WeightsNam<-paste(c('Obligation of de-registration','Obligation of de-registration of third country nationals','Monitoring third country nationals','Administrative corrections'),
#                   " (",tmp,")",sep='')
WeightsNam<-paste(c('Obligation of de-registration','Obligation of de-registration of third country nationals','Monitoring third country nationals','Administrative corrections'),sep='')
CountriesL<-paste(countrycode::countrycode(Countries,'eurostat','country.name'),' (',Countries,')',sep='')
Countries<-as.list(Countries)
names(Countries)<-CountriesL
PanelNames<-c('About','Metadata classify (I)','Metadata classify (E)','Model plots (I)','Model plots (E)',
              'Model classify (I)','Model classify (E)','Combined scores (I)','Combined scores (E)','Scores summary (I)','Scores summary (E)', 'Help')

IMEMc<-function(k) c('The parameter adds a weight to IMEM (<a href="https://www.imem.cpc.ac.uk/About.aspx">Integrated Modeling of European Migration</a>) undercount classification (Raymer et al. 2013) converted to numerical value (<b>IMEM score num</b>), where
         0 denotes <span style="color:#008000">Low</span> undercounting and 1 denotes <span style="color:#FF0000">High</span> undercounting.','',
                     paste('Weighted <b>IMEM score num</b> is used to calculate <b>combined score num (',k,')</b>',sep=''),'',
                     '<b>References</b>','<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F. and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>')


shinyServer <-  function(input, output, session) {
  
  observe_helpers(withMathJax = TRUE, help_dir = 'helpfiles')
  
  observeEvent(input$I2t1,  {
    updateSliderInput(session = session, inputId = "I2t2", max = input$I2t1)
    updateSliderInput(session = session, inputId = "I2t3", max = input$I2t2)
    updateSliderInput(session = session, inputId = "I2t4", max = input$I2t3)
  })
  
  observeEvent(input$I2t4,  {
    updateSliderInput(session = session, inputId = "I2t3", min = input$I2t4)
    updateSliderInput(session = session, inputId = "I2t2", min = input$I2t3)
    updateSliderInput(session = session, inputId = "I2t1", min = input$I2t2)
  })
  
  observeEvent(input$I2t2,  {
    updateSliderInput(session = session, inputId = "I2t3", max = input$I2t2)
    updateSliderInput(session = session, inputId = "I2t4", max = input$I2t3)
    updateSliderInput(session = session, inputId = "I2t1", min = input$I2t2)
  })
  
  observeEvent(input$I2t3,  {
    updateSliderInput(session = session, inputId = "I2t4", max = input$I2t3)
    updateSliderInput(session = session, inputId = "I2t2", min = input$I2t3)
    updateSliderInput(session = session, inputId = "I2t1", min = input$I2t2)
  })
  
  observeEvent(input$I2treset,  {
    updateSliderInput(session = session, inputId = "I2t4", value = BB[5])
    updateSliderInput(session = session, inputId = "I2t3", value = BB[4])
    updateSliderInput(session = session, inputId = "I2t2", value = BB[3])
    updateSliderInput(session = session, inputId = "I2t1", value = BB[2])
  })
  
  observeEvent(input$I2yearreset,  {
    updateSliderInput(session = session, inputId = "I2year", value = 2008)
  })
  
  
  ###############################
  
  observeEvent(input$E2t1,  {
    updateSliderInput(session = session, inputId = "E2t2", max = input$E2t1)
    updateSliderInput(session = session, inputId = "E2t3", max = input$E2t2)
    updateSliderInput(session = session, inputId = "E2t4", max = input$E2t3)
  })
  
  observeEvent(input$E2t4,  {
    updateSliderInput(session = session, inputId = "E2t3", min = input$E2t4)
    updateSliderInput(session = session, inputId = "E2t2", min = input$E2t3)
    updateSliderInput(session = session, inputId = "E2t1", min = input$E2t2)
  })
  
  observeEvent(input$E2t2,  {
    updateSliderInput(session = session, inputId = "E2t3", max = input$E2t2)
    updateSliderInput(session = session, inputId = "E2t4", max = input$E2t3)
    updateSliderInput(session = session, inputId = "E2t1", min = input$E2t2)
  })
  
  observeEvent(input$E2t3,  {
    updateSliderInput(session = session, inputId = "E2t4", max = input$E2t3)
    updateSliderInput(session = session, inputId = "E2t2", min = input$E2t3)
    updateSliderInput(session = session, inputId = "E2t1", min = input$E2t2)
  })
  
  observeEvent(input$E2treset,  {
    updateSliderInput(session = session, inputId = "E2t4", value = BB[5])
    updateSliderInput(session = session, inputId = "E2t3", value = BB[4])
    updateSliderInput(session = session, inputId = "E2t2", value = BB[3])
    updateSliderInput(session = session, inputId = "E2t1", value = BB[2])
  })
  
  observeEvent(input$E2yearreset,  {
    updateSliderInput(session = session, inputId = "E2year", value = 2008)
  })
  

  ############################### E2year
  
  observeEvent(input$E2year,  {
    updateSliderInput(session = session, inputId = "I2year", value = input$E2year)
  })
  
  observeEvent(input$I2year,  {
    updateSliderInput(session = session, inputId = "E2year", value = input$I2year)
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
                     extrapol=FALSE, 
                     raymer=input$Eraymer, 
                     logscale=input$Elogscale, 
                     plotCI=input$EplotCI)
      dev.off()
    }
  )
  
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
                     extrapol=FALSE, 
                     raymer=input$Iraymer, 
                     logscale=input$Ilogscale, 
                     plotCI=input$IplotCI)
      dev.off()
    }
  )
  
  output$EmiPlot <- renderPlot({
    plot_ui_result('E', 
                   country=input$Ecountry, 
                   refcountry=input$Erefcountry, 
                   stats=2, 
                   extrapol=FALSE, 
                   raymer=input$Eraymer, 
                   logscale=input$Elogscale, 
                   plotCI=input$EplotCI)
  })
  
  output$ImiPlot <- renderPlot({
    plot_ui_result('I', 
                   country=input$Icountry, 
                   refcountry=input$Irefcountry, 
                   stats=2, 
                   extrapol=FALSE, 
                   raymer=input$Iraymer, 
                   logscale=input$Ilogscale, 
                   plotCI=input$IplotCI)
  })
  
  #############################
  
  I2CBA<-reactive({
    CBA(input$I2refcountry, input$I2year, 'I',input$I2raymer, input$I2t1, input$I2t2,
        input$I2t3, input$I2t4, 1e4)
  })
  
  output$I2download<- downloadHandler(
    filename = function() { 
      paste('Immi_Undercounting_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(reformatIE2tab(I2CBA(), input$I2hide, COLO)), file,row.names = FALSE)
    }
  )
  
  output$I2saveplot<- downloadHandler(
    filename = function() { 
      paste('Immi_Undercounting_Scores.', input$I2format, sep='') },
    content = function(file) {
      ffo <- input$I2format
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }  
      plot.BA(I2CBA(),input$I2t1, input$I2t2,
              input$I2t3, input$I2t4, input$I2year, input$I2plotCI, input$I2logscale, input$I2colorCI, input$I2hide, COLO=COLO)
      dev.off()
    }
  )
  
  output$I2table<-renderDT({
    reformatIE2tab(I2CBA(), input$I2hide, COLO)
  })
  
  output$I2miPlot <- renderPlot({
    plot.BA(I2CBA(),input$I2t1, input$I2t2,
            input$I2t3, input$I2t4, input$I2year, input$I2plotCI, input$I2logscale, input$I2colorCI, input$I2hide, COLO=COLO)
  })
  
  #############################
  
  E2CBA<-reactive({
    CBA(input$E2refcountry, input$E2year, 'E',input$E2raymer, input$E2t1, input$E2t2,
        input$E2t3, input$E2t4, 1e4)
  })
  
  output$E2download<- downloadHandler(
    filename = function() { 
      paste('Emi_Undercounting_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(reformatIE2tab(E2CBA() , input$I2hide, COLO)), file,row.names = FALSE)
    }
  )
  
  output$E2saveplot<- downloadHandler(
    filename = function() { 
      paste('Emi_Undercounting_Scores.', input$E2format, sep='') },
    content = function(file) {
      ffo <- input$E2format
      if(ffo=='pdf') {
        pdf(file,8,6)
      } else if(ffo=='png'){
        png(file,width=8*600,height=6*600,res=600)
      } else if(ffo=='tiff'){
        tiff(file,width=8*600,height=6*600,res=600,compression = 'rle')
      }  
      plot.BA(E2CBA(),input$E2t1, input$E2t2,
              input$E2t3, input$E2t4, input$E2year, input$E2plotCI, input$E2logscale, input$E2colorCI, input$E2hide,COLO)
      dev.off()
    }
  )
  
  output$E2table<-renderDT({
    reformatIE2tab(E2CBA(), input$E2hide, COLO)
  })
  
  output$E2miPlot <- renderPlot({
    plot.BA(E2CBA(),input$E2t1, input$E2t2,
            input$E2t3, input$E2t4, input$E2year, input$E2plotCI, input$E2logscale, input$E2colorCI, input$E2hide, COLO)
  })
  
  #########################3
  
  observeEvent(input$I3t2,  {
    updateSliderInput(session = session, inputId = "I3t1", max = input$I3t2)
  })
  
  observeEvent(input$I3t1,  {
    updateSliderInput(session = session, inputId = "I3t2", min = input$I3t1)
  })
  
  observeEvent(input$I3weightsreset, {
    updateSliderInput(session = session, inputId = "I3wimemb", value = wimemb)
    updateSliderInput(session = session, inputId = "I3wimema", value = wimema)
    updateSliderInput(session = session, inputId = "I3wmetab", value = wmetab)
    updateSliderInput(session = session, inputId = "I3wmetaa", value = wmetaa)
    updateSliderInput(session = session, inputId = "I3wmodelb", value = wmodelb)
    updateSliderInput(session = session, inputId = "I3wmodela", value = wmodela)
  })
  
  observeEvent(input$I3threshreset, {
    updateSliderInput(session = session, inputId = "I3t1", value = thr1)
    updateSliderInput(session = session, inputId = "I3t2", value = thr2)
  })
  
  I3tabre<-reactive(CalcCombineThreshols(ImmiMetaScores(), I2CBA(), 
                                         thr1=input$I3t1, thr2=input$I3t2, 
                                         wimema=input$I3wimema, 
                                         wimemb=input$I3wimemb, wmetaa=input$I3wmetaa, 
                                         wmetab=input$I3wmetab, wmodela=input$I3wmodela, 
                                         wmodelb=input$I3wmodelb, mirror=input$I3mirror,"I"))
  output$I3table<-renderDT({
    I3tabre()
  })
  
  I4tabre<-reactive(summaryTable(ImmiMetaScores(),I2CBA(), I3tabre(), 'I', COLO))
  
  output$I4table<-renderDT({
    I4tabre()
  })
  
  output$I3download<- downloadHandler(
    filename = function() { 
      paste('Immi_Combined_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(I3tabre()), file,row.names = FALSE)
    }
  )
  
  output$I4download<- downloadHandler(
    filename = function() { 
      paste('Immi_Summary_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(I4tabre()), file,row.names = FALSE)
    }
  )
  
  #########################3
  
  observeEvent(input$E3t2,  {
    updateSliderInput(session = session, inputId = "E3t1", max = input$E3t2)
  })
  
  observeEvent(input$E3t1,  {
    updateSliderInput(session = session, inputId = "E3t2", min = input$E3t1)
  })
  
  observeEvent(input$E3weightsreset, {
    updateSliderInput(session = session, inputId = "E3wimemb", value = wimemb)
    updateSliderInput(session = session, inputId = "E3wimema", value = wimema)
    updateSliderInput(session = session, inputId = "E3wmetab", value = wmetab)
    updateSliderInput(session = session, inputId = "E3wmetaa", value = wmetaa)
    updateSliderInput(session = session, inputId = "E3wmodelb", value = wmodelb)
    updateSliderInput(session = session, inputId = "E3wmodela", value = wmodela)
  })
  
  observeEvent(input$E3threshreset, {
    updateSliderInput(session = session, inputId = "E3t1", value = thr1)
    updateSliderInput(session = session, inputId = "E3t2", value = thr2)
  })
  
  E3tabre<-reactive(CalcCombineThreshols(EmiMetaScores(), E2CBA(), 
                                         thr1=input$E3t1, thr2=input$E3t2, 
                                         wimema=input$E3wimema, 
                                         wimemb=input$E3wimemb, wmetaa=input$E3wmetaa, 
                                         wmetab=input$E3wmetab, wmodela=input$E3wmodela, 
                                         wmodelb=input$E3wmodelb, mirror=input$E3mirror,"E"))
  output$E3table<-renderDT({
    E3tabre()
  })
  
  E4tabre<-reactive(summaryTable(EmiMetaScores(),E2CBA(), E3tabre(), 'E', COLO))
  
  output$E4table<-renderDT({
    E4tabre()
  })
  
  output$E3download<- downloadHandler(
    filename = function() { 
      paste('Emi_Combined_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(E3tabre()), file,row.names = FALSE)
    }
  )
  
  output$E4download<- downloadHandler(
    filename = function() { 
      paste('Emi_Summary_Scores', '.csv', sep='') },
    content = function(file) {
      write.csv(DT2DF(E4tabre()), file,row.names = FALSE)
    }
  )
  
  output$E2yearshow <- renderUI({
    yrange <- paste('(<b>B</b>) - before ',input$E2year,', (<b>A</b>) from ',input$E2year,' onwards',sep='')
    h5(HTML(yrange))
  })
  
  output$I2yearshow <- renderUI({
    yrange <- paste('(<b>B</b>) - before ',input$I2year,', (<b>A</b>) from ',input$I2year,' onwards',sep='')
    h5(HTML(yrange))
  })
  
  output$I2dynamicfigcaption <- renderUI({
    h4(HTML(paste('<b>Figure 3.</b> Median bilateral flows ratio of immigration data for years 1998 - ',
                  input$I2year-1,' (<b>B</b>) and ',input$I2year,' - 2019 (<b>A</b>). The ratio is calcualted by dividing flows from a country X to a group of good data quality countries (the <b>Reference group of countries</b>) reported by country X 
                                          by the same type of flow reported by the group of good data quality countries (the <b>Reference group of countries</b>). Bars shows the ratio, vertical thin lines show bootstrapped 95% 
                  interquantile confidence intervals, background colors reflect clasification based on the tresholds. 
                  If ratio is 1 there is no under- or over- counting. Ratios higher than 1 indicate overcounting, 
                  while ratios lower than 1 indicate undercounting of immigration flows. The lower the ratio value the higher the undercounting.
                  ',sep='')))
  })
  
  output$I2dynamictabcaption <- renderUI({
    h4(HTML(paste('<b>Table 3.</b> Classification of median bilateral flows ratio of immigration data for years 1998 - ',
                  input$I2year-1,' (<b>B</b>) and ',input$I2year,' - 2019 (<b>A</b>). The ratio is calcualted by dividing flows from a country X to a group of good data quality countries (the <b>Reference group of countries</b>) reported by country X 
                                          by the same type of flow reported by the group of good data quality countries (the <b>Reference group of countries</b>). <b>lo</b> and <b>hi</b> denotes the lower and upper bounds of bootstrapped 95% 
                  interquantile confidence intervals of estimated <b>median</b>s. Both <b>A</b> and <b>B</b> <b>median</b>s are classified into scores according to the <b>score classification thresholds</b> (left panel), 
                  <b>score num</b> is a numerical representation of the <b>score</b>. Empty records denotes missing bilateral data.
                  ',sep='')))
  })
  
  output$E2dynamictabcaption <- renderUI({
    h4(HTML(paste('<b>Table 4.</b> Classification of median bilateral flows ratio of emigration data for years 1998 - ',
                  input$E2year-1,' (<b>B</b>) and ',input$E2year,' - 2019 (<b>A</b>). The ratio is calcualted by dividing flows from a country X to a group of good data quality countries (the <b>Reference group of countries</b>) reported by country X 
                                          by the same type of flow reported by the group of good data quality countries (the <b>Reference group of countries</b>).<b>lo</b> and <b>hi</b> denotes the lower and upper bounds of bootstrapped 95% 
                  interquantile confidence intervals of estimated <b>median</b>s. Both <b>A</b> and <b>B</b> <b>median</b>s are classified into scores according to the <b>score classification thresholds</b> (left panel), 
                  <b>score num</b> is a numerical representation of the <b>score</b>. Empty records denotes missing bilateral data.
                  ',sep='')))
  })
  
  output$I3dynamictabcaption <- renderUI({
    h4(HTML(paste('<b>Table 5.</b> Classification of combined scores of immigration data for years 1998 - ',
                  input$I2year-1,' (<b>B</b>) and ',input$I2year,' - 2019 (<b>A</b>). <b>IMEM score num</b> is the IMEM undercount classification (Raymer et al. 2013) converted to numerical value, 
                  <b>metadata score num</b> is obtained from <b>Metadata classify (I)</b> page, <b>model score num (B)</b> obtained from <b>Model classify (I)</b> page. 
                  <b>combined score num</b>s are weighted means of these variables (see <b>Mixing weights</b> on the left panel). Both <b>A</b> and <b>B</b> <b>combined score num</b>s are classified into scores according to the <b>score classification thresholds</b> (left panel). 
                  ',sep='')))
  })
  
  output$I4dynamictabcaption <- renderUI({
    h4(HTML(paste('<b>Table 7.</b> Summary of obtained scores of immigration data for years 1998 - ',
                  input$I2year-1,' (<b>B</b>) and ',input$I2year,' - 2019 (<b>A</b>).',sep='')))
  })
  
  output$E4dynamictabcaption <- renderUI({
    h4(HTML(paste('<b>Table 8.</b> Summary of obtained scores of emigration data for years 1998 - ',
                  input$I2year-1,' (<b>B</b>) and ',input$I2year,' - 2019 (<b>A</b>).',sep='')))
  })
  
  output$E3dynamictabcaption <- renderUI({
    h4(HTML(paste('<b>Table 6.</b> Classification of combined scores of emigration data for years 1998 - ',
                  input$E2year-1,' (<b>B</b>) and ',input$E2year,' - 2019 (<b>A</b>). <b>IMEM score num</b> is the IMEM undercount classification (Raymer et al. 2013) converted to numerical value, 
                  <b>metadata score num</b> is obtained from <b>Metadata classify (I)</b> page, <b>model score num (B)</b> obtained from <b>Model classify (I)</b> page. 
                  <b>combined score num</b>s are weighted means of these variables (see <b>Mixing weights</b> on the left panel). Both <b>A</b> and <b>B</b> <b>combined score num</b>s are classified into scores according to the <b>score classification thresholds</b> (left panel). 
                  ',sep='')))
  })
  
  output$E2dynamicfigcaption <- renderUI({
    h4(HTML(paste('<b>Figure 4.</b> Median bilateral flows ratio of emigration data for years 1998 - ',
                  input$E2year-1,' (<b>B</b>) and ',input$E2year,' - 2019 (<b>A</b>). The ratio is calcualted by dividing flows from a country X to a group of good data quality countries (the <b>Reference group of countries</b>) reported by country X 
                                          by the same type of flow reported by the group of good data quality countries (the <b>Reference group of countries</b>). Bars shows the ratio, vertical thin lines show bootstrapped 95% 
                  interquantile confidence intervals, background colors reflect clasification based on the tresholds. 
                  If ratio is 1 there is no under- or over- counting. Ratios higher than 1 indicate overcounting, 
                  while ratios lower than 1 indicate undercounting of emigration flows. The lower the ratio value the higher the undercounting.',sep='')))
  })
  
  output$dynamicT1 <-renderUI({
    helper(h4('Score classification thresholds'),
           colour='#FF0000',type='inline',title='Score classification thresholds',buttonLabel = 'Close',
           content = c('The <b>score num</b> is classified into <b>score</b> as follows:','',
                       paste("<b>score num</b> from 0 to", input$Emimetat1,': <b>score</b> = <span style="color:#008000">Low</span>'),
                       paste("<b>score num</b> from ",input$Emimetat1,"to", input$Emimetat2,': <b>score</b> = <span style="color:#FFA500">Medium</span>'),
                       paste("<b>score num</b> from ",input$Emimetat2,'to 1 : <b>score</b> = <span style="color:#FF0000">High</span>'))
    )
  })
  
  output$dynamicTI2 <-renderUI({
    helper(h4('Score classification thresholds'),
           colour='#FF0000',type='inline',title='Score classification thresholds',buttonLabel = 'Close',
           content = c('Median bilateral flow ratios (<b>median (B)</b> and <b>median (A)</b>) are classified into <b>score</b>s (<b>(B)</b> and <b>(A)</b>) as follows:','',
                       paste("<b>median</b> from 0 to", input$I2t4,': <b>score</b> = <span style="color:',COLO[5],'">Very high</span>, <b> score num</b> = 1'),
                       paste("<b>median</b> from ",input$I2t4,"to", input$I2t3,': <b>score</b> = <span style="color:',COLO[4],'">High</span>, <b> score num</b> = 0.75'),
                       paste("<b>median</b> from ",input$I2t3,"to", input$I2t2,': <b>score</b> = <span style="color:',COLO[3],'">Medium</span>, <b> score num</b> = 0.5'),
                       paste("<b>median</b> from ",input$I2t2,"to", input$I2t1,': <b>score</b> = <span style="color:',COLO[2],'">Low</span>, <b> score num</b> = 0.25'),
                       paste("<b>median</b> from ",input$I2t1,'to 1 : <b>score</b> = <span style="color:',COLO[1],'">Very low</span>, <b> score num</b> = 0'))
           
    )
  })
  
  output$dynamicTE2 <-renderUI({
    helper(h4('Score classification thresholds'),
           colour='#FF0000',type='inline',title='Score classification thresholds',buttonLabel = 'Close',
           content = c('Median bilateral flow ratios (<b>median (B)</b> and <b>median (A)</b>) are classified into <b>score</b>s (<b>(B)</b> and <b>(A)</b>) as follows:','',
                       paste("<b>median</b> from 0 to", input$E2t4,': <b>score</b> = <span style="color:',COLO[5],'">Very high</span>, <b> score num</b> = 1'),
                       paste("<b>median</b> from ",input$E2t4,"to", input$E2t3,': <b>score</b> = <span style="color:',COLO[4],'">High</span>, <b> score num</b> = 0.75'),
                       paste("<b>median</b> from ",input$E2t3,"to", input$E2t2,': <b>score</b> = <span style="color:',COLO[3],'">Medium</span>, <b> score num</b> = 0.5'),
                       paste("<b>median</b> from ",input$E2t2,"to", input$E2t1,': <b>score</b> = <span style="color:',COLO[2],'">Low</span>, <b> score num</b> = 0.25'),
                       paste("<b>median</b> from ",input$E2t1,'to 1 : <b>score</b> = <span style="color:',COLO[1],'">Very low</span>, <b> score num</b> = 0'))
           
    )
  })
  
  output$dynamicTI3 <-renderUI({
    helper(h4('Score classification thresholds'),
           colour='#FF0000',type='inline',title='Score classification thresholds',buttonLabel = 'Close',
           content = c('The <b>combined  score num</b> is classified into <b>combined score</b> as follows:','',
                       paste("<b>combined score num</b> from 0 to", input$I3t1,': <b>combined score</b> = <span style="color:#008000">Low</span>'),
                       paste("<b>combined score num</b> from ",input$I3t1,"to", input$I3t2,': <b>combined score</b> = <span style="color:#FFA500">Medium</span>'),
                       paste("<b>combined score num</b> from ",input$I3t2,'to 1 : <b>combined score</b> = <span style="color:#FF0000">High</span>'))
    )
  })
  
  output$dynamicTE3 <-renderUI({
    helper(h4('Score classification thresholds'),
           colour='#FF0000',type='inline',title='Score classification thresholds',buttonLabel = 'Close',
           content = c('The <b>combined score num</b> is classified into <b>combined score</b> as follows:','',
                       paste("<b>combined score num</b> from 0 to", input$E3t1,': <b>combined score</b> = <span style="color:#008000">Low</span>'),
                       paste("<b>combined score num</b> from ",input$E3t1,"to", input$E3t2,': <b>combined score</b> = <span style="color:#FFA500">Medium</span>'),
                       paste("<b>combined score num</b> from ",input$E3t2,'to 1 : <b>combined score</b> = <span style="color:#FF0000">High</span>'))
    )
  })
  
}

colabout="#A9DFBF"
colimmi="#AED6F1"
colemi="#FAD7A0"
coltxt='black'
colsel='#873600'

shinyUI <- fluidPage( 
  titlePanel("UndercountMigScores",'UndercountMigScores'),
  fluidRow(
    
    column(width = 9,
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
                                       br(),
                                       h3('UndercountMigScores v0.4.8'),
                                       br(),
                                       h4('Combining Eurostat metadata undercounting migration scores and the scores based on bilateral flows ratio of Eurostat migration data'),
                                       br(),
                                       h4('Maciej J. Dańko'),
                                       h4(HTML('email: <a href="mailto:name@email.com"> danko@demogr.mpg.de </a>')),
                                       br(),
                                       h4('Max Planck Institute for Demographic Research'),
                                       h4('Rostock, Germany'),
                                       h4('2021'),
                                       br(),
                                       br(),
                                       h5('____________________________________________________________________________'),
                                       h5('The newest version of the app is always available on GitHub. To run it use this R code:'),
                                       h5(HTML('<span style="font-family: Courier New">shiny::runGitHub("MaciejDanko/UndercountMigScores", launch.browser = TRUE)</span>')),
                                       br(),
                                       h5('You may need to update/install some dependencies:'),
                                       h5(HTML('<span style="font-family: Courier New">install.packages(c("Cairo", "DT", "shiny", "colourpicker", <br>
                                               "shinyhelper", "magicaxis", "data.table", "countrycodes"))</span>'))
                                )
                       ),          
                       tabPanel(title = PanelNames[2],
                                br(), br(),
                                sidebarPanel(
                                  
                                  h4("Options"),
                                  helper(checkboxInput("nordicimmi", "Trust Nordic countries", value = TrustNordic),
                                         colour='#FF0000',type='inline',title='Score calculation procedure',buttonLabel = 'Close',
                                         content=c('No obligation of registration = <span style="color:#FF0000">High</span> undercounting, obligation of registration = <span style="color:#008000">Low</span> undercounting,
                                                                                                           but if <b>No limit</b> or <b>No sanctions</b> occur the score is changed to <span style="color:#FFA500">Medium</span>.',
                                                   '','The <span style="font-style:italic">Trust Nordic countries</span> option set <span style="color:#008000">Low</span> score for all Nordic countries ignoring the metadata.','',
                                                   'Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).')),       
                                  tags$hr(style="border-color: black;"),
                                  h4("References"),
                                  tags$body('Tab4a (page 20) of'),
                                  tags$a(href="http://quantmig.eu/res/files/QuantMig_Deliverable%206.2%20vf.pdf#page=21", "Jarl Mooyaart, Maciej J. Dańko, Rafael Costa, and Michaël Boissonneault (2021) Quality assessment of European migration data. Deliverable 6.2")
                                ),
                                mainPanel(
                                  h4(HTML('<b>Table 1.</b> Immigration undercounting related metadata and its classification.')),
                                  downloadButton("downloadIMData", "Download table"),
                                  br(),
                                  DTOutput('table1'),
                                  br(), br()
                                )
                       ),
                       tabPanel(title = PanelNames[3],
                                br(), br(),
                                sidebarPanel(
                                  helper(h4('Weights'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content=c('The <b>score num</b> is calcualted as a weighted mean which excludes all variables with "Unknown" records.')),
                                  sliderInput(inputId = "Emimetaw1", label = WeightsNam[1], min = 0, max = 1, value = MWt1, step=0.001),
                                  sliderInput(inputId = "Emimetaw2", label = WeightsNam[2], min = 0, max = 1, value = MWt2, step=0.001),
                                  sliderInput(inputId = "Emimetaw3", label = WeightsNam[3], min = 0, max = 1, value = MWt3, step=0.001),
                                  sliderInput(inputId = "Emimetaw4", label = WeightsNam[4], min = 0, max = 1, value = MWt4, step=0.001),
                                  actionButton("EMweightsreset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4("Options"),
                                  helper(checkboxInput("nordicemi", 'Trust Nordic countries', value = TrustNordic),
                                         colour='#FF0000',type='inline',title='Trust Nordic countries',buttonLabel = 'Close',
                                         content=c('The <span style="font-style:italic">Trust Nordic countries</span> option set <span style="color:#008000">Low</span> score for all Nordic countries ignoring the metadata.',
                                                   '','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).')),
                                  tags$hr(style="border-color: black;"),
                                  #h4('Score classification thresholds'),
                                  uiOutput(outputId = "dynamicT1"),
                                  sliderInput(inputId = "Emimetat1", label = "Low | Medium", min = 0, max = 1, value = MThr1, step=0.001),
                                  sliderInput(inputId = "Emimetat2", label = "Medium | High", min = 0, max = 1, value = MThr2, step=0.001),
                                  actionButton("EMthreshreset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4("References"),
                                  tags$body('Table 4b of'),
                                  tags$a(href="http://quantmig.eu/res/files/QuantMig_Deliverable%206.2%20vf.pdf#page=22", "Jarl Mooyaart, Maciej J. Dańko, Rafael Costa, and Michaël Boissonneault (2021) Quality assessment of European migration data. Deliverable 6.2"),
                                  br(),
                                  tags$body('Table 1.6 of'),
                                  tags$a(href="https://ec.europa.eu/eurostat/ramon/statmanuals/files/KS-CC-03-005-EN.pdf#page=22", "Eurostat (2003) Demographic statistics: Definitions and methods of collection in 31 European Countries. ISSN 1725-065X
                                  ISBN 92-894-6051-2.")
                                ),
                                mainPanel(
                                  h4(HTML('<b>Table 2.</b> Emigration undercounting related metadata and its classification.')),
                                  downloadButton("downloadEMData", "Download table"),
                                  br(),
                                  DTOutput('table2'),
                                  br(), br()
                                )
                       ),
                       tabPanel(title = PanelNames[4],
                                br(),br(),
                                sidebarPanel(
                                  helper(h4("Overview"),colour='#FF0000',type='markdown',title='Bilateral model description',buttonLabel = 'Close',
                                         content='BilateralModel'),       
                                  tags$p(HTML("This page <b>can only be used to view</b> the results of the bilateral flows ratio model. Any changes made here will not affect the final classification of undercounting score.")),
                                  tags$hr(style="border-color: black;"),
                                  
                                  checkboxGroupInput("Icountry", h4("Countries selection"), 
                                                     choices = Countries, selected = c('ES','BG','FI','SK','IT'), inline = TRUE),
                                  
                                  actionButton("Iall", "All"),actionButton("Inone", "None"),
                                  tags$hr(style="border-color: black;"),
                                  
                                  helper(radioButtons("Irefcountry", h4("Reference group of countries"),
                                                      choices = list("Nordic countries" = 1, "Nordic countries + CH + BE" = 2,
                                                                     "Nordic countries + CH + BE + AT + IE + NL" = 3, 
                                                                     'Nordic countries + CH + BE + AT + IE + NL + DE + FR' = 4,
                                                                     "All countries" = 5),selected = RefCntrSel),
                                         colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                         content=c('Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "Overview" for more information about the bilateral flows ratio model.')),
                                  
                                  # tags$hr(style="border-color: black;"),
                                  # radioButtons("IStats", h4("Select the type of the plot"),
                                  #              choices = list("Estimate + bootstrapped confidence intervals (∓ 1.96*SD)" = 1, "Bootstrapped median + 95% interquantiles" = 2), selected = 2),
                                  tags$hr(style="border-color: black;"),
                                  h4("IMEM correction"),
                                  helper(checkboxInput("Iraymer", "Use IMEM correction for the duration of stay", value = TRUE),
                                         colour='#FF0000',type='markdown',title="IMEM correction",buttonLabel = 'Close',
                                         content = c('RaymerCorrection')),
                                  tags$hr(style="border-color: black;"),
                                  h4('Graphical options'),
                                  #checkboxInput("Iextrapol", "Extrapolate missing values", value = FALSE),
                                  checkboxInput("Ilogscale", "Use log-scale", value = TRUE),
                                  checkboxInput("IplotCI", "Plot confidence intervals", value = TRUE),
                                ),
                                br(),br(),
                                mainPanel(
                                  plotOutput(outputId = "ImiPlot", height="600px"),
                                  br(),
                                  h4(HTML('<b>Figure 1.</b> Bilateral flows ratio for immigration data. The ratio is calcualted by dividing flows from a country X to a group of good data quality countries (the <b>Reference group of countries</b>) reported by country X 
                                          by the same type of flow reported by the group of good data quality countries (the <b>Reference group of countries</b>).')),
                                  div(style="display:inline-block;vertical-align:top;",
                                      h5('Choose a format and save the plot'),
                                      column(6,selectInput("Iformat", NULL, 
                                                           choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                      column(6,downloadButton("Isaveplot", "Save plot")))
                                )
                       ),
                       tabPanel(title = PanelNames[5],
                                br(),br(),
                                sidebarPanel(
                                  helper(h4("Overview"),colour='#FF0000',type='markdown',title='Bilateral model description',buttonLabel = 'Close',
                                         content='BilateralModel'),       
                                  tags$p(HTML("This page <b>can only be used to view</b> the results of the bilateral flows ratio model. Any changes made here will not affect the final classification of undercounting score.")),
                                  tags$hr(style="border-color: black;"),
                                  
                                  checkboxGroupInput("Ecountry", h4("Countries selection"), 
                                                     choices = Countries, selected = c('ES','BG','FI','SK','IT'), inline = TRUE),
                                  actionButton("Eall", "All"),actionButton("Enone", "None"),
                                  tags$hr(style="border-color: black;"),
                                  helper(radioButtons("Erefcountry", h4("Reference group of countries"),
                                                      choices = list("Nordic countries" = 1, "Nordic countries + CH + BE" = 2,
                                                                     "Nordic countries + CH + BE + AT + IE + NL" = 3, 
                                                                     'Nordic countries + CH + BE + AT + IE + NL + DE + FR' = 4,
                                                                     "All countries" = 5),selected = RefCntrSel),
                                         colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                         content=c('Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "Overview" for more information about the bilateral flows ratio model.')),
                                  tags$hr(style="border-color: black;"),
                                  # radioButtons("EStats", h4("Select the type of the plot"),
                                  #              choices = list("Estimate + bootstrapped confidence intervals (∓ 1.96*SD)" = 1, "Bootstrapped median + 95% interquantiles" = 2), selected = 2),
                                  # tags$hr(style="border-color: black;"),
                                  h4("IMEM correction"),
                                  helper(checkboxInput("Eraymer", "Use IMEM correction for the duration of stay", value = TRUE),
                                         colour='#FF0000',type='markdown',title="IMEM correction",buttonLabel = 'Close',
                                         content = c('RaymerCorrection')),
                                  tags$hr(style="border-color: black;"),
                                  h4('Graphical options'),
                                  #checkboxInput("Eextrapol", "Extrapolate missing values", value = FALSE),
                                  checkboxInput("Elogscale", "Use log-scale", value = TRUE),
                                  checkboxInput("EplotCI", "Plot confidence intervals", value = TRUE)
                                ),
                                br(),br(),
                                mainPanel(
                                  plotOutput(outputId = "EmiPlot", height="600px"),
                                  br(),
                                  h4(HTML('<b>Figure 2.</b> Bilateral flows ratio for Emigration data. The ratio is calcualted by dividing flows from a country X to a group of good data quality countries (the <b>Reference group of countries</b>) reported by country X 
                                          by the same type of flow reported by the group of good data quality countries (the <b>Reference group of countries</b>).')),
                                  div(style="display:inline-block;vertical-align:top;",
                                      h5('Choose a format and save the plot'),
                                      column(6,selectInput("Eformat", NULL, 
                                                           choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                      column(6,downloadButton("Esaveplot", "Save plot")))
                                )
                       ),
                       tabPanel(title = PanelNames[6],
                                
                                br(),br(),
                                sidebarPanel(
                                  helper(h4("Overview"),colour='#FF0000',type='markdown',title='Bilateral model description',buttonLabel = 'Close',
                                         content='BilateralModel'),       
                                  tags$p(HTML("This page can be used to set the model parameters. Any changes made here will affect the final classification of the undercounting score (<b>Combined scores (I)</b>).")),
                                  tags$hr(style="border-color: black;"),
                                  
                                  helper(radioButtons("I2refcountry", h4("Reference group of countries"),
                                                      choices = list("Nordic countries" = 1, "Nordic countries + CH + BE" = 2,
                                                                     "Nordic countries + CH + BE + AT + IE + NL" = 3, 
                                                                     'Nordic countries + CH + BE + AT + IE + NL + DE + FR' = 4,
                                                                     "All countries" = 5),selected = RefCntrSel),
                                         colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                         content=c('Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "Overview" for more information about the bilateral flows ratio model.')),
                                  tags$hr(style="border-color: black;"),
                                  helper(h4('Threshold year'),
                                         colour='#FF0000',type='inline',title='Threshold year',buttonLabel = 'Close',
                                         content=c('<b>Threshold year</b> groups bilateral flows ratios into two periods: before the treshold year (<b>B</b>) and from the threshold year onward (<b>A</b>).
                                                   <b>Threshold year</b> is identical for both immigration end emigration data.')),
                                  sliderInput(inputId = "I2year", label = NULL, min = 2000, max = 2016, value = 2008, step=1, sep=''),
                                  actionButton("I2yearreset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4("IMEM correction"),
                                  helper(checkboxInput("I2raymer", "Use IMEM correction for the duration of stay", value = TRUE),
                                         colour='#FF0000',type='markdown',title="IMEM correction",buttonLabel = 'Close',
                                         content = c('RaymerCorrection')),
                                  tags$hr(style="border-color: black;"),
                                  uiOutput(outputId = "dynamicTI2"),
                                  sliderInput(inputId = "I2t4", label = "Very high | High", min = 0, max = 1, value = round(BB[5],3), step=0.001), #thr4
                                  sliderInput(inputId = "I2t3", label = "High | Medium", min = 0, max = 1, value = round(BB[4],3), step=0.001), #thr3
                                  sliderInput(inputId = "I2t2", label = "Medium | Low", min = 0, max = 1, value = round(BB[3],3), step=0.001), #thr2
                                  sliderInput(inputId = "I2t1", label = "Low | Very low", min = 0, max = 1, value = round(BB[2],3), step=0.001), #thr1
                                  actionButton("I2treset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4('Graphical options'),
                                  checkboxInput("I2hide", "Hide countries with unknown bilateral flows", value = TRUE), #corrected
                                  checkboxInput("I2logscale", "Plot in log-scale", value = TRUE),
                                  checkboxInput("I2plotCI", "Plot confidence intervals", value = TRUE), #plotci
                                  div(
                                    column(7,h5('Confidence intervals color')),
                                    column(5,colourInput('I2colorCI',NULL,palette='limited'))
                                  ), br(), br(),
                                  
                                  
                                ),
                                br(),br(),
                                mainPanel(
                                  plotOutput(outputId = "I2miPlot", height="600px"),
                                  br(),
                                  uiOutput(outputId = "I2dynamicfigcaption"),
                                  div(style="display:inline-block;vertical-align:top;",
                                      
                                      h5('Choose a format and save the plot'),
                                      column(6,selectInput("I2format", NULL, 
                                                           choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                      column(6,downloadButton("I2saveplot", "Save plot"))),
                                  br(),
                                  tags$hr(style="border-color: black;"),
                                  uiOutput(outputId = "I2dynamictabcaption"),
                                  downloadButton("I2download", "Download table"),
                                  br(),
                                  DTOutput('I2table'),
                                  br(),br()
                                )
                       ),
                       tabPanel(title = PanelNames[7],
                                br(),br(),
                                sidebarPanel(
                                  helper(h4("Overview"),colour='#FF0000',type='markdown',title='Bilateral model description',buttonLabel = 'Close',
                                         content='BilateralModel'),       
                                  tags$p(HTML("This page can be used to set the model parameters. Any changes made here will affect the final classification of the undercounting score (<b>Combined scores (E)</b>).")),
                                  tags$hr(style="border-color: black;"),
                                  helper(radioButtons("E2refcountry", h4("Reference group of countries"),
                                                      choices = list("Nordic countries" = 1, "Nordic countries + CH + BE" = 2,
                                                                     "Nordic countries + CH + BE + AT + IE + NL" = 3, 
                                                                     'Nordic countries + CH + BE + AT + IE + NL + DE + FR' = 4,
                                                                     "All countries" = 5),selected = RefCntrSel),
                                         colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                         content=c('Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "Overview" for more information about the bilateral flows ratio model.')),
                                  tags$hr(style="border-color: black;"),
                                  helper(h4('Threshold year'),
                                         colour='#FF0000',type='inline',title='Threshold year',buttonLabel = 'Close',
                                         content=c('<b>Threshold year</b> groups bilateral flows ratios into two periods: before the treshold year (<b>B</b>) and from the threshold year onward (<b>A</b>).
                                                   <b>Threshold year</b> is identical for both immigration end emigration data.')),
                                  sliderInput(inputId = "E2year", label = NULL, min = 2000, max = 2016, value = 2008, step=1, sep=''),
                                  actionButton("E2yearreset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4("IMEM correction"),
                                  helper(checkboxInput("E2raymer", "Use IMEM correction for the duration of stay", value = TRUE),
                                         colour='#FF0000',type='markdown',title="IMEM correction",buttonLabel = 'Close',
                                         content = c('RaymerCorrection')),
                                  tags$hr(style="border-color: black;"),
                                  uiOutput(outputId = "dynamicTE2"),
                                  sliderInput(inputId = "E2t4", label = "Very high | High", min = 0, max = 1, value = round(BB[5],3), step=0.001), #thr4
                                  sliderInput(inputId = "E2t3", label = "High | Medium", min = 0, max = 1, value = round(BB[4],3), step=0.001), #thr3
                                  sliderInput(inputId = "E2t2", label = "Medium | Low", min = 0, max = 1, value = round(BB[3],3), step=0.001), #thr2
                                  sliderInput(inputId = "E2t1", label = "Low | Very low", min = 0, max = 1, value = round(BB[2],3), step=0.001), #thr1
                                  actionButton("E2treset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4('Graphical options'),
                                  checkboxInput("E2hide", "Hide countries with unknown bilateral flows", value = TRUE), #corrected
                                  checkboxInput("E2logscale", "Plot in log-scale", value = TRUE),
                                  checkboxInput("E2plotCI", "Plot confidence intervals", value = TRUE), #plotci
                                  div(
                                    column(7,h5('Confidence intervals color')),
                                    column(5,colourInput('E2colorCI',NULL,palette='limited'))
                                  ), br(), br(),
                                ),
                                br(),br(),
                                mainPanel(
                                  plotOutput(outputId = "E2miPlot", height="600px"),
                                  br(),
                                  uiOutput(outputId = "E2dynamicfigcaption"),
                                  div(style="display:inline-block;vertical-align:top;",     
                                      h5('Choose a format and save the plot'),
                                      column(6,selectInput("E2format", NULL, 
                                                           choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                      column(6,downloadButton("E2saveplot", "Save plot"))),
                                  br(),
                                  tags$hr(style="border-color: black;"),
                                  uiOutput(outputId = "E2dynamictabcaption"),
                                  downloadButton("E2download", "Download table"),
                                  br(),
                                  DTOutput('E2table'),
                                  br(),br()
                                )
                                
                       ),
                       tabPanel(title = PanelNames[8],
                                br(), br(),
                                sidebarPanel(
                                  helper(h4('Mixing weights'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of <b>score</b>s <b>num</b> obtained in previous pages'),
                                  uiOutput('I2yearshow'),
                                  helper(sliderInput(inputId = "I3wimemb", label = "IMEM score num (B)", min = 0, max = 1, value = wimemb, step=0.001),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('B')),
                                  helper(sliderInput(inputId = "I3wimema", label = "IMEM score num (A)", min = 0, max = 1, value = wimema, step=0.001),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('A')),
                                  helper(sliderInput(inputId = "I3wmetab", label = "Metadata score num (B)", min = 0, max = 1, value = wmetab, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content='Weight of the metadata <b>score num</b> obtained in <b>Metadata classify (I)</b> page used to calculate <b>combined score num (B)</b>'),
                                  helper(sliderInput(inputId = "I3wmetaa", label = "Metadata score num (A)", min = 0, max = 1, value = wmetaa, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content='Weight of the metadata <b>score num</b> obtained in <b>Metadata classify (I)</b> page used to calculate <b>combined score num (A)</b>'),
                                  helper(sliderInput(inputId = "I3wmodelb", label = "Model score num (B)", min = 0, max = 1, value = wmodelb, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content='Weight of the model <b>score num (B)</b> obtained in <b>Model classify (I)</b> page used to calculate <b>combined score num (A)</b>'),
                                  helper(sliderInput(inputId = "I3wmodela", label = "Model score num (A)", min = 0, max = 1, value = wmodela, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content='Weight of the model <b>score num (A)</b> obtained in <b>Model classify (I)</b> page used to calculate <b>combined score num (A)</b>'),
                                  actionButton("I3weightsreset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4('Options'),
                                  checkboxInput("I3mirror", HTML('Mirror extrapolation (fill missing values of model score num (B) using model score num (A) and vice versa). Interpolated values are shown in <span style="color:magenta;">magenta</span>.'), value = TRUE),
                                  tags$hr(style="border-color: black;"),
                                  uiOutput(outputId = "dynamicTI3"),
                                  sliderInput(inputId = "I3t1", label = "Low | Medium", min = 0, max = 1, value = thr1, step=0.001),
                                  sliderInput(inputId = "I3t2", label = "Medium | High", min = 0, max = 1, value = thr2, step=0.001),
                                  actionButton("I3threshreset", "Reset")
                                ),
                                mainPanel(
                                  uiOutput(outputId = "I3dynamictabcaption"),
                                  downloadButton("I3download", "Download table"),
                                  br(),
                                  DTOutput('I3table'),
                                  br(),br()
                                )
                       ),
                       tabPanel(title = PanelNames[9],
                                br(), br(),
                                sidebarPanel(
                                  helper(h4('Mixing weights'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of <b>score</b>s <b>num</b> obtained in previous pages'),
                                  uiOutput('E2yearshow'),
                                  helper(sliderInput(inputId = "E3wimemb", label = "IMEM score num (B)", min = 0, max = 1, value = wimemb, step=0.001),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('B')),
                                  helper(sliderInput(inputId = "E3wimema", label = "IMEM score num (A)", min = 0, max = 1, value = wimema, step=0.001),
                                         colour='#FF0000',type='inline',title='Integrated Modeling of European Migration (IMEM)',buttonLabel = 'Close',
                                         content=IMEMc('A')),
                                  helper(sliderInput(inputId = "E3wmetab", label = "Metadata score num (B)", min = 0, max = 1, value = wmetab, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content='Weight of the metadata <b>score num</b> obtained in <b>Metadata classify (E)</b> page used to calculate <b>combined score num (B)</b>'),
                                  helper(sliderInput(inputId = "E3wmetaa", label = "Metadata score num (A)", min = 0, max = 1, value = wmetaa, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content='Weight of the metadata <b>score num</b> obtained in <b>Metadata classify (E)</b> page used to calculate <b>combined score num (A)</b>'),
                                  helper(sliderInput(inputId = "E3wmodelb", label = "Model score num (B)", min = 0, max = 1, value = wmodelb, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content='Weight of the model <b>score num (B)</b> obtained in <b>Model classify (E)</b> page used to calculate <b>combined score num (A)</b>'),
                                  helper(sliderInput(inputId = "E3wmodela", label = "Model score num (A)", min = 0, max = 1, value = wmodela, step=0.001),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content='Weight of the model <b>score num (A)</b> obtained in <b>Model classify (E)</b> page used to calculate <b>combined score num (A)</b>'),
                                  actionButton("E3weightsreset", "Reset"),
                                  tags$hr(style="border-color: black;"),
                                  h4('Options'),
                                  checkboxInput("E3mirror", HTML('Mirror extrapolation (fill missing values of model score num (B) using model score num (A) and vice versa). Interpolated values are shown in <span style="color:magenta;">magenta</span>.'), value = TRUE),
                                  tags$hr(style="border-color: black;"),
                                  uiOutput(outputId = "dynamicTE3"),
                                  sliderInput(inputId = "E3t1", label = "Low | Medium", min = 0, max = 1, value = thr1, step=0.001),
                                  sliderInput(inputId = "E3t2", label = "Medium | High", min = 0, max = 1, value = thr2, step=0.001),
                                  actionButton("E3threshreset", "Reset")
                                ),
                                mainPanel(
                                  uiOutput(outputId = "E3dynamictabcaption"),
                                  downloadButton("E3download", "Download table"),
                                  br(),
                                  DTOutput('E3table'),
                                  br(),br()
                                )
                       ),
                       tabPanel(title = PanelNames[10],
                                br(), br(),
                                uiOutput(outputId = "I4dynamictabcaption"),      
                                downloadButton("I4download", "Download table"),
                                br(),
                                column(8, align="center", DTOutput('I4table')),
                                br(),br()
                       ),
                       tabPanel(title = PanelNames[11],
                                br(), br(),
                                uiOutput(outputId = "E4dynamictabcaption"),      
                                downloadButton("E4download", "Download table"),
                                br(),
                                column(8, align="center", DTOutput('E4table')),
                                br(),br()
                       )
           )
    )
  )
)

shinyApp(ui=shinyUI, server = shinyServer)


