rm(list=ls())

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
#remotes::install_github("rstudio/bslib")
library(bslib)

options(bitmapType="cairo")
source('./code/UNDERCOUNTING_PKG_2023_APP.R')


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
  legend('left',legend=c('IMEM / QuantMig','Metadata','Model'),bty='o',
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
#mypie2(0.1,0.2,0.52,0.2,0.85)
# d=1:3
# mypie(0.1,0.2,0.52,0.2,0.2,0.52,0.85)
# text(1.5,1,eval(substitute(expression(bold(d)),list(d='5'))),cex=2)
# text(1.5,1,expression(bold("5")),cex=2)

thr1 <- 0.25
thr2 <- 0.5

# wimema <- 0.25
# wimemb <- 0.3
# wmetaa <- 0.15
# wmetab <- 0.15
# wmodela <- 1
# wmodelb <- 1
#
# 100*wimema/(wimema+wmetaa+wmodela) # 15%
# 100*wmetaa/(wimema+wmetaa+wmodela) # 10%
# 100*wmodela/(wimema+wmetaa+wmodela) # 75%
#
# 100*wimemb/(wimemb+wmetab+wmodelb) # 20%
# 100*wmetab/(wimema+wmetab+wmodelb) # 10%
# 100*wmodelb/(wimema+wmetab+wmodelb) # 70%

wimema <- 0.2
wimemb <- 0.2
wmetaa <- 0.1
wmetab <- 0.1
wmodela <- 1 - wimema - wmetaa
wmodelb <- 1 - wimemb - wmetab


#mirror <- TRUE

RefCntrSel <- 9

Step <-0.005

MWt1 <- 0.5
MWt2 <- 0.1
MWt3 <- 0.1
MWt4 <- 0.3

# MWt1/(MWt1+MWt2+MWt3+MWt4)
# MWt2/(MWt1+MWt2+MWt3+MWt4)
# MWt3/(MWt1+MWt2+MWt3+MWt4)
# MWt4/(MWt1+MWt2+MWt3+MWt4)
#
# MWt1 <- 0.4
# MWt2 <- 0.2
# MWt3 <- 0.2
# MWt4 <- 0.2


MThr1 <- 0.25
MThr2 <- 0.75
TrustNordic<-TRUE

Meta_DeReg<-Recalc_Meta_DeReg(Meta_DeReg,MWt1,MWt2,MWt3,MWt4,MThr1,MThr2,TrustNordic)
Meta_Reg<-Recalc_Meta_Reg(Meta_Reg, TrustNordic)

COLO=c("#00DD00",'#008000','#FFA500','#FF0000','#800000')

WeightsNam<-paste(c('Obligation of de-registration','Obligation of de-registration of third country nationals','Monitoring third country nationals','Administrative corrections'),sep='')
CountriesL<-paste(countrycode::countrycode(toeurostat(Countries),'eurostat','country.name'),' (',Countries,')',sep='')
Countries<-as.list(Countries)
names(Countries)<-CountriesL
PanelNames<-c('About','Immigration metadata & expertize','Emigration metadata & expertize','Immigration model','Emigration model',
              'X','X','Combined immigration scores','Combined emigration scores','X','X', 'Help')

IMEMc<-function(k) c('The parameter adds a weight to IMEM (<a href="https://www.imem.cpc.ac.uk/About.aspx">Integrated Modeling of European Migration</a>) undercount classification converted to numerical value (<b>IMEM score num</b>), where
         0 denotes <span style="color:#008000">Low</span> undercounting and 1 denotes <span style="color:#FF0000">High</span> undercounting. See also metadata and expert opinion tabs.','',
                     paste('Weighted <b>IMEM score num</b> is used to calculate <b>combined score num (',k,')</b>',sep=''),'',
                     '<b>References</b>','<a href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20">Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F. and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819.</a>')

QUANTMIGc<-function(k) c('The parameter adds a weight to QuantMig (<a href="http://quantmig.eu/">Quantifying Migration Scenarios for Better Policy.</a>) undercount classification converted to numerical value (<b>QuantMig score num</b>), where
         0 denotes <span style="color:#008000">Excellent</span> (lack of) undercounting and 1 denotes <span style="color:#FF0000">High</span> undercounting. See also metadata and expert opinion tabs.','',
                         paste('Weighted <b>QuantMig score num</b> is used to calculate <b>combined score num (',k,')</b>',sep=''),'',
                         '<b>References</b>','<a href="http://quantmig.eu/res/files/QuantMig%20deliverable_6_3.pdf#page=11">Aristotelous, G., Smith, P.W.F., and Bijak, J. (2022). Technical report: Estimation methodology. QuantMig Project Deliverable 6.3. The Hague: Netherlands Interdisciplinary, Demographic Institute (NIDI-KNAW)/University of Groningen.</a>')


METAwtxt<-'Weight for the metadata <b>score num</b> obtained in <b>Immigration metadata</b> panel.'
MODELwtxt <- function(k='immigration') paste0('Weight for the <b>model score num</b> obtained in <b>',k,' model</b> tab used to calculate <b>Mean weighted scores</b> and <b>Classification of undercaunting</b> in this tab.')
ThreshTxt <- '<b> Threshold year </b> allows you to set two different sets of weights for two separate time periods. It also separates the effect of IMEM and QuantMig expert opinion scores.'
WeightsMixTxt <- 'Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous tabs).'
MarkNoDataTxt<-'Selecting this option marks cases where the calculation of bilateral flow ratios was impossible due to the lack of flows in all reference country or in a country in question.'

version<-'0.8.1'
DOI<-'10.5281/zenodo.6612951'
BADGE<-paste0('<a href="https://doi.org/',DOI,'"><img src="https://zenodo.org/badge/DOI/',DOI,'.svg" alt="DOI"></a>')

#IniCntrSel<-c('ES','BG','FI','SK','IT')
IniCntrSel<-c('AT','BG','FI','SK','IT','PL')

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
    updateSliderInput(session = session, inputId = "E3wimema", value = input$E3wimema/SA)
    updateSliderInput(session = session, inputId = "E3wmetaa", value = input$E3wmetaa/SA)
    updateSliderInput(session = session, inputId = "E3wmodela", value = input$E3wmodela/SA)
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

  observeEvent(input$Iall,{
    updateCheckboxGroupInput(session = session, inputId = "Icountry", selected = CountriesS )
  })

  observeEvent(input$Inone,{
    updateCheckboxGroupInput(session = session, inputId = "Icountry", selected = '' )
  })

  observeEvent(input$Eall,{
    updateCheckboxGroupInput(session = session, inputId = "Ecountry", selected = CountriesS )
  })

  observeEvent(input$Enone,{
    updateCheckboxGroupInput(session = session, inputId = "Ecountry", selected = '' )
  })

  ######################

  observeEvent(input$I3weightsresetb, {
    updateSliderInput(session = session, inputId = "I3wimemb", value = wimemb)
    updateSliderInput(session = session, inputId = "I3wmetab", value = wmetab)
    updateSliderInput(session = session, inputId = "I3wmodelb", value = wmodelb)
  })

  observeEvent(input$I3weightsreseta, {
    updateSliderInput(session = session, inputId = "I3wimema", value = wimema)
    updateSliderInput(session = session, inputId = "I3wmetaa", value = wmetaa)
    updateSliderInput(session = session, inputId = "I3wmodela", value = wmodela)
  })


  observeEvent(input$I3threshreset, {
    updateSliderInput(session = session, inputId = "I3t1", value = thr1)
    updateSliderInput(session = session, inputId = "I3t2", value = thr2)
  })


  observeEvent(input$E3weightsreseta, {
    updateSliderInput(session = session, inputId = "E3wimema", value = wimema)
    updateSliderInput(session = session, inputId = "E3wmetaa", value = wmetaa)
    updateSliderInput(session = session, inputId = "E3wmodela", value = wmodela)
  })

  observeEvent(input$E3weightsresetb, {
    updateSliderInput(session = session, inputId = "E3wimemb", value = wimemb)
    updateSliderInput(session = session, inputId = "E3wmetab", value = wmetab)
    updateSliderInput(session = session, inputId = "E3wmodelb", value = wmodelb)
  })

  observeEvent(input$E3threshreset, {
    updateSliderInput(session = session, inputId = "E3t1", value = thr1)
    updateSliderInput(session = session, inputId = "E3t2", value = thr2)
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

  BILRATIOSI<-reactive({
    get_ui_result('I',
                   refcountry=input$Irefcountry,
                   raymer=input$Iraymer,
                   additive = input$Iadditive,
                   separated = input$Iseparated,
                   ncp=input$Incp
    )
  })

  BILRATIOSE<-reactive({
    get_ui_result('E',
                  refcountry=input$Erefcountry,
                  raymer=input$Eraymer,
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
              UseQuantiles = input$IUseQuantiles,
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
              UseQuantiles = input$EUseQuantiles,
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
    striped=TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  output$Elogratios <- renderTable({
    getModel(RESE())$logindex},
    bordered = TRUE,
    rownames = TRUE,
    striped= TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )


  output$IlogratiosThresholds <- renderTable({
    getModel(RESI())$logindexthresholds},
    bordered = TRUE,
    rownames = TRUE,
    striped = TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  DURI<-reactive({getDuration('I',input$IdurationCountries)})

  output$Iduration <- renderTable({
    DURI()},
    digits=0,
    #hover=TRUE,
    striped=TRUE,
    bordered = TRUE,
    rownames = TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  DURE<-reactive({getDuration('E',input$EdurationCountries)})

  output$Eduration <- renderTable({
    DURE()},
    digits=0,
    #hover=TRUE,
    striped=TRUE,
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
    Striped= TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  output$IscoresThresholds <- renderTable({
    getCombined(RESI())$rawthresholds},
    bordered = TRUE,
    rownames = TRUE,
    striped=TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  output$EscoresThresholds <- renderTable({
    getCombined(RESE())$rawthresholds},
    bordered = TRUE,
    rownames = TRUE,
    striped=TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  output$Iscores <- renderTable({
    getCombined(RESI())$raw},
    bordered = TRUE,
    rownames = TRUE,
    striped=TRUE,
    spacing = 'xs',
    width = '100%',
    align = 'c'
  )

  output$Escores <- renderTable({
    getCombined(RESE())$raw},
    bordered = TRUE,
    rownames = TRUE,
    striped=TRUE,
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

  output$IsaveBFR<- downloadHandler(
    filename = function() {
      paste('Immi_Bilateral_Flows_Ratios', '.xlsx', sep='') },
    content = function(filenamer) {
      #print(filenamer)
      saveBFR(filenamer, BILRATIOSI() )
    }
  )
  
  output$EsaveBFR<- downloadHandler(
    filename = function() {
      paste('Emi_Bilateral_Flows_Ratios', '.xlsx', sep='') },
    content = function(filenamer) {
      #print(filenamer)
      saveBFR(filenamer, BILRATIOSE() )
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
    },
  )

  output$img <- renderUI({
    tags$img(src = "https://github.com/MaciejDanko/UndercountMigScores/blob/master/www/qrcode.png?raw=true",width='100px')
  })
  
}

colabout="#A9DFBF"
colimmi="#AED6F1"
colemi="#FAD7A0"
coltxt='black'
colsel='#873600'

#shinyUI <- fluidPage(
shinyUI <-  bootstrapPage(
  tags$head(tags$style("body {min-width:100%; max-width: 100%}", media="screen", type="text/css")),
  theme = bs_theme(version = 3),
  titlePanel(HTML('<span style="color:#000070;font-family:Serif,Georgia,Serif"><b>UndercountMigScores</b></span>'),'UndercountMigScores'),
  fluidRow(style='max-width:1800px; min-width:1300px',
    column(width = 12,
           tags$head(tags$style("h3 {margin-top:0px;}", media="screen", type="text/css")),
           tags$head(tags$style("h4 {margin-top:0px;}", media="screen", type="text/css")),
           tags$head(tags$style("img {border:1px; border-color: #D5D5D5; border-style: solid;}", media="screen", type="text/css")),
           #tags$head(tags$style("tabbable {max-width:1800px; min-width:1100px}", media="screen", type="text/css")),
           #tags$head(tags$style("nav {max-width:1800px; min-width:1100px}", media="screen", type="text/css")),
           
           #tags$head(tags$style(".well {border:2px; border-color: #D5D5D5; border-style: solid; 
          #                      padding: 3px; background-color: #F5F5F5; margin:5px}", media="screen", type="text/css")), #margin-left: 10px; margin-bottom: 10px

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
                       
                       tabPanel(title = PanelNames[1], style='max-width:1800px',
                                column(12,offset=0, align="center",
                                       br(),
                                       h3(HTML(BADGE)),
                                       h3(HTML(paste0('<b>UndercountMigScores v',version,'</b>'))),
                                       h4(HTML('<a href="https://maciej-jan-danko.shinyapps.io/undercountmigscores/"> https://maciej-jan-danko.shinyapps.io/undercountmigscores/ </a>')),
                                       br(),
                                       h4('Assessing the Level of Undercounting in the InternationalMigration Flows Reported by Eurostat and Other Data Sources'),
                                       br(),
                                       h4('Maciej J. Dańko'),
                                       h4(HTML('email: <a href="mailto:name@email.com"> danko@demogr.mpg.de </a>')),
                                       br(),
                                       h4('Max Planck Institute for Demographic Research'),
                                       h4('Rostock, Germany'),
                                       #h4('2021-2022'),
                                       #img(src = "https://github.com/MaciejDanko/UndercountMigScores/blob/master/www/qrcode.png"),
                                      
                                       
                                       h5('____________________________________________________________________________'),
                                       h4('How to cite this software?'),
                                       h5(HTML(paste0('Maciej J. Dańko. UndercountMigScores ',version,'. (2023)<br>
                                               Assessing the Level of Undercounting in the InternationalMigration Flows Reported by Eurostat and Other Data Sources.
                                               <br>DOI: ',DOI,'. URL:https://github.com/MaciejDanko/UndercountMigScore'))),
                                       downloadButton("downloadBIB", "Download citation in .bib format"),
                                       h5('____________________________________________________________________________'),
                                       h5(HTML('The newest version of the app is always available on GitHub. To run it use this R code:<br><span style="font-family: Courier New">shiny::runGitHub("MaciejDanko/UndercountMigScores", launch.browser = TRUE)</span><br>')),
                                       h5(HTML('You may need to update/install some dependencies:<br><span style="font-family: Courier New">install.packages("usethis", "shiny", "Cairo", "openxlsx", "countrycodes", "data.table", <br> "DT", "magicaxis", "shinyWidgets", "RColorBrewer", "shinyhelper"); remotes::install_github("rstudio/bslib")</span><br>')),
                                       h5(HTML('If equations do not display correctly you may need to (re-)install mathjax on your computer<br>
                                       Linux: <span style="font-family: Courier New">sudo apt-get install -y libjs-mathjax</span>,<br>Windows/Mac/Linux: <a href="https://sourceforge.net/projects/mathjax/"> https://sourceforge.net/projects/mathjax/</a>')),
                                       h5('____________________________________________________________________________'),
                                       uiOutput('img'),
                                       br(),br(),br(),br()
                                       
                                )
                       ),
                       tabPanel(title = PanelNames[2],
                                br(), #br(),
                                #sidebarPanel(fluid=FALSE, width=4,
                                div(class="row", style='margin-left:0px;',# max-width: 1600px',#style='margin:0px; padding:0px',
                                    div(class="col-lg-6", style='padding-right:0px; max-width:960px; min-width:200px',
                                        div(class='well', style="height:100px; margin-bottom:15px;",
                                # div(class="row", style='margin-left:0px',  #style='margin:0px; padding:0px',
                                #     div(class="col-lg-6", style='padding-right:0px; max-width:800px; min-width:200px',
                                #         div(class='well', style='height:100px; margin-bottom:15px;',
                                             h3("Missing metadata options"),
                                             helper(checkboxInput("nordicimmi", "Trust Nordic countries", value = TrustNordic),
                                                    colour='#FF0000',type='inline',title='Score calculation procedure',buttonLabel = 'Close',
                                                    content=c('No obligation of registration = <span style="color:#FF0000">High</span> undercounting, obligation of registration = <span style="color:#008000">Low</span> undercounting,
                                                                                                           but if <b>No limit</b> or <b>No sanctions</b> occur the score is changed to <span style="color:#FFA500">Medium</span>.',
                                                              '','The <b>Trust Nordic countries</b> option set <span style="color:#008000">Low</span> score for all Nordic countries ignoring the metadata.','',
                                                              'Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).')),
                                       
                                        )),
                                    div(class="col-lg-6",style='max-width:960px; min-width:200px; padding-right:0px', #style='padding-left:0px', 
                                        div(class='well', style="height:100px; margin-bottom:15px;",
                                    # div(class="col-lg-6", style='max-width:800px; min-width:200px; padding-right:0px',
                                    #     div(class='well', style='height:270px; margin-bottom:15px;',
                                    #                 
                                                    
                                #sidebarPanel(width=8,
                                             #div(id='ZZD',
                                                 h3(HTML('Immigration undercounting related metadata, expert opinions, and their classification.'))
                                             #),
                                             #tags$head(tags$style("#ZZD h3 {margin-bottom: 19px;}", media="screen", type="text/css")),
                                )),
                                div(class="col-lg-6", style='max-width:960px; min-width:200px; padding-right:0px'), #normalize above output
                                ),
                                
                                #sidebarPanel(width=12,
                                
                                div(class="row", style='margin-left:0px',  
                                    div(class="col-lg-12",style='padding-right:0px; margin-right:0px;',#style='max-width:800px', 
                                        div(class='well', style='margin-bottom:15px;',
      
                                             downloadButton("downloadIMData", "Download table"),
                                             br(),
                                             DTOutput('table1',width='100%'), 
                                             
                                             # DETECT ANDROID and THEN:
                                            #div(class="col-lg-12",#style='max-width:800px', 
                                            #DTOutput('table1',width='50%')
                                             
                                             tags$head(tags$style("#table1 table th {background-color: #CCBBFF; border-width:1px;}", media="screen", type="text/css")),

                                             br(), br()
                                ))),
                                #sidebarPanel(width=12,
                                div(class="row", style='margin-left:0px', 
                                    div(class="col-lg-12",style='padding-right:0px', 
                                        div(class='well', style='margin-bottom:15px;',
                                             h3("References"),
                                             tags$body('Tab4a (page 20) of'),
                                             tags$a(href="http://quantmig.eu/res/files/QuantMig_Deliverable%206.2%20vf.pdf#page=21", "Jarl Mooyaart, Maciej J. Dańko, Rafael Costa, and Michaël Boissonneault (2021) Quality assessment of European migration data. QuantMig Project Deliverable 6.2. The Hague: Netherlands Interdisciplinary, Demographic Institute (NIDI-KNAW)/University of Groningen."),
                                             br(),br(),
                                             tags$a(href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20","Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F., and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819."),
                                             br(),br(),
                                             tags$body('Table 2 of'),
                                             tags$a(href="http://quantmig.eu/res/files/QuantMig%20deliverable_6_3.pdf#page=11","Aristotelous, G., Smith, P.W.F., and Bijak, J. (2022). Technical report: Estimation methodology. QuantMig Project Deliverable 6.3. The Hague: Netherlands Interdisciplinary, Demographic Institute (NIDI-KNAW)/University of Groningen."),
                                             br(),br()
                                ))),
                       ),
                       tabPanel(title = PanelNames[3],
                                br(), #br(),
                                #sidebarPanel(fluid=FALSE,width=6,
                                div(class="row", style='margin-left:0px',  #style='margin:0px; padding:0px',
                                    div(class="col-lg-6", style='padding-right:0px; max-width:960px; min-width:200px',
                                        div(class='well', style='height:600px; margin-bottom:15px;',
                                             helper(h3('Metadata weights'),sise='s',
                                                    colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                                    content=c('The <b>score num</b> is calculated as a weighted mean which excludes all variables with "Unknown" records.')),
                                             sliderInput(inputId = "Emimetaw1", label = WeightsNam[1], min = 0, max = 1, value = MWt1, step=Step),
                                             sliderInput(inputId = "Emimetaw2", label = WeightsNam[2], min = 0, max = 1, value = MWt2, step=Step),
                                             sliderInput(inputId = "Emimetaw3", label = WeightsNam[3], min = 0, max = 1, value = MWt3, step=Step),
                                             sliderInput(inputId = "Emimetaw4", label = WeightsNam[4], min = 0, max = 1, value = MWt4, step=Step),
                                             helper(tags$span(' '),sise='s',
                                                    colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                                    content='<b>Reset</b> - restores default values, <b>&#8721 weights = 1</b> makes all weights sum up to 1.'),

                                             actionButton("EMweightsreset", "Reset"),
                                             actionButton("EMrecalc", HTML("&#8721 weights = 1")),
                                             tags$hr(style="border-color: black;"),

                                             h3("Missing metadata options"),
                                             div(id='ZZ2',helper(checkboxInput("nordicemi", 'Trust Nordic countries', value = TrustNordic),
                                                                 colour='#FF0000',type='inline',title='Trust Nordic countries',buttonLabel = 'Close',
                                                                 content=c('The <b>Trust Nordic countries</b> option set <span style="color:#008000">Low</span> score for all Nordic countries ignoring the metadata.',
                                                                           '','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).'))),
                                             tags$head(tags$style("#ZZ2 .checkbox {margin-bottom: 15px;}", media="screen", type="text/css")),

                                )),
                                #sidebarPanel(width=6,
                                #div(class="row", #style='margin:0px; padding:0px',
                                    div(class="col-lg-6", style='max-width:960px; min-width:200px; padding-right:0px',
                                        div(class='well', style='height:270px; margin-bottom:15px;',
                                             h3('Normalized weights'),
                                             plotOutput('EMPlot', height='200', width='100%'),
                                             br()
                                )),
                                #sidebarPanel(width=6,
                                #div(class="row", #style='margin:0px; padding:0px',
                                    div(class="col-lg-6", style='max-width:960px; min-width:200px; padding-right:0px', 
                                        div(class='well', style='height:315px; margin-bottom:15px;',
                                             h3('Metadata classification thresholds'),
                                             #tags$hr(style="border-color: black;"),
                                             uiOutput(outputId = "dynamicT1"),
                                             sliderInput(inputId = "Emimetat1", label = "Low | Medium", min = 0, max = 1, value = MThr1, step=Step),
                                             sliderInput(inputId = "Emimetat2", label = "Medium | High", min = 0, max = 1, value = MThr2, step=Step),
                                             actionButton("EMthreshreset", "Reset"),
                                             tags$head(tags$style("#EMthreshreset {margin-bottom: 9px;}", media="screen", type="text/css")),
                                ))),
                                #sidebarPanel(width=12,
                                div(class="row", style='margin-left:0px',
                                    div(class="col-lg-12",style='padding-right:0px; margin-right:0px', #style='max-width:800px',
                                        div(class='well', style='margin-bottom:15px;',
                                             h3(HTML('Emigration undercounting related metadata, expert opinions, and their classification.')),

                                             downloadButton("downloadEMData", "Download table"),
                                             br(),
                                             DTOutput('table2'),
                                             tags$head(tags$style("#table2 table th {background-color: #CCBBFF; border-width:1px;}", media="screen", type="text/css")),

                                             br(), br()
                                ))),
                                #sidebarPanel(width=12,
                                div(class="row", style='margin-left:0px',
                                    div(class="col-lg-12",style='padding-right:0px; margin-right:0px',#", #style='max-width:800px',
                                        div(class='well', style='margin-bottom:15px;',
                                             h3("References"),
                                             tags$body('Table 4b of'),
                                             tags$a(href="http://quantmig.eu/res/files/QuantMig_Deliverable%206.2%20vf.pdf#page=22", "Jarl Mooyaart, Maciej J. Dańko, Rafael Costa, and Michaël Boissonneault (2021) Quality assessment of European migration data. QuantMig Project Deliverable 6.2. The Hague: Netherlands Interdisciplinary, Demographic Institute (NIDI-KNAW)/University of Groningen."),
                                             br(), br(),
                                             tags$body('Table 1.6 of'),
                                             tags$a(href="https://ec.europa.eu/eurostat/ramon/statmanuals/files/KS-CC-03-005-EN.pdf#page=22", "Eurostat (2003) Demographic statistics: Definitions and methods of collection in 31 European Countries. ISSN 1725-065X
                                  ISBN 92-894-6051-2."),
                                             br(),br(),
                                             tags$a(href="https://www.tandfonline.com/doi/abs/10.1080/01621459.2013.789435?journalCode=uasa20","Raymer, J., Wiśniowski, A., Forster, J. J., Smith, P. W. F., and Bijak, J. (2013), ‘Integrated Modeling of European Migration’, Journal of the American Statistical Association 108(503), 801–819."),
                                            br(),br(),
                                             tags$body('Table 2 of'),
                                             tags$a(href="http://quantmig.eu/res/files/QuantMig%20deliverable_6_3.pdf#page=11","Aristotelous, G., Smith, P.W.F., and Bijak, J. (2022). Technical report: Estimation methodology. QuantMig Project Deliverable 6.3. The Hague: Netherlands Interdisciplinary, Demographic Institute (NIDI-KNAW)/University of Groningen."),
                                             br(),br()
                                )))
                       ),
                       tabPanel(title = PanelNames[4],
                                br(),
                                div(class="row", style='margin-left:0px',#style='margin:0px; padding:0px',
                                    div(class="col-lg-4", style='padding-right:0px; max-width:960px; min-width:410px',
                                        div(class='well', style="margin-bottom:15px;",
                                            
                                  helper(h3("General model options"),colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                         content='BilateralModel',size='s'),

                                  tags$hr(style="border-color: black;"),

                                  helper(h4("Duration of stay correction"),sise='s',
                                         colour='#FF0000',type='markdown',title="",buttonLabel = 'Close',
                                         content = c('BilateralModel')),

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
                                              selected = 13),

                                  tableOutput('corrItab'),
                                  tags$head(tags$style("#corrItab table {background-color: white; }", media="screen", type="text/css")),
                                  tags$head(tags$style("#corrItab table th {background-color: #CCBBFF; }", media="screen", type="text/css")),

                                  conditionalPanel(condition = "input.Iraymer > 4",
                                                   checkboxInput("Iseparated", "Use duration corrrection parameters calculated separately for immigration", value = FALSE),
                                                   checkboxInput("Iadditive", "Use additive optimization criteria (otherwise multiplicative)", value = TRUE),
                                  ),
                                                   tags$hr(style="border-color: black;"),
                                                   helper(selectInput("Irefcountry", h4("Reference group of countries"),
                                                                      choices = list('Nordic countries (without IS)'=1,'Nordic countries'=2,'Nordic countries+BE'=3,'Nordic countries+CH'=4,'Nordic countries+NL'=5,
                                                                                     'Nordic countries+BE+CH'=6,'Nordic countries+BE+NL'=7,'Nordic countries+CH+NL'=8,
                                                                                     'Nordic countries+BE+CH+NL'=9,'Nordic countries+AT+BE+CH+NL'=10,
                                                                                     'Nordic countries+AT+BE+CH+DE+NL'=11,'Nordic countries+AT+BE+CH+DE+FR+NL'=12,
                                                                                     'Nordic countries+AT+BE+CH+DE+FR+IE+NL'=13,'Nordic countries+AT+BE+CH+DE+FR+IE+NL+UK'=14,'All countries'=15),
                                                                      selected = RefCntrSel),
                                                          colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                                          content=c('Please set the "Duration of stay correction" first before setting this parameter.','','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "General model options" for more information about the bilateral flows ratio model.')),


                                  tags$hr(style="border-color: black;"),
                                  h4('Imputations of missing values'),
                                  helper(checkboxInput("Iimputations", "Use PCA imputations", value = TRUE),
                                         colour='#FF0000',type='inline',title='PCA imputations',buttonLabel = 'Close',
                                         content=c('Impute the missing values of the flows ratios matrix with the Principal Components Analysis model using missMDA package.')),
                                  conditionalPanel(condition = "input.Iimputations == true",
                                                   helper(sliderInput(inputId = "Incp", label = 'ncp parameter', min = 1, max = 5, value = 1, step=1, sep=''),
                                                          colour='#FF0000',type='inline',title='ncp parameter',buttonLabel = 'Close',
                                                          content='From imputePCA {missMDA package}: "integer corresponding to the number of components used to to predict the missing entries".')),
                                )), #)
                                #sidebarPanel(width=8,
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                             h3(HTML('Classification options')),
                                             tags$hr(style="border-color: black;"),
                                             column(width=5,
                                                    helper(checkboxInput("IUseQuantiles", "Classification based on quantiles", value = TRUE),
                                                           colour='#FF0000',type='inline',title='Type of classification',buttonLabel = 'Close',
                                                           content=HTML('When this option is set, log<sub>10</sub> bilateral flows ratios are classified using quantile based thresholds (default), otherwise evenly spaced thresholds are used.')),
                                                    helper(checkboxInput("IIgnoreOverCounting", "Ignore over-counting", value = TRUE),
                                                           colour='#FF0000',type='inline',title='Dealing with over-counting',buttonLabel = 'Close',
                                                           content='When this option is set, the cases of over-counting (log <sub> 10 </sub> bilateral flow rates> 0) are combined with the class of the lowest under-counting, otherwise over-counting has its own class.')
                                             ),

                                             column(width=7,
                                                    div(id='Izupa',
                                                        sliderInput(inputId = "ITranslateGroups", label = 'Model sensitivity (number of classes)', min = 2, max = 11, value = 5, step=1, sep=''))
                                             ),
                                             span(HTML('&#160;'),style="font-size:1px; align: top;"),
                                             tags$head(tags$style("#Izupa .form-group.shiny-input-container {margin-bottom: 0px;}", media="screen", type="text/css")),
                                )),
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                             style='background-color: #FFFFFF; border-color: #FFFFFF; padding: 0px; margin-bottom: -15px;',
                                             radioGroupButtons(
                                               inputId = "Ipanels",
                                               label = NULL,#"Select the result panel",
                                               justified= TRUE,
                                               width='100%',
                                               individual=FALSE,
                                               #checkIcon = list(  yes = icon("check-square")),
                                               choiceNames = c("Inspect bilateral flows ratios","Duration of stay",HTML("log<sub>10</sub> flows ratios"), "Classification"),
                                               choiceValues = 1:4,
                                               status = "danger"
                                             ))),
                                tags$head(tags$style("#Ipanels .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Ipanels .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),
                                conditionalPanel(condition = "input.Ipanels == 1",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",
                                                              helper(h3('Bilateral flows ratios for immigration data'),
                                                                     colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                                                     content='BilateralModel',size='l'),
                                                              tags$hr(style="border-color: black;"),
                                                              checkboxGroupInput("Icountry", h4("Countries selection"),
                                                                                 choices = Countries, selected = IniCntrSel, inline = TRUE),
                                                              actionButton("Iall", "All"),actionButton("Inone", "None"), downloadButton("IsaveBFR", "Download bilateral flows ratio data in xlsx format"),
                                                              br(),
                                                              br(),
                                                              plotOutput(outputId = "ImiPlot", height="700px", width='100%'),
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
                                                 )
                                                 )),
                                conditionalPanel(condition = "input.Ipanels == 2",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",

                                                              h3(HTML('Duration of stay by country of origin')),
                                                              selectInput("IdurationCountries", label = 'Select country of destination',
                                                                          choices = Countries),
                                                              tableOutput('Iduration'),
                                                              tags$head(tags$style("#Iduration {border-width:1px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Iduration table {font-size: 10px;border-collapse: collapse;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Iduration table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Iduration table td {white-space: nowrap; padding-right:0px;padding-left:0px;}", media="screen", type="text/css")),
                                                              #tags$head(tags$style("#Iduration table td.shrink {white-space: nowrap;}", media="screen", type="text/css")),
                                                              #tags$head(tags$style("#Iduration table td {}", media="screen", type="text/css")),
                                                              #tags$head(tags$style("#Iduration table td {padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              #tags$head(tags$style("#Iduration table td:not(:last-child) {white-space: nowrap;}", media="screen", type="text/css")),
                                                              #tags$head(tags$style("#Iduration table td:last-child {width:100%;}", media="screen", type="text/css")),


                                                 ))),
                                conditionalPanel(condition = "input.Ipanels == 3",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",

                                                              h3(HTML('Estimated log<sub>10</sub> ratios of the bilateral flows')),
                                                              tableOutput('Ilogratios'),
                                                              tags$head(tags$style("#Ilogratios {border-width:1px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Ilogratios table {padding-right:0px;padding-left:0px;font-size: 10px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Ilogratios table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Ilogratios table td {white-space: nowrap; padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Quantile-based thresholds used for classification.')),
                                                              tableOutput('IlogratiosThresholds'),
                                                              tags$head(tags$style("#IlogratiosThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#IlogratiosThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),
                                                 ))),
                                conditionalPanel(condition = "input.Ipanels == 4",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",
                                                              h3(HTML('Classification of the bilateral flow ratios.')),
                                                              plotOutput(outputId = "ImiPlotB", height="700px", width='100%'),

                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),

                                                                         helper(checkboxInput("INoData", "Mark no data", value = TRUE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c(MarkNoDataTxt)
                                                                         )
                                                                  ),
                                                                  column(3,h5(HTML('Image format')),selectInput("IformatB", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),

                                                                  column(3,h5(HTML('&#160;')),downloadButton("IsaveplotB", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("IsavedataB", "Save results as xlsx"))
                                                              ),
                                                 ),
                                ))),
                                mainPanel(br(),br(),br(),br(),br())
                       ),
                       
                       
                       tabPanel(title = PanelNames[5],
                                br(),#br(),
                                #sidebarPanel(
                                div(class="row", style='margin-left:0px',#style='margin:0px; padding:0px',
                                    div(class="col-lg-4", style='padding-right:0px; max-width:960px; min-width:200px',
                                        div(class='well', style="margin-bottom:15px;",
                                            
                                  helper(h3("General model options"),colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                         content='BilateralModel',size='l'),

                                  tags$hr(style="border-color: black;"),

                                  helper(h4("Duration of stay correction"),
                                         colour='#FF0000',type='markdown',title="",buttonLabel = 'Close',
                                         content = c('BilateralModel')),

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
                                              selected = 13),

                                  tableOutput('corrEtab'),
                                  tags$head(tags$style("#corrEtab table {background-color: white; }", media="screen", type="text/css")),
                                  tags$head(tags$style("#corrEtab table th {background-color: #CCBBFF; }", media="screen", type="text/css")),

                                  conditionalPanel(condition = "input.Eraymer > 4",
                                                   checkboxInput("Eseparated", "Use duration corrrection parameters calculated separately for immigration", value = FALSE),
                                                   checkboxInput("Eadditive", "Use additive optimization criteria (otherwise multiplicative)", value = TRUE),
                                  ),
                                                   tags$hr(style="border-color: black;"),
                                                   helper(selectInput("Erefcountry", h4("Reference group of countries"),
                                                                      choices = list('Nordic countries (without IS)'=1,'Nordic countries'=2,'Nordic countries+BE'=3,'Nordic countries+CH'=4,'Nordic countries+NL'=5,
                                                                                     'Nordic countries+BE+CH'=6,'Nordic countries+BE+NL'=7,'Nordic countries+CH+NL'=8,
                                                                                     'Nordic countries+BE+CH+NL'=9,'Nordic countries+AT+BE+CH+NL'=10,
                                                                                     'Nordic countries+AT+BE+CH+DE+NL'=11,'Nordic countries+AT+BE+CH+DE+FR+NL'=12,
                                                                                     'Nordic countries+AT+BE+CH+DE+FR+IE+NL'=13,'Nordic countries+AT+BE+CH+DE+FR+IE+NL+UK'=14,'All countries'=15),
                                                                      selected = RefCntrSel),
                                                          colour='#FF0000',type='inline',title='Reference group of countries',buttonLabel = 'Close',
                                                          content=c('Please set the "Duration of stay correction" first before setting this parameter.',
                                                                    '','Nordic countries include DK (Denmark), FI (Finland), IS (Island), NO (Norway), and SE (Sweeden).','',' See help (?) in "General model options" for more information about the bilateral flows ratio model.')),


                                  tags$hr(style="border-color: black;"),
                                  h4('Imputations of missing values'),
                                  helper(checkboxInput("Eimputations", "Use PCA imputations", value = TRUE),
                                         colour='#FF0000',type='inline',title='PCA imputations',buttonLabel = 'Close',
                                         content=c('Impute the missing values of the flows ratios matrix with the Principal Components Analysis model using missMDA package.')),
                                  conditionalPanel(condition = "input.Eimputations == true",
                                                   helper(sliderInput(inputId = "Encp", label = 'ncp parameter', min = 1, max = 5, value = 1, step=1, sep=''),
                                                          colour='#FF0000',type='inline',title='ncp parameter',buttonLabel = 'Close',
                                                          content='From imputePCA {missMDA package}: "integer corresponding to the number of components used to to predict the missing entries".')),
                                )),
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
                                             h3(HTML('Classification options')),
                                             tags$hr(style="border-color: black;"),

                                             column(width=5,
                                                    helper(checkboxInput("EUseQuantiles", "Classification based on quantiles", value = TRUE),
                                                           colour='#FF0000',type='inline',title='Type of classification',buttonLabel = 'Close',
                                                           content=HTML('When this option is set, log<sub>10</sub> bilateral flows ratios are classified using quantile based thresholds (default), otherwise evenly spaced thresholds are used.')),
                                                    helper(checkboxInput("EIgnoreOverCounting", "Ignore over-counting", value = TRUE),
                                                           colour='#FF0000',type='inline',title='Dealing with over-counting',buttonLabel = 'Close',
                                                           content='When this option is set, the cases of over-counting (log <sub> 10 </sub> bilateral flow rates> 0) are combined with the class of the lowest under-counting, otherwise over-counting has its own class.')
                                             ),
                                             column(width=7,
                                                    div(id='Ezupa',
                                                        sliderInput(inputId = "ETranslateGroups", label = 'Model sensitivity (number of classes)', min = 2, max = 11, value = 5, step=1, sep=''))
                                             ),
                                             span(HTML('&#160;'),style="font-size:1px; align: top;"),
                                             tags$head(tags$style("#Ezupa .form-group.shiny-input-container {margin-bottom: 0px;}", media="screen", type="text/css")),
                                )),
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
                                             style='background-color: #FFFFFF; border-color: #FFFFFF; padding: 0px; margin-bottom: -15px;',
                                             radioGroupButtons(
                                               inputId = "Epanels",
                                               label = NULL,#"Select the result panel",
                                               justified= TRUE,
                                               width='100%',
                                               individual=FALSE,
                                               #checkIcon = list(  yes = icon("check-square")),
                                               choiceNames = c("Inspect bilateral flows ratios","Duration of stay",HTML("log<sub>10</sub> flows ratios"), "Classification"),
                                               choiceValues = 1:4,
                                               status = "danger"
                                             ))),
                                tags$head(tags$style("#Epanels .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Epanels .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),
                                conditionalPanel(condition = "input.Epanels == 1",
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",
                                                         
                                                              helper(h3('Bilateral flows ratios for emigration data'),
                                                                     colour='#FF0000',type='markdown',title='',buttonLabel = 'Close',
                                                                     content='BilateralModel',size='l'),
                                                              tags$hr(style="border-color: black;"),
                                                              checkboxGroupInput("Ecountry", h4("Countries selection"),
                                                                                 choices = Countries, selected = IniCntrSel, inline = TRUE),
                                                              actionButton("Eall", "All"),actionButton("Enone", "None"),downloadButton("EsaveBFR", "Download bilateral flows ratio data in xlsx format"),
                                                              br(),
                                                              br(),
                                                              plotOutput(outputId = "EmiPlot", height="700px", width='100%'),
                                                              br(),
                                                              div(style="display:inline-block;vertical-align:top;",
                                                                  column(6,checkboxInput("Elogscale", "Use log-scale", value = TRUE)),
                                                                  column(6,checkboxInput("EplotCI", "Plot confidence intervals", value = TRUE)),
                                                              ),
                                                              div(style="display:inline-block;vertical-align:top;",
                                                                  h5('Choose a format and save the plot'),
                                                                  column(6,selectInput("Eformat", NULL,
                                                                                       choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),
                                                                  column(6,downloadButton("Esaveplot", "Save plot"))
                                                                  )
                                                 )
                                                 )),
                                conditionalPanel(condition = "input.Epanels == 2",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",
                                                         
                                                              h3(HTML('Duration of stay by country of destination')),
                                                              selectInput("EdurationCountries", label = 'Select country of origin',
                                                                          choices = Countries),
                                                              tableOutput('Eduration'),
                                                              tags$head(tags$style("#Eduration {border-width:1px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Eduration table {padding-right:0px;padding-left:0px;font-size: 10px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Eduration table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Eduration table td {white-space: nowrap; padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              #tags$head(tags$style("#Eduration table td {padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),

                                                 ))),
                                conditionalPanel(condition = "input.Epanels == 3",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",
                                                         

                                                              h3(HTML('Estimated log<sub>10</sub> ratios of the bilateral flows')),
                                                              tableOutput('Elogratios'),
                                                              tags$head(tags$style("#Elogratios {border-width:4px;border-color:black;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Elogratios table {font-size: 10px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Elogratios table th {background-color: #CCBBFF;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Elogratios table td {white-space: nowrap; padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Quantile-based thresholds used for classification.')),
                                                              tableOutput('ElogratiosThresholds'),
                                                              tags$head(tags$style("#ElogratiosThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#ElogratiosThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),
                                                 ))),
                                conditionalPanel(condition = "input.Epanels == 4",
                                                 #sidebarPanel(width=8,
                                                 div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                                     div(class='well', style="margin-bottom:15px;",
                                                         
                                                              h3(HTML('Classification of the bilateral flow ratios.')),
                                                              plotOutput(outputId = "EmiPlotB", height="700px", width='100%'),

                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),

                                                                         helper(checkboxInput("ENoData", "Mark no data", value = TRUE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c(MarkNoDataTxt)
                                                                         )
                                                                  ),
                                                                  column(3,h5(HTML('Image format')),selectInput("EformatB", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),

                                                                  column(3,h5(HTML('&#160;')),downloadButton("EsaveplotB", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("EsavedataB", "Save results as xlsx"))
                                                              ),
                                                     ),
                                                 ))),
                                mainPanel(br(),br(),br(),br(),br())
                       ),
                       tabPanel(title = PanelNames[8],
                                br(), #br(),
                                # div(class="row", style='margin-left:0px',#style='margin:0px; padding:0px',
                                #     div(class="col-lg-4", style='padding-right:0px; max-width:800px; min-width:200px',
                                #         div(class='well', style="margin-bottom:15px;",
                                #             
                                #sidebarPanel(
                                div(class="row", style='margin-left:0px',#style='margin:0px; padding:0px',
                                    div(class="col-lg-4", style='padding-right:0px; max-width:960px; min-width:200px',
                                        div(class='well', style="margin-bottom:15px;",
                                  h3('Mixing options'),
                                  tags$hr(style="border-color: black;"),
                                  h4('Mixing threshold'),
                                  helper(
                                    sliderInput(inputId = "IYear", label = 'Threshold year', min = 2003, max = 2018, value = 2009, step=1, sep=''),
                                    colour='#FF0000',type='inline',title='Threshold year',buttonLabel = 'Close',
                                    content=ThreshTxt),

                                  tags$hr(style="border-color: black;"),

                                  helper(uiOutput('I2yearshowA'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content=WeightsMixTxt),
                                  #tags$hr(style="border-color: black; border-top: dashed 1px"),

                                  helper(sliderInput(inputId = "I3wimemb", label = "IMEM score num (A)", min = 0, max = 1, value = wimemb, step=Step),
                                         colour='#FF0000',type='inline',title='IMEM score weight',buttonLabel = 'Close',
                                         content=IMEMc('A')),

                                  helper(sliderInput(inputId = "I3wmetab", label = "Metadata score num (A)", min = 0, max = 1, value = wmetab, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content=METAwtxt),

                                  helper(sliderInput(inputId = "I3wmodelb", label = "Model score num (A)", min = 0, max = 1, value = wmodelb, step=Step),
                                         colour='#FF0000',type='inline',title='Model weight for (A)',buttonLabel = 'Close',
                                         content=MODELwtxt('Immigration')),

                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from emigration</b> - replaces current values of parameters with equivalent values from <b>Combined emigration scores</b> tab, <b>&#8721 weights = 1</b> makes all weights sum up to 1.'),

                                  actionButton("I3weightsresetb", "Reset"),
                                  actionButton("I3cloneb", "Clone from emigration"),
                                  actionButton("I3recalcb", HTML("&#8721 weights = 1")),

                                  tags$hr(style="border-color: black; border-top: dashed 1px"),
                                  helper(uiOutput('I2yearshowB'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content=WeightsMixTxt),
                                  #tags$hr(style="border-color: black; border-top: dashed 1px"),

                                  helper(sliderInput(inputId = "I3wimema", label = "QuantMig score num (B)", min = 0, max = 1, value = wimema, step=Step),
                                         colour='#FF0000',type='inline',title='QuantMig score weight',buttonLabel = 'Close',
                                         content=QUANTMIGc('B')),

                                  helper(sliderInput(inputId = "I3wmetaa", label = "Metadata score num (B)", min = 0, max = 1, value = wmetaa, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content=METAwtxt),

                                  helper(sliderInput(inputId = "I3wmodela", label = "Model score num (B)", min = 0, max = 1, value = wmodela, step=Step),
                                         colour='#FF0000',type='inline',title='Model weight for (B)',buttonLabel = 'Close',
                                         content=MODELwtxt('Immigration')),

                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from emigration</b> - replaces current values of parameters with equivalent values from <b>Combined emigration scores</b> tab, <b>&#8721 weights = 1</b> makes all weights sum up to 1.'),

                                  actionButton("I3weightsreseta", "Reset"),
                                  actionButton("I3clonea", "Clone from emigration"),
                                  actionButton("I3recalca", HTML("&#8721 weights = 1")),
                                  br(),
                                  br(),
                                )),
                                #sidebarPanel(width=8,
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
                                             h3('Classification options'),
                                             tags$hr(style="border-color: black;"),
                                             helper(sliderInput(inputId = "IFinalGroups", label = 'Number of undercounting classes', min = 2, max = 7, value = 5, step=1, sep=''),
                                                    colour='#FF0000',type='inline',title='Number of undercounting classes',buttonLabel = 'Close',
                                                    content='Undercounting is categorized according to uniformly spaced thresholds. See <b>Mean weighted scores</b> panel below.'
                                             ),
                                )),
                                #sidebarPanel(width=8,
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
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
                                             ))),
                                tags$head(tags$style("#Ipanels2 .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Ipanels2 .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),

                                #sidebarPanel(width=8,
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
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
                                                              tags$head(tags$style("#Iscores table {font-size: 10px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Iscores table th {background-color: #CCBBFF;}", media="screen", type="text/css")),

                                                              tags$head(tags$style("#Iscores table td {white-space: nowrap; padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Uniformly spaced thresholds used for classification.')),
                                                              tableOutput('IscoresThresholds'),
                                                              tags$head(tags$style("#IscoresThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#IscoresThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),

                                                              br()
                                             ),
                                             conditionalPanel(condition ="input.Ipanels2 == 3",
                                                              h3(HTML('Finall classification of the undercounting.')),
                                                              plotOutput(outputId = "ImiPlot2", height="800px", width='100%'),
                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),

                                                                         helper(checkboxInput("INoData2", "Mark no data", value = FALSE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c(MarkNoDataTxt)),
                                                                  ),

                                                                  column(3,h5(HTML('Image format')),selectInput("Iformat2", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),

                                                                  column(3,h5(HTML('&#160;')),downloadButton("Isaveplot2", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("Isavedata2", "Save results as xlsx"))
                                                              ),
                                             ),
                                ))),

                                mainPanel(br(),br(),br(),br(),br()),
                       ),
                       tabPanel(title = PanelNames[9],
                                br(), #br(),
                                div(class="row", style='margin-left:0px',#style='margin:0px; padding:0px',
                                    div(class="col-lg-4", style='padding-right:0px; max-width:960px; min-width:200px',
                                        div(class='well', style="margin-bottom:15px;",
                                            
                                  h3('Mixing options'),
                                  tags$hr(style="border-color: black;"),
                                  h4('Mixing threshold'),
                                  helper(
                                    sliderInput(inputId = "EYear", label = 'Threshold year', min = 2003, max = 2018, value = 2009, step=1, sep=''),
                                    colour='#FF0000',type='inline',title='Threshold year',buttonLabel = 'Close',
                                    content=ThreshTxt),

                                  tags$hr(style="border-color: black;"),

                                  helper(uiOutput('E2yearshowA'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous panels)'),

                                  helper(sliderInput(inputId = "E3wimemb", label = "IMEM score num (A)", min = 0, max = 1, value = wimemb, step=Step),
                                         colour='#FF0000',type='inline',title='IMEM score weight',buttonLabel = 'Close',
                                         content=IMEMc('A')),

                                  helper(sliderInput(inputId = "E3wmetab", label = "Metadata score num (A)", min = 0, max = 1, value = wmetab, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (A)',buttonLabel = 'Close',
                                         content=METAwtxt),

                                  helper(sliderInput(inputId = "E3wmodelb", label = "Model score num (A)", min = 0, max = 1, value = wmodelb, step=Step),
                                         colour='#FF0000',type='inline',title='Model weight for (A)',buttonLabel = 'Close',
                                         content=MODELwtxt('Emigration')),

                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from immigration</b> - replaces current values of parameters with equivalent values from <b>Combined immigration scores</b> tab, <b>&#8721 weights = 1</b> makes all weights sum up to 1.'),

                                  actionButton("E3weightsresetb", "Reset"),
                                  actionButton("E3cloneb", "Clone from immigration"),
                                  actionButton("E3recalcb", HTML("&#8721 weights = 1")),

                                  tags$hr(style="border-color: black; border-top: dashed 1px"),
                                  helper(uiOutput('E2yearshowB'),
                                         colour='#FF0000',type='inline',title='Weighted mean',buttonLabel = 'Close',
                                         content='Weights used to calculate weighted mean of numerical scores for metadata, IMEM, and model (see previous panels)'),

                                  helper(sliderInput(inputId = "E3wimema", label = "QuantMig score num (B)", min = 0, max = 1, value = wimema, step=Step),
                                         colour='#FF0000',type='inline',title='QuantMig score weight',buttonLabel = 'Close',
                                         content=QUANTMIGc('B')),

                                  helper(sliderInput(inputId = "E3wmetaa", label = "Metadata score num (B)", min = 0, max = 1, value = wmetaa, step=Step),
                                         colour='#FF0000',type='inline',title='Metadata weight for (B)',buttonLabel = 'Close',
                                         content=METAwtxt),

                                  helper(sliderInput(inputId = "E3wmodela", label = "Model score num (B)", min = 0, max = 1, value = wmodela, step=Step),
                                         colour='#FF0000',type='inline',title='Model weight for (B)',buttonLabel = 'Close',
                                         content=MODELwtxt('Emigration')),

                                  helper(tags$span(' '),
                                         colour='#FF0000',type='inline',title='Buttons',buttonLabel = 'Close',
                                         content='<b>Reset</b> - restores default values, <b>Clone from immigration</b> - replaces current values of parameters with equivalent values from <b>Combined immigration scores</b> tab, <b>&#8721 weights = 1</b> makes all weights sum up to 1.'),

                                  actionButton("E3weightsreseta", "Reset"),
                                  actionButton("E3clonea", "Clone from immigration"),
                                  actionButton("E3recalca", HTML("&#8721 weights = 1")),
                                  br(),
                                  br(),
                                )),
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
                                             h3('Classification options'),
                                             tags$hr(style="border-color: black;"),
                                             helper(sliderInput(inputId = "EFinalGroups", label = 'Number of undercounting classes', min = 2, max = 7, value = 5, step=1, sep=''),
                                                    colour='#FF0000',type='inline',title='Number of undercounting classes',buttonLabel = 'Close',
                                                    content='Undercounting is categorized according to uniformly spaced thresholds. See <b>Mean weighted scores</b> panel below.'
                                             ),
                                )),
                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
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
                                             ))),
                                tags$head(tags$style("#Epanels2 .btn-danger {background-color: #FFBBBB; border-color: #DD9999;}", media="screen", type="text/css")),
                                tags$head(tags$style("#Epanels2 .btn-danger.active {background-color: #CC0000; border-color: #AA0000;}", media="screen", type="text/css")),

                                div(class="col-lg-8", style='padding-right:0px; max-width:960px; min-width:925px',
                                    div(class='well', style="margin-bottom:15px;",
                                        
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
                                                              tags$head(tags$style("#Escores table {font-size: 10px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#Escores table th {background-color: #CCBBFF;}", media="screen", type="text/css")),

                                                              tags$head(tags$style("#Escores table td {white-space: nowrap; padding-right:0px;padding-left:0px;width:1px;}", media="screen", type="text/css")),
                                                              tags$hr(style="border-color: black;"),
                                                              h3(HTML('Uniformly spaced thresholds used for classification.')),
                                                              tableOutput('EscoresThresholds'),
                                                              tags$head(tags$style("#EscoresThresholds table th {background-color: #CCBBFF; width:1px;}", media="screen", type="text/css")),
                                                              tags$head(tags$style("#EscoresThresholds table {font-size: 13px; width:1px; align: center}", media="screen", type="text/css")),

                                                              br()
                                             ),
                                             conditionalPanel(condition ="input.Epanels2 == 3",
                                                              h3(HTML('Finall classification of the undercounting.')),
                                                              plotOutput(outputId = "EmiPlot2", height="800px", width='100%'),
                                                              div(style="display:inline-block;vertical-align:bottom;",
                                                                  column(3,
                                                                         h4(HTML('&#160;')),

                                                                         helper(checkboxInput("ENoData2", "Mark no data", value = FALSE),
                                                                                colour='#FF0000',type='inline',title='Mark no data',buttonLabel = 'Close',
                                                                                content=c(MarkNoDataTxt)),
                                                                  ),

                                                                  column(3,h5(HTML('Image format')),selectInput("Eformat2", NULL,
                                                                                                                choices = list("pdf" = 'pdf', "png" = 'png',"tiff" = 'tiff'), selected = 1, width='100%')),

                                                                  column(3,h5(HTML('&#160;')),downloadButton("Esaveplot2", "Save image")),
                                                                  column(3,h5(HTML('&#160;')),downloadButton("Esavedata2", "Save results as xlsx"))
                                                              ),
                                             ),
                                ))),

                                mainPanel(br(),br(),br(),br(),br()),

                       )
           )
    )
  )
)

shinyApp(ui=shinyUI, server = shinyServer)

#rsconnect::deployApp()

