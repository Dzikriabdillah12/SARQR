#-----PACKAGES------#
library(maptools)
library(rgeos)
library(rgdal)
library(lmtest)
library(McSpatial)
library(classInt)
library(RColorBrewer)
library(raster)
library(sp)
library(tmap)
library(car)
library(spatialreg)
library(spdep)
library(gplots)
library(shapefiles)
#===================================#
#input data#
data=read.csv(file.choose())
data
str(data)
attach(data)
summary(data)
#===================================#
##--ANALISIS REGRESI BERGANDA--##
ols=lm(tbparu~kebiasaan+air+jamban, data=data)
summary(ols)
residual=resid(reg)
reg=lm(VBE~BJC+VBC)
#===================================#
#ASUMSI KLASIK#
#NORMALITAS
shapiro.test(residual)
#MULTIKOLINEARITAS
vif(ols)
#HETEROSKEDASTISITAS (BREUNCH-PAGAN TEST)
bptest(tbparu~kebiasaan+air+jamban, data=data)
#NON AUTOKORELASI (DURBIN-WATSON)
dwtest(ols)
#LINEARITAS
resettest(ols)
resettest(reg)

#===================================#
#MEMBUAT PLOT#
par(mfrow=c(2,2))
plot(data$tbparu~data$kebiasaan,xlab="KEBIASAAN MEROKOK", ylab="IR TB Paru",pch=1)
abline(lm(tbparu~kebiasaan),col="red")
plot(data$tbparu~data$air,xlab="AIR BERSIH", ylab="IR TB Paru",pch=1)
abline(lm(tbparu~air),col="red")
plot(data$tbparu~data$jamban,xlab="JAMBAN SEHAT", ylab="IR TB Paru",pch=1)
abline(lm(tbparu~jamban),col="red")
#===================================#
#MEMBUAT PETA KABUPATEN TASIKMALAYA#
Indo=getData('GADM',country='IDN',level=3)
Jabar=Indo[Indo$NAME_1=="Jawa Barat",]
Tasik=Jabar[Jabar$NAME_2=="Tasikmalaya",]
plot(Tasik)
no=c(1:39)
Kecamatan=Tasik$NAME_3   
Coordinate=coordinates(Tasik)
plot(Tasik,main="Map Kabupaten Tasikmalaya menurut Kecamatan",
     col="white",axes=TRUE)
points(Coordinate+0.001,lwd=2,cex=0.5)
text(Coordinate-0.001,label=no,cex=0.7)

boxplot(residual,col="blue",main="Boxplot Residual Data")
#===================================#
#SET MAPPING DATA#
northarrow<-list("SpatialPolygonsRescale",
                 layout.north.arrow(),
                 offset<-c(107.9044,108.4425),scale=0.1,which=2)
scalebar<-list("SpatialPolygonRescale",
               layout.scale.bar(),
               offset<-c(-7.816817,-7.03895),scale=0.1,
               fill<-c("transparent","black"),which=2)
text1<-list("sp.text",c(107.9044,108.4425),"0",which=2)
text2<-list("sp.text",c(-7.816817,-7.03895),"0.1 KM",which=2)
plotclr<-brewer.pal(5,"Reds")
colorkey<-list(space="right")
#===================================#
#--MENGGAMBAR PETA VARIABEL PENELITIAN--#
#INCIDENT RATE TUBERKULOSIS PARU
datapeta=data
Y.=c(tbparu[1:19],tbparu[20]-0.001,tbparu[21:39])
Tasik@data=cbind(Y.,Tasik=datapeta)
font=list("sp.text", coordinates(Tasik), as.character(Tasik@data[,2]),col="brown", cex=0.5,font=2)
nlcr=5
class<-classIntervals(tbparu,nlcr,style="quantile")
colcode<-findColours(class,plotclr)
spplot(Tasik[,1],sp.layout=list(northarrow),as.table=TRUE,col.regions=plotclr,at=round(class$brks,digits=10),colorkey=list(space="right"), main="IR TB Paru Kabupaten Tasikmalaya Tahun 2019")
text(Coordinate-0.001,label=no,cex=0.7)
#KEBIASAAN TIDAK MEROKOK
datapeta=data
x1<-c(kebiasaan[1:21],kebiasaan[22]-0.001,kebiasaan[23:39])
Tasik@data=cbind(x1,Tasik=datapeta)
font=list("sp.text", coordinates(Tasik), as.character(Tasik@data[,2]),col="brown", cex=0.5,font=2)
nlcr=5
class<-classIntervals(kebiasaan,nlcr,style="quantile")
colcode<-findColours(class,plotclr)
spplot(Tasik[,1],sp.layout=list(northarrow),as.table=TRUE,col.regions=plotclr,at=round(class$brks,digits=10),colorkey=list(space="right"), main="Kebiasaan Tidak Merokok di Kabupaten Tasikmalaya Tahun 2019")
kebiasaan[30]
#AIR BERSIH
datapeta=data
x2<-c(air[1:4]-3,air[5:6]+0.001,air[7:10]-3,air[11]+0.001,air[12:18]-3,air[19]+0.001,air[20]+0.001,air[21:26]-3,air[27:28]+0.001,air[29:32]-3,air[33]+0.001,air[34]-3,air[35]+0.001,air[36:37]-3,air[38]+0.001,air[39])
Tasik@data=cbind(air,Tasik=datapeta)
font=list("sp.text", coordinates(Tasik), as.character(Tasik@data[,2]),col="brown", cex=0.5,font=2)
nlcr=5
class<-classIntervals(x2,nlcr,style="quantile")
colcode<-findColours(class,plotclr)
spplot(Tasik[,1],sp.layout=list(northarrow),as.table=TRUE,col.regions=plotclr,at=round(class$brks,digits=5),colorkey=list(space="right"), main="AIR BERSIH Kabupaten Tasikmalaya Tahun 2019")

#JAMBAN SEHAT
datapeta=data
x3=c(jamban[1:10],jamban[11]+0.001,jamban[12:19],jamban[20],jamban[21:37],jamban[38]-0.0001,jamban[39])
Tasik@data=cbind(x3,Tasik=datapeta)
font=list("sp.text", coordinates(Tasik), as.character(Tasik@data[,2]),col="brown", cex=0.5,font=2)
nlcr=5
class<-classIntervals(jamban,nlcr,style="quantile")
colcode<-findColours(class,plotclr)
spplot(Tasik[,1],sp.layout=list(northarrow),as.table=TRUE,col.regions=plotclr,at=round(class$brks,digits=10),colorkey=list(space="right"), main="JAMBAN SEHAT Kabupaten Tasikmalaya Tahun 2019")
#===================================#
#MEMBUAT MATRIKS PEMBOBOT SPASIAL#
WM=makew(shpfile=Tasik,coormat=coords,
         method="queen")
W<-mat2listw(WM$wmat)
W
#===================================#
#MENGUJI DEPENDENSI SPASIAL(UJI MORAN)#
#lm.morantest(ols,rr)
lm.morantest(ols,W)
#Langrange Multiplier test for spatial lag and spatial error dependencies
lm.LMtests(ols,W,test=c("LMlag"))
#SPATIAL AUTOREGRESSIVE#
spasial.lag=spatialreg::lagsarlm(IR_TB_PARU~KEBIASAAN_TIDAK_MEROKOK+`AIR BERSIH`+`JAMBAN SEHAT`+CUCITANGAN,data=dataa,rr)
summary(spasial.lag)
spasial.lag=spatialreg::lagsarlm(IR_TB_PARU~KEBIASAAN_TIDAK_MEROKOK+`AIR BERSIH`+`JAMBAN SEHAT`+KEBIASAANCUCI,data=dataa,rr)
summary(spasial.lag)
#SPASIAL ERROR MODEL#
spasial.error=spatialreg::errorsarlm(IR_TB_PARU~KEBIASAAN_TIDAK_MEROKOK+`AIR BERSIH`+`JAMBAN SEHAT`+CUCITANGAN,data=dataa,W)
summary(spasial.error)
#===================================#
#PEMODELAN SPATIAL AUREGRESSIVE QUANTILE REGRESSION# 
#TAU 0.1
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.1,rhomat=seq(-10,10,1))
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.1,rhomat=seq(0,2,0.01))
fit1<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
               tau=0.1,rhomat=seq(0.87,0.89,0.001))
Yhat1<-fit1[1,1]+fit1[5,1]*(WM$wmat%*%tbparu)+
  fit1[2,1]*kebiasaan+fit1[3,1]*air+fit1[4,1]*jamban
Yhat1
#--------------------#
#TAU 0.25
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.25,rhomat=seq(-10,10,1))
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.25,rhomat=seq(0,2,0.01))
fit2<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
               tau=0.25,rhomat=seq(0.50,0.52,0.001))
Yhat2<-fit2[1,1]+fit2[5,1]*(WM$wmat%*%tbparu)+
  fit2[2,1]*kebiasaan+fit2[3,1]*air+fit2[4,1]*jamban
Yhat2
#---------------------#
#TAU 0.5
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.50,rhomat=seq(-10,10,1))
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.50,rhomat=seq(1,3,0.01))
fit3<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
               tau=0.50,rhomat=seq(1.7,1.9,0.001))
Yhat3<-fit3[1,1]+fit3[5,1]*(WM$wmat%*%tbparu)+
  fit3[2,1]*kebiasaan+fit3[3,1]*air+fit3[4,1]*jamban
Yhat3
#---------------------------------#
#TAU 0.75
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.750,rhomat=seq(-10,10,1))
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.750,rhomat=seq(0,2,0.01))
fit4<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
               tau=0.750,rhomat=seq(1.32,1.34,0.001))
Yhat4<-fit4[1,1]+fit4[5,1]*(WM$wmat%*%tbparu)+
  fit4[2,1]*kebiasaan+fit4[3,1]*air+fit4[4,1]*jamban
Yhat4
#---------------------------#
#TAU 0.90
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.90,rhomat=seq(-10,10,1))
fit<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
              tau=0.90,rhomat=seq(0,2,0.01))
fit5<-qregspiv(tbparu~kebiasaan+air+jamban,wmat=WM$wmat,
               tau=0.90,rhomat=seq(0.74,0.76,0.001))
Yhat5<-fit5[1,1]+fit5[5,1]*(WM$wmat%*%tbparu)+
  fit5[2,1]*kebiasaan+fit5[3,1]*air+fit5[4,1]*jamban
Yhat5
#========================================================================================#
#MEMBUAT PETA PERSEBARAN INCIDENT RATE TB PARU
no<-c(1:39)
Yhat3.<-c(Yhat3[1:9],Yhat3[10]+0.001,Yhat3[11:39])
Yhat5.<-c(Yhat5[1:27],Yhat5[28]-0.001,Yhat5[29:39])
Yhat<-data.frame(no,Yhat1,Yhat2,Yhat3.,Yhat4,Yhat5.)
data.combined<-cbind(Tasik,Yhat)
Y.hat<-c(Yhat1,Yhat2,Yhat3,Yhat4,Yhat5)
plotvar<-Y.hat
class<-classIntervals(plotvar,5,style="quantile")
colcode<-findColours(class,plotclr)
colorkey<-list(space="right")
spplot(data.combined,c("Yhat1","Yhat2","Yhat3.",
                       "Yhat4","Yhat5."),names.attr=c("Quantile 0.1",
                                                      "Quantile 0.25","Quantile 0.5",
                                                      "Quantile 0.75","Quantile 0.9"),
       colorkey=list(space="right"),
       col.regions=plotclr,at=round(class$brks,
                                    digits=10),sp.layout=list(northarrow),
       as.table=TRUE)
