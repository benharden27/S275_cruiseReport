rm(list = ls())

# Load the OCE package
library(oce)
library(ocedata)
library(sea)
data(coastlineWorldFine)

# Set the master (source) and output folders for the data
cruiseID <- 'S275'

rootFold <- file.path("~/Documents/SEA",cruiseID,"cruiseReport")

folddata <- file.path(rootFold,"data")
foldplot <- file.path(rootFold,"plots")

#####
# CTD DATA
#####

# Set up parameters
CTDflag <- NULL
foldin <- file.path(folddata,"CTD","Cnv")
plotFL = F
newFL = F

# Read in CTDs
CTDs <- readSEActd(foldin,CTDflag=CTDflag,plotFL=plotFL,newFL = newFL,plotfold=foldplot)

# plot CTD section along cruise track
if(length(CTDs)>1) {
  # plot Temp, Sal, Density data with Map
  outname <- file.path(foldplot,paste0(cruiseID,'_CTD_section.png'))
  png(filename=outname,height=9,width=9,units='in',res=300,type='cairo')
  plotmap3sec(CTDs,ylim=c(0,600))
  dev.off()
  # plot Auxilary data (O2 and fluo)
  outname <- file.path(foldplot,paste0(cruiseID,'_CTD_section_2.png'))
  png(filename=outname,height=6,width=7,units='in',res=300,type='cairo')
  plotO2flsec(CTDs)
  dev.off()
}


#####
# NEUSTON
#####
# Read biomass
filein <- file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_Neuston.xlsm'))
df <- try(readSEAxls(filein,rplcsv = T))
if(class(df)=='try-error') {
  filein <- file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_Neuston.xlsx'))
  df <- try(readSEAxls(filein,skip=1,rplcsv = T))
}
outname <- file.path(foldplot,paste0(cruiseID,'_biomass.png'))
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAbio(df,legloc='bottomright')
dev.off()

outname <- file.path(foldplot,paste0(cruiseID,'_plastics.png'))
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAplastic(df,legloc='bottomright')
dev.off()

# Read 100 count
filein <- file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_Neuston.xlsm'))
df2 <- readSEAxls(filein,sheet=2,skip=1,rplcsv = T)
# names(df2) <- names(readSEAxls(filein,sheet=2,rplcsv = T))
df3 <- data.frame(df,shannon=df2[[35]][1:dim(df)[1]])
outname <- file.path(foldplot,paste0(cruiseID,'_biodiversity.png'))
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAbiod(df3,legloc='bottomright')
dev.off()


#####
# FLOWTHROUGH DATA
#####

# Read in flowthrough data
elgs <- list.files(path=folddata, pattern="\\.elg")[1]
filein <- file.path(folddata,elgs)
df <- readSEAelg(filein)

reg <- ""
bathy <- T
step <- 60
outname <- file.path(foldplot,paste0(cruiseID,'_flowthrough.png'))
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAelg(df,reg=reg,step=step,bathy=bathy)
dev.off()

# Plot cruisetrack with stations
# Read CTD station locations
X <- readCTDsll(readSEActd(foldin=file.path(folddata,"CTD","Cnv"),newFL=F,plotFL=F))
dfn <- readSEAxls(file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_Neuston.xlsm')),rplcsv = T)
X <- readbioll(dfn,X=X)

outname <- file.path(foldplot,paste0(cruiseID,'_cruiseTrack.png'))
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAct(df,stations=X)
# plotSEAct(df)
dev.off()



#####
# HOURLY DATA
#####
# Read in hourly work
filein <- file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_Hourlywork.xlsm'))
df <- readSEAxls(filein)

# Plot winds
outname <- file.path(foldplot,paste0(cruiseID,'_winds.png'))
png(filename=outname,height=7,width=7,units='in',res=300,type='cairo') # set up the png file to print to
plotSEAwind(df)
dev.off()


#####
# SURFACE STATIONS
#####
# Read in surface stations
filein <- file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_surfsamp.xlsm'))
df <- readSEAxls(filein)
# Plot surface station data

regs <- ""

for (i in 1:length(regs)) {
  outname <- file.path(foldplot,paste0(paste0(cruiseID,'_surfStat'),rep('_',nchar(regs[i])>0),regs[i],'.png'))
  png(filename=outname,height=9,width=9,units='in',res=300,type='cairo') # set up the png file to print to
  plotSEAsurf(df,reg=regs[i])
  dev.off()
}


#####
# CURRENTS
#####
# Read currents
foldin <- file.path(folddata,"OceanDataView","ADCP Text Files")
X <- readSEAadcp_all(foldin)
# plot currents
regs <- ""

scales <- c(0.05,0.005,0.005,0.005)
stps <- c(6,1,1,1)
for (i in 1:length(regs)) {
  outname <- file.path(foldplot,paste0(paste0(cruiseID,'_currents'),rep('_',nchar(regs[i])>0),regs[i],'.png'))
  png(filename=outname,height=9,width=9,units='in',res=300,type='cairo') # set up the png file to print to
  plotSEAcurr(X,scale=scales[i],stp=stps[i],reg=regs[i])
  dev.off()
}



######
# RBR TOW-YO
######
foldin <- file.path(folddata,'CTD','RBR CTD',"dat","dat")
files <- list.files(path=foldin, pattern="S275.*.dat")

# auxilary ELGs
elgs <- list.files(path=folddata, pattern="\\.elg")[1]
elg <- file.path(folddata,elgs)

# auxilary CTDs
CTDs <- readSEActd(foldin=file.path(folddata,"CTD","Cnv"),newFL=F,plotFL=F)

for (i in files) {
  CTD1 <- CTD2 <- NULL
  if(length(grep("009",i))>0) {
    CTD1 <- CTDs[[4]]
    CTD2 <- CTDs[[5]]
  }
  if(length(grep("012",i))>0) {
    CTD1 <- CTDs[[6]]
    CTD2 <- CTDs[[7]]
  }

  X <- readRBRtow(file.path(foldin,i),elg=elg)
  df <- X$df
  if(length(grep("018",i))>0) {
    X$ctds <- X$ctds[c(1,3:length(X$ctds))]
  }
  # outname <- file.path(foldplot,sub(".dat",".png",i))
  # png(filename=outname,height=9,width=7,units='in',res=300,type='cairo') # set up the png file to print to
  # plotRBRtow(df)
  # dev.off()
  if (length(names(X))>1) {
    f2 <- function(x){range(x@data$temperature,na.rm=T)}
    Tran <- range(unlist(lapply(X$ctds,f2)))
    f2 <- function(x){range(x@data$salinity,na.rm=T)}
    Sran <- range(unlist(lapply(X$ctds,f2)))
    f2 <- function(x){range(swSigma(x@data$salinity,x@data$temperature,x@data$pressure),na.rm=T)}
    Dran <- range(unlist(lapply(X$ctds,f2)))
    ctdplot <- append(X$ctds,CTD1,0)
    ctdplot <- append(ctdplot,CTD2)

    outname <- file.path(foldplot,sub(".dat","_grid.png",i))
    outname <- gsub("-","_",outname)
    png(filename=outname,height=7,width=10,units='in',res=300,type='cairo') # set up the png file to print to
    plotmap3sec(ctdplot,Dran=Dran,Tran=Tran,Sran=Sran,ylim=c(0,120))
    dev.off()
  }
}



#######
# PLOT HYDROWORK
#######
filein <- file.path(file.path(folddata,"SHIPDATA"),paste0(cruiseID,'_Hydrowork.xlsm'))
df <- readSEAxls(filein)
outname <- file.path(foldplot,paste0(cruiseID,'_hydrowork.png'))
png(filename=outname,height=11,width=11,units='in',res=300,type='cairo') # set up the png file to print to
plothydro(df)
dev.off()

