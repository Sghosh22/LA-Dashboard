
# This program reads summit output files and creates plots of the User Benefit
# values.
#
# Erin Wardell <wardell@pbworld.com> and Andrew Stryker <stryker@pbworld.com>
#
#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

#libraries - you will need to install these prior to using script the first time

library(foreign)
library(maptools)
library(sp)
library(RColorBrewer)
library(lattice)
library(dplyr)  # tidy data manipulation
library(leaflet)  # interative mapping
library(DT)  # interactive tables
library(crosstalk)  # inter-widget interactivity
library(ggplot2)
#library(ggiraph)
#knitr::opts_chunk$set(echo = TRUE)
library(knitr)
#library(reactable)
library(tibble)
library(rgdal)
library(sf)
library(sp)
library(tidyverse)
#install.packages("mapview")
library(mapview)
#remotes::install_github("r-spatial/mapview@develop")
#remotes::install_github("r-spatial/sf")
#remotes::install_github("r-spatial/mapview")
# install.packages("maptools")

# Set your filepaths here [Jump to 242/432/300 to modify the stops/output/title]
r <- list(list(), list(), list(), list())
# Filepath to GIS
# filepathtoshapes <- 'C:/Users/USSG674953.CORP/Documents/Desktop/Work/LA Dashboard/Shapefile'
filepathtoshapes <- here::here('Shapefile') #MGComment: always make your pathes relative, it helps others use your code

# Filepath to Summit data
# filepathtodata <- 'C:/Users/USSG674953.CORP/Documents/Desktop/Work/LA Dashboard/C1_NB_vs_C1_LaBrea'
filepathtodata <- here::here('C1_NB_vs_C1_LaBrea')

# Filepath where you want output to go
# filepathoutput<-'C:/Users/USSG674953.CORP/Documents/Desktop/Work/LA Dashboard/C1_NB_vs_C1_LaBrea/Maps'
filepathoutput<- here::here('C1_NB_vs_C1_LaBrea/Maps')

#-----------------------------------------------------------------------------#
#
# Set legend bin type here.
# Standardized bin legend if FALSE. FTA bin legend if TRUE.

fta_bin <- FALSE

#name mapping data
# Add as many shapes as you'd like, following same pattern

#tazs.shapefile   <- paste(filepathtoshapes,"AMC_TAZ_Split_3017.shp",sep="/")

boundary.shapefile   <- paste(filepathtoshapes,"KNE_Study_Area.shp",sep="/")
# district.shapefile   <- paste(filepathtoshapes,"AMC_TAZ_Split_Dist22.shp",sep="/")

lrt.purple_mos1       <- paste(filepathtoshapes,"Purple_Line_MOS2.shp",sep="/")
lrt.purple_mos2       <- paste(filepathtoshapes,"Purple_Line.shp",sep="/")
lrt.purple_mos3       <- paste(filepathtoshapes,"Purple_Line_Extension_Section3.shp",sep="/")
lrt.red          <- paste(filepathtoshapes,"Red_Line.shp",sep="/")
lrt.orange       <- paste(filepathtoshapes,"Orange_2035MR.shp",sep="/")
lrt.OrangeEx     <- paste(filepathtoshapes,"OrangeLine_SR134_Ext.shp",sep="/")
lrt.NS           <- paste(filepathtoshapes,"NS_Line.shp",sep="/")
lrt.NSExt        <- paste(filepathtoshapes,"Gold_Line_Ext.shp",sep="/")
lrt.EW           <- paste(filepathtoshapes,"EW_Line_Alt5.shp",sep="/")

# C1
lrt.crenshaw     <- paste(filepathtoshapes,"Crenshaw_Norwalk_C1.shp",sep="/")
lrt.greenlax     <- paste(filepathtoshapes,"Green_LAX_Torrance.shp",sep="/")
# C2
# lrt.crenshaw     <- paste(filepathtoshapes,"Crenshaw_Torrance_C2.shp",sep="/")
# lrt.greenlax     <- paste(filepathtoshapes,"Green_LAX_Norwalk.shp",sep="/")

lrt.laxpmv       <- paste(filepathtoshapes,"LAX_APM.shp",sep="/")
lrt.VanNuys      <- paste(filepathtoshapes,"VanNuys_LRT.shp",sep="/")
lrt.VerMont      <- paste(filepathtoshapes,"Vermont_BRT.shp",sep="/")
lrt.WSAB         <- paste(filepathtoshapes,"WSAB_Line_MOS3.shp",sep="/")
lrt.Sepulveda    <- paste(filepathtoshapes,"Sepulveda_HRT_Phase2.shp",sep="/")

lrt.Vicente      <- paste(filepathtoshapes,"Vicente_Stations.shp",sep="/")
lrt.Fairfax      <- paste(filepathtoshapes,"Fairfax_Stations.shp",sep="/")
lrt.Fairfax      <- paste(filepathtoshapes,"Firefax_Stations.shp",sep="/")

#lrt.GoldEx       <- paste(filepathtoshapes,"2035_Files/Gold_Line_Ext.shp",sep="/")
#lrt.GGSTAshape   <- paste(filepathtoshapes,"2035_Files/GoldGreenExt_Sta.shp",sep="/")
# lrt.URSTAshape   <- paste(filepathtoshapes,"2035_Files/2035_Stations_V1.shp",sep="/") #Previous Station File: deleted EW stations
# lrt.URSTAshapeHR <- paste(filepathtoshapes,"2035_Files/2035_ex_station_V2.shp",sep="/")#Horizon Year Station File (extra stations)
# lrt.metrolink    <- paste(filepathtoshapes,"Metrolinks/Metrolink_SP.shp",sep="/")
# lrt.CRSTAshape   <- paste(filepathtoshapes,"Metrolinks/MetrolinkStations_SP.shp",sep="/")

#----------------------------------------------------------------------------#

# Read in files, put in same order as shapefile
#
# Change 'zone' to match your data file's name for the geographic area
# Change 'CRCShape_$TAZ' to match your shapefile's name and geographic ID field
#
#----------------------------------------------------------------------------#

read.rcx <- function(fname) {
  print(paste('Reading row and column data in', fname))

  rcx <- read.table(paste(filepathtodata,fname,sep="/"), header=TRUE)
  rcx <- rcx[order(rcx$zone),]
  row.names(rcx) <- rcx$zone
  rcx <- rcx[as.character(tazs.shape$TAZ),]

  rcx
}

#-----------------------------------------------------------------------------#
#
# Sum your market segment files into one per purpose
# Set number of files to number of market segments you have
# in brackets, [,2:3] means sum columns 2-3, change the 3 to your number of fields
#-----------------------------------------------------------------------------#

sum.four <- function(fname_seg1,fname_seg2,fname_seg3,fname_seg4) {
  sum.p  <- read.rcx(fname_seg1)
  table2 <- read.rcx(fname_seg2)
  table3 <- read.rcx(fname_seg3)
  table4 <- read.rcx(fname_seg4)
  sum.p[,2:5]  <- (sum.p[,2:5]+table2[,2:5]+table3[,2:5]+table4[,2:5])
  sum.p
}

sum.eight <- function(fname_seg1,fname_seg2,fname_seg3,fname_seg4,fname_seg5,fname_seg6,fname_seg7,fname_seg8) {
  sum.p  <- read.rcx(fname_seg1)
  table2 <- read.rcx(fname_seg2)
  table3 <- read.rcx(fname_seg3)
  table4 <- read.rcx(fname_seg4)
  table5 <- read.rcx(fname_seg5)
  table6 <- read.rcx(fname_seg6)
  table7 <- read.rcx(fname_seg7)
  table8 <- read.rcx(fname_seg8)
  sum.p[,2:5]  <- (sum.p[,2:5]+table2[,2:5]+table3[,2:5]+table4[,2:5]+table5[,2:5]+table6[,2:5]+table7[,2:5]+table8[,2:5])
  sum.p
}

#----------------------------------------------------------------------------#
# Begin custom function to make series of maps
#----------------------------------------------------------------------------#
palette <- c("#67000D", "#CB181D", "#FB6A4A", "white","#ADDD8E", "#41AB5D", "#004529")

ub.plot_rs <- function(All, mtitle, subtitle) {
  print(paste('plotting', mtitle))

  # This IF statement will plot all white if there are only 0's in your file
  values<-  All[c("rs5", "rs1", "zone")]
  values <- as.data.frame(values[order(-values$rs5),])
  if (max(values$rs5) > 0) {

    # If fta bin = FALSE you can set new break points manually below

    if (fta_bin == FALSE) {

      valueset <- as.data.frame(values)
      valuesubset <- as.data.frame(subset(valueset,valueset$rs5>0))
      valuesubset <- as.data.frame(valuesubset[order(-valuesubset$rs5),])
      #colnames(valuesubset$rs5) <- "values"

      valuesubset$cumulativesum<-cumsum(valuesubset$rs5)
      maxcumsum<-max(valuesubset$cumulativesum)
      valuesubset$cumulativepercent<-round(100*valuesubset$cumulativesum/maxcumsum)

      fortybreakpointcalc<-subset(valuesubset,cumulativepercent>=40)
      fortybreakpoint<-min(fortybreakpointcalc$cumulativepercent)

      thirtybreakpointcalc <- subset(valuesubset,cumulativepercent>=70)
      thirtybreakpoint <- min(thirtybreakpointcalc$cumulativepercent)

      tenbreakpointcalc <- subset(valuesubset,cumulativepercent>=90)
      tenbreakpoint <- min(tenbreakpointcalc$cumulativepercent)

      fortypercent <- subset(valuesubset,cumulativepercent==fortybreakpoint,select=c("rs5"))
      thirtypercent <- subset(valuesubset,cumulativepercent==thirtybreakpoint,select=c("rs5"))
      tenpercent <- subset(valuesubset,cumulativepercent==tenbreakpoint,select=c("rs5"))

      Firstbreak <- max(valuesubset$rs5)
      Secondbreak <- max(unique(fortypercent$rs5))
      Thirdbreak <- max(unique(thirtypercent$rs5))
      Fourthbreak <- max(unique(tenpercent$rs5))

      #palette <- c("#67000D", "#CB181D", "#FB6A4A", "white","#ADDD8E", "#41AB5D", "#004529")

      # compute bins *before* messing with values
      bins <- c((0-Firstbreak),(0-Secondbreak),(0-Thirdbreak),(0-Fourthbreak),
                Fourthbreak, Thirdbreak, Secondbreak, Firstbreak)

      df <- values
      df$plot.code <- cut(values$rs5, unique(bins), include.lowest=TRUE, labels=FALSE)
      df$plot.colors <- palette[df$plot.code]

      df$plot.bins <- bins[df$plot.code]

      LegendText. <- paste(bins[1:(length(bins) - 1)], bins[2:length(bins)],
                           sep=' - ')
     # LegendText2. <- c("Districts")
      #legend=c(LegendText.,LegendText2.)
      legend=LegendText.
      df$breaks <- legend[df$plot.code]

    }  else {


      #palette <- c("#67000D", "#CB181D", "#FB6A4A", "white","#ADDD8E", "#41AB5D", "#004529")


      # When FTA=FALSE, set your desired breakpoints here. If the number changes, so should
      # the number of colors in the palette

      bins <- c(min(-33400, values$rs5),-2000, -1000, -500, 500, 1000, 2000, max(values$rs5, 33400))

      df <- values
      df$plot.code <- cut(values$rs5, unique(bins), include.lowest=TRUE, labels=FALSE)
      df$plot.colors <- palette[df$plot.code]

      df$plot.bins <- bins[df$plot.code]

      LegendText. <- paste(bins[1:(length(bins) - 1)], bins[2:length(bins)],
                           sep=' - ')
      #LegendText2. <- c("Districts")
      #legend=c(LegendText.,LegendText2.)
      legend=LegendText.
      df$breaks <- legend[df$plot.code]


    }

  } else {

    df$plot.colors<-"white"
    Palette<-"white"
    df$bins<-0
    df$plot.bins<- 0
    df$plot.code <- 0
  }

  return(df)
}

#-----------------------------------------------------------------------------#
#For cs
#-----------------------------------------------------------------------------#

ub.plot_cs <- function(All, mtitle, subtitle) {
  print(paste('plotting', mtitle))

  # This IF statement will plot all white if there are only 0's in your file
  values<-  All[c("cs5", "cs1", "zone")]
  values <- as.data.frame(values[order(-values$cs5),])
  if (max(values$cs5) > 0) {

    # If fta bin = FALSE you can set new break points manually below

    if (fta_bin == FALSE) {

      valueset <- as.data.frame(values)
      valuesubset <- as.data.frame(subset(valueset,valueset$cs5>0))
      valuesubset <- as.data.frame(valuesubset[order(-valuesubset$cs5),])
      #colnames(valuesubset$rs5) <- "values"

      valuesubset$cumulativesum<-cumsum(valuesubset$cs5)
      maxcumsum<-max(valuesubset$cumulativesum)
      valuesubset$cumulativepercent<-round(100*valuesubset$cumulativesum/maxcumsum)

      fortybreakpointcalc<-subset(valuesubset,cumulativepercent>=40)
      fortybreakpoint<-min(fortybreakpointcalc$cumulativepercent)

      thirtybreakpointcalc <- subset(valuesubset,cumulativepercent>=70)
      thirtybreakpoint <- min(thirtybreakpointcalc$cumulativepercent)

      tenbreakpointcalc <- subset(valuesubset,cumulativepercent>=90)
      tenbreakpoint <- min(tenbreakpointcalc$cumulativepercent)

      fortypercent <- subset(valuesubset,cumulativepercent==fortybreakpoint,select=c("cs5"))
      thirtypercent <- subset(valuesubset,cumulativepercent==thirtybreakpoint,select=c("cs5"))
      tenpercent <- subset(valuesubset,cumulativepercent==tenbreakpoint,select=c("cs5"))

      Firstbreak <- max(valuesubset$cs5)
      Secondbreak <- max(unique(fortypercent$cs5))
      Thirdbreak <- max(unique(thirtypercent$cs5))
      Fourthbreak <- max(unique(tenpercent$cs5))

      #palette <- c("#67000D", "#CB181D", "#FB6A4A", "white","#ADDD8E", "#41AB5D", "#004529")

      # compute bins *before* messing with values
      bins <- c((0-Firstbreak),(0-Secondbreak),(0-Thirdbreak),(0-Fourthbreak),
                Fourthbreak, Thirdbreak, Secondbreak, Firstbreak)

      df <- values
      df$plot.code <- cut(values$cs5, unique(bins), include.lowest=TRUE, labels=FALSE)
      df$plot.colors <- palette[df$plot.code]

      df$plot.bins <- bins[df$plot.code]

      LegendText. <- paste(bins[1:(length(bins) - 1)], bins[2:length(bins)],
                           sep=' - ')
      #LegendText2. <- c("Districts")
      #legend=c(LegendText.,LegendText2.)
      legend= LegendText.
      df$breaks <- legend[df$plot.code]


    }  else {


      #palette <- c("#67000D", "#CB181D", "#FB6A4A", "white","#ADDD8E", "#41AB5D", "#004529")


      # When FTA=FALSE, set your desired breakpoints here. If the number changes, so should
      # the number of colors in the palette

      bins <- c(min(-33400, values$cs5),-2000, -1000, -500, 500, 1000, 2000, max(values$rs5, 33400))

      df <- values
      df$plot.code <- cut(values$cs5, unique(bins), include.lowest=TRUE, labels=FALSE)
      df$plot.colors <- palette[df$plot.code]

      df$plot.bins <- bins[df$plot.code]

      LegendText. <- paste(bins[1:(length(bins) - 1)], bins[2:length(bins)],
                           sep=' - ')
      #LegendText2. <- c("Districts")
      #legend=c(LegendText.,LegendText2.)
      legend= LegendText.
      df$breaks <- legend[df$plot.code]



    }

  } else {

    df$plot.colors<-"white"
    df$palette<-"white"
    df$bins<-0
    df$plot.bins<- 0
    df$plot.code <- 0
  }

  return(df)
}


#-----------------------------------------------------------------------------#
# Function tpo set Color Palette for maps
#-----------------------------------------------------------------------------#


ColorPalette <- function(taz){
  df_col_1 = select(taz, Color, Range) %>%
    st_drop_geometry() %>%
    unique()%>%
    mutate(Range = fct_inorder(Range))


  #colpal_1 = leaflet::colorFactor(palette = df_col_1$Color, levels = levels((df_col_1$Range)))
  #return(mapview(taz, zcol = "Range", col.regions = colpal_1(taz$Range)))
  return(df_col_1)

}

#-----------------------------------------------------------------------------#
#
# Load mapping features
#
#-----------------------------------------------------------------------------#

##POLYGONS##

#print(paste('Reading tazs in', tazs.shapefile))
tazs.shape <- read_sf(here::here("Shapefile/AMC_TAZ_Split_3017.shp"))
# mapview(tazs.shape, zcol ="Area")


print(paste('Reading district in', boundary.shapefile))
boundary.shape <- read_sf(boundary.shapefile) %>%
  mutate(Name = "Boundary Shape")

#---------------------------------------------------------------------------

##LINES##


# print(paste('Reading district in', district.shapefile))
# district.shape <- readShapePoly(district.shapefile)


list(
  c(lrt.NS, lrt.NSExt, lrt.red)
  ,c("gold", "gold", "red")) %>%
  pmap(function(x, z)
    read_sf(x) %>%
      select("geometry")%>%
      mutate(Name = "lrt.NS",
             color = z)%>%
      st_transform(crs =4326)
  )


print(paste('Reading lrt lines in', lrt.NS))
lrt.NSline <- read_sf(lrt.NS) %>%
  select("geometry")%>%
  mutate(Name = "lrt.NS",
         color = "gold")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.NSExt))
lrt.NSExtline <- read_sf(lrt.NSExt)%>%
  select("geometry")%>%
  mutate(Name = "lrt.NSExt",
         color = "gold")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.red))
lrt.redline <- read_sf(lrt.red)%>%
  select("geometry")%>%
  mutate(Name = "lrt.red",
         color = "red")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.orange))
lrt.orangeline <- read_sf(lrt.orange)%>%
  select("geometry")%>%
  mutate(Name = "lrt.orange",
         color = "orange")%>%
  st_transform(crs =4326)

# print(paste('Reading lrt lines in', lrt.green))
# lrt.greenline <- readShapeLines(lrt.green)

print(paste('Reading lrt lines in', lrt.greenlax))
lrt.greenlaxline <- read_sf(lrt.greenlax)%>%
  select("geometry")%>%
  mutate(Name = "lrt.greenlax",
         color = "green")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.EW))
lrt.EWline <- read_sf(lrt.EW)%>%
  select("geometry")%>%
  mutate(Name = "lrt.EW",
         color = "blue")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.purple_mos1))
lrt.purpleline_mos1 <- read_sf(lrt.purple_mos1)%>%
  select("geometry")%>%
  mutate(Name = "lrt.purple_mos1",
         color = "purple")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.purple_mos2))
lrt.purpleline_mos2 <- read_sf(lrt.purple_mos2)%>%
  select("geometry")%>%
  mutate(Name = "lrt.purple_mos2",
         color = "purple")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.purple_mos3))
lrt.purpleline_mos3 <- read_sf(lrt.purple_mos3)%>%
  select("geometry")%>%
  mutate(Name = "lrt.purple_mos3",
         color = "purple")%>%
  st_zm(drop=TRUE, what = "ZM")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.crenshaw))
lrt.crenshawline <- read_sf(lrt.crenshaw)%>%
  select("geometry")%>%
  mutate(Name = "lrt.crenshaw",
         color = "magenta4")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.laxpmv))
lrt.laxpmvline <- read_sf(lrt.laxpmv)%>%
  select("geometry")%>%
  mutate(Name = "lrt.laxpmv",
         color = "gold3")%>%
  st_transform(crs =4326)

# print(paste('Reading lrt lines in', lrt.metrolink))
# lrt.Metrolinkline <- readShapeLines(lrt.metrolink)

print(paste('Reading lrt lines in', lrt.VanNuys))
lrt.VanNuysline <- read_sf(lrt.VanNuys)%>%
  select("geometry")%>%
  mutate(Name = "lrt.VanNuys",
         color = "hotpink4")%>%
  st_zm(drop=TRUE, what = "ZM")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.VerMont))
lrt.VerMontline <- read_sf(lrt.VerMont)%>%
  select("geometry")%>%
  mutate(Name = "lrt.VerMont",
         color = "lightpink3")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.WSAB))
lrt.WSABline <- read_sf(lrt.WSAB)%>%
  select("geometry")%>%
  mutate(Name = "lrt.WSAB",
         color = "lightcoral")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.Sepulveda))
lrt.Sepulvedaline <- read_sf(lrt.Sepulveda)%>%
  select("geometry")%>%
  mutate(Name = "lrt.Sepulveda",
         color = "sienna4")%>%
  st_transform(crs =4326)

print(paste('Reading lrt lines in', lrt.OrangeEx))
lrt.OrangeExline <- read_sf(lrt.OrangeEx)%>%
  select("geometry")%>%
  mutate(Name = "lrt.OrangeEx",
         color = "orange")%>%
  st_transform(crs =4326)

#print(paste('Reading lrt lines in', lrt.GreenEx))
#lrt.GreenEx <- readShapePoints(lrt.GreenEx)

#print(paste('Reading lrt lines in', lrt.GoldEx))
#lrt.GoldEx <- readShapePoints(lrt.GoldEx)

#print(paste('Reading lrt lines in', lrt.GGSTAshape))
#lrt.GGSTA <- readShapePoints(lrt.GGSTAshape)

#print(paste('Reading lrt lines in', lrt.URSTAshape))
#lrt.URSTA <- readShapePoints(lrt.URSTAshape)

#print(paste('Reading lrt lines in', lrt.URSTAshapeHR))
#lrt.URSTAHR <- readShapePoints(lrt.URSTAshapeHR)

#print(paste('Reading lrt lines in', lrt.CRSTAshape))
#lrt.CRSTA <- readShapePoints(lrt.CRSTAshape)

print(paste('Reading lrt lines in', lrt.Vicente))
lrt.VicenteStop <- read_sf(lrt.Vicente)%>%
  select("geometry")%>%
  mutate(Name = "lrt.Vicente",
         color = "#FDD017")%>%
  st_transform(crs =4326)


#-----------------Joining all Lines into one data frame----------------------#

All_lines <- rbind(lrt.purpleline_mos1,
                   lrt.purpleline_mos2,
                   lrt.purpleline_mos3,
                   lrt.redline,
                   lrt.NSline,
                   lrt.NSExtline,
                   lrt.EWline,
                   lrt.greenlaxline,
                   lrt.orangeline,
                   lrt.crenshawline,
                   lrt.laxpmvline,
                   lrt.VanNuysline,
                   lrt.VerMontline,
                   lrt.WSABline,
                   lrt.Sepulvedaline,
                   lrt.OrangeExline)

Points <- lrt.VicenteStop


#----------------------------------------------------------------------------#
# Calls to PlotData functions
#----------------------------------------------------------------------------#

# Add all the files you want to read in here

rcs.data_hbwpk  <- read.rcx("Peak/hbwpk.rcs")
rcs.data_hbupk  <- read.rcx("Peak/hbupk.rcs")

#-----------------------------------------------------------------------------------#


##UBMaps_C1_Vicente_vs_C1_Fairfax_Peak##

hbwpk_rs5 <- ub.plot_rs(rcs.data_hbwpk[c("rs5", "rs1", "zone")],"Home Based Work Peak","Productions")%>%
  rename("Person trips" = "rs1",
         "User Benefits (mins)" = "rs5",
         "TAZ" = "zone",
         "Color" = "plot.colors",
         "Range" = "breaks")%>%
  mutate(Purpose ="Home Based Work - Productions",
         "Time Period" = 'Peak')

# hbwpk_cs5 <- ub.plot_cs(rcs.data_hbwpk[c("cs5", "cs1", "zone")],"Home Based Work Peak","Attractions")%>%
#                 rename("Person trips" = "cs1",
#                        "User Benefits (mins)" = "cs5",
#                        "TAZ" = "zone",
#                        "Color" = "plot.colors",
#                        "Range" = "breaks")%>%
#                   mutate(Purpose ="Home Based Work - Attractions",
#                          "Time Period" = 'Peak')
#
#
# hbupk_rs5 <- ub.plot_rs(rcs.data_hbupk[c("rs5", "rs1", "zone")],"Home Based University Peak","Productions")%>%
#                 rename("Person trips" = "rs1",
#                        "User Benefits (mins)" = "rs5",
#                        "TAZ" = "zone",
#                        "Color" = "plot.colors",
#                        "Range" = "breaks")%>%
#                   mutate(Purpose ="Home Based University - Productions",
#                          "Time Period" = 'Peak')
#
# hbupk_cs5 <- ub.plot_cs(rcs.data_hbupk[c("cs5", "cs1", "zone")],"Home Based University Peak","Attractions")%>%
#                 rename("Person trips" = "cs1",
#                        "User Benefits (mins)" = "cs5",
#                        "TAZ" = "zone",
#                        "Color" = "plot.colors",
#                        "Range" = "breaks")%>%
#                   mutate(Purpose ="Home Based University - Attractions",
#                          "Time Period" = 'Peak')



##TAZ transformations##

tazs.shape_hbwpk_rs5 =  merge(x = tazs.shape, y = hbwpk_rs5, by = c("TAZ"))  %>%
                        st_transform(crs =4326)

tazs.shape_hbwpk_rs5_col = ColorPalette(tazs.shape_hbwpk_rs5)

colpal_hbwpk_rs5 = leaflet::colorFactor(palette = tazs.shape_hbwpk_rs5_col$Color, levels = levels((tazs.shape_hbwpk_rs5_col$Range)))


# tazs.shape_hbwpk_cs5 =  merge(x = tazs.shape, y = hbwpk_cs5, by = c("TAZ"))  %>%
#                         st_transform(crs =4326)
#
# tazs.shape_hbwpk_cs5_col = ColorPalette(tazs.shape_hbwpk_cs5)
#
# colpal_hbwpk_cs5 = leaflet::colorFactor(palette = tazs.shape_hbwpk_cs5_col$Color, levels = levels((tazs.shape_hbwpk_cs5_col$Range)))
#
#
#
# tazs.shape_hbupk_rs5 =  merge(x = tazs.shape, y = hbupk_rs5, by = c("TAZ"))  %>%
#                         st_transform(crs =4326)
#
# tazs.shape_hbupk_rs5_col = ColorPalette(tazs.shape_hbupk_rs5)
#
# colpal_hbupk_rs5 = leaflet::colorFactor(palette = tazs.shape_hbupk_rs5_col$Color, levels = levels((tazs.shape_hbupk_rs5_col$Range)))
#
#
#
# tazs.shape_hbupk_cs5 =  merge(x = tazs.shape, y = hbupk_cs5, by = c("TAZ"))  %>%
#                         st_transform(crs =4326)
#
# tazs.shape_hbupk_cs5_col = ColorPalette(tazs.shape_hbupk_cs5)
#
# colpal_hbupk_cs5 = leaflet::colorFactor(palette = tazs.shape_hbupk_cs5_col$Color, levels = levels((tazs.shape_hbupk_cs5_col$Range)))


#--------------------------------------------------------------------------------#
#------------------------PLOTTING MAPS--------------------------------------------#
#--------------------------------------------------------------------------------#

popup = c("District","Person trips","User Benefits (mins)", "Purpose", "Time Period")
#cntr_crds <- c(34.008308, -118.162090)

#coordinates(cntr_crds) <- cbind(cntr_crds[1] , cntr_crds[2])
#----------------Peak Period-----------------------------------------------------#

Peak_maps <-  mapview(tazs.shape_hbwpk_rs5,
                    zcol = 'Range',
                    col.region = colpal_hbwpk_rs5(tazs.shape_hbwpk_rs5$Range),
                    layer.name = str_glue("HBW - Production User Benefits (mins)"),
                    #map.types = "OpenStreetMap",
                    popup = popup) #+
  # mapview(tazs.shape_hbwpk_cs5,
  #         zcol = "Range",
  #         col.region = colpal_hbwpk_cs5(tazs.shape_hbwpk_cs5$Range),
  #         layer.name = str_glue("HBW - Attraction User Benefits (mins)"),
  #         popup = popup,
  #         hide = TRUE) +
  # mapview(tazs.shape_hbupk_rs5,
  #         zcol = "Range",
  #         col.region = colpal_hbupk_rs5(tazs.shape_hbupk_rs5$Range),
  #         layer.name = str_glue("HBU - Production User Benefits (mins)"),
  #         popup = popup,
  #         hide = TRUE) +
  # mapview(tazs.shape_hbupk_cs5,
  #         zcol = "Range",
  #         col.region = colpal_hbupk_cs5(tazs.shape_hbupk_cs5$Range),
  #         layer.name = str_glue("HBU - Attraction User Benefits (mins)"),
  #         popup = popup,
  #         hide = TRUE)




  mapview(All_lines,
          color = All_lines$color,
          alpha.regions = 0,
          layer.name = "Lines",
          lwd = 2, legend = FALSE)+
  mapview(Points,
          color = "#FDD017",
          legend = FALSE)
