#This is the fibers geometry file for AESOP
#This file was first written at: Tue Apr  5 16:30:01 CEST 2016
#Original filename: /home/tdwelly/4most/OpSim/inputs_to_system_model/AESOP/info_2016-04-05/aesop-cfg.json
#This file contains one entry per Fiber/Positioner, and gives the coordinates and details of each fiber
#Other info:
#Pitch is 9.542mm between fibres
#Fibre size is 0.085, so 1.43 asec
#Plate scale is 0.0594 mm/asec
#FoV is 535mm = 2.502 degrees diameter
#Rad curve 4,653 mm
#3D avoidance is 0.1mm
#Tip avoidance is 0.9mm
#Spine length is 250mm
#Patrol requirement is 11.5mm, but in practice they expect 12.3mm +/- 0.5 (so 11.8-12.8mm)
#Description of columns:
#Index  Name        Format    Description
#-----  ----------- --------- --------------------------------------------------------------------------------
#1      id          integer   Fiber identification number
#2      x0_mm       float     offset of the fiber base position from the FoV centre in the x-direction (mm)
#3      y0_mm       float     offset of the fiber base position from the FoV centre in the x-direction (mm)
#4      res         integer   code giving the resolution code of the fiber (1=low res, 2=high res)
#5      sector      integer   ID of the sky fiber allocation sector that this fiber lies within
#6      EdgeFlag    integer   Flag set if the fiber is at edge of field (0=middle;1=border;2=edge;3=corner)
#7      Spectro     string    HR, LR-A, LR-B
#8      Slitlet     integer   Slitlet within spectrograph
#9      Slitpos     integer   Slitpos within slitlet
#-----  ----------- --------- --------------------------------------------------------------------------------

.onLoad <- function(libname, pkgname, platescale=0.0594, patrol_mm=11.5){
  data('AESOP')
  AESOP=as.data.table(AESOP)
  AESOP$Spectro=as.character(AESOP$Spectro)
  AESOP[,x0_asec:=x0_mm/platescale]
  AESOP[,y0_asec:=y0_mm/platescale]
  AESOP[,x0_rad:=x0_mm*pi/180/3600/platescale]
  AESOP[,y0_rad:=y0_mm*pi/180/3600/platescale]
  AESOP[,patrol_mm:=patrol_mm]
  AESOP[,patrol_asec:=patrol_mm/platescale]
  assign("AESOP_fibres", AESOP, envir=globalenv())
  assign("AESOP_platescale", platescale, envir=globalenv())
  assign("AESOP_fibrad_mm", 0.085, envir=globalenv())
}
