#This is the fibers geometry file for AESOP
#This file was first written at: Tue Apr  5 16:30:01 CEST 2016
#Original filename: /home/tdwelly/4most/OpSim/inputs_to_system_model/AESOP/info_2016-04-05/aesop-cfg.json
#This file contains one entry per Fiber/Positioner, and gives the coordinates and details of each fiber
#Other info:
#Other info:
#Plate scale: 0.0594 mm/asec
#Closest pitch between fibres: 9.542mm (160.6397 asec)
#Fibre size is 0.085 (1.430976 asec)
#FoV is diameter: 535mm (2.501871 deg)
#Radius of curvature at focal plane: 4,653 mm
#3D avoidance below the focal plane is 0.1mm (1.683502 asec) ignoring the fibre radius
#Tip avoidance on the focal plane is 0.9mm (15.15152 asec)
#Spine length is 250mm
#Patrol requirement is 11.5mm (193.6027 asec), but in practice they expect 12.3mm (207.0707 asec) +/- 0.5 (8.417508 asec), i.e. 11.8mm (198.6532 asec) - 12.8mm (215.4882 asec)
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

.onLoad <- function(libname, pkgname){
  platescale=0.0594; patrol_mm=11.5
  x0_asec=x0_mm=y0_asec=y0_mm=x0_rad=y0_rad=patrol_asec=NULL
  AESOP=NULL
  utils::data('AESOP', package='Greedy4MOST', envir = environment())
  AESOP=as.data.table(AESOP)
  AESOP$Spectro=as.character(AESOP$Spectro)
  AESOP[,x0_asec:=x0_mm/platescale]
  AESOP[,y0_asec:=y0_mm/platescale]
  AESOP[,x0_rad:=x0_mm*pi/180/3600/platescale]
  AESOP[,y0_rad:=y0_mm*pi/180/3600/platescale]
  AESOP[,patrol_mm:=patrol_mm]
  AESOP[,patrol_asec:=patrol_mm/platescale]
  assign("GreedyEnv", new.env(), .GlobalEnv)
  assign("AESOP_fibres", AESOP, envir=GreedyEnv)
  assign("AESOP_platescale", platescale, envir=GreedyEnv)
  assign("AESOP_fibrad_mm", 0.085, envir=GreedyEnv)
}
