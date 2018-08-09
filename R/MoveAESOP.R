MoveAESOP=function(RA_AESOP=0, Dec_AESOP=0){
  #Check assertions:
  assertScalar(RA_AESOP)
  assertScalar(Dec_AESOP)

  x0_rad=y0_rad=NULL
  rotdec=rotate3d(as.matrix(cbind(GreedyEnv$AESOP_fibres[,list(x0_rad,y0_rad)],1)), x=1, y=0, z=0, angle=pi/2-Dec_AESOP*pi/180)
  rotra=rotate3d(rotdec, x=0, y=0, z=1, angle=pi/2-RA_AESOP*pi/180)
  sph=car2sph(rotra,deg = TRUE)
  sph[,'long']=sph[,'long'] %% 360
  colnames(sph)=c('RA', 'Dec', 'radius')
  invisible(list(car=rotra, sph=sph[,1:2], RA_AESOP=RA_AESOP, Dec_AESOP=Dec_AESOP))
}
