denmap=function(RA_data, Dec_data, pri_data=pri_base, RAlo=157.3, RAhi=225, Declo=-4, Dechi=4, grid=0.05, rad=grid, pri_base=0){
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    RA_data=as.matrix(RA_data)
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }
  if(length(pri_data)==1){pri_data=rep(pri_data, length(RA_data))}
  if(length(pri_data)!=length(RA_data)){stop('Length of data inputs do not match!')}

  if(RAlo>RAhi){
    RA_data[RA_data>RAhi]=RA_data[RA_data>RAhi]-360
    RAlo=RAlo-360
  }

  #Check assertions:
  assertNumeric(RA_data)
  assertNumeric(Dec_data)
  assertIntegerish(pri_data)
  assertScalar(RAlo)
  assertScalar(RAhi)
  assertScalar(Declo)
  assertScalar(Dechi)
  assertScalar(grid)
  assertScalar(rad)
  assertInt(pri_base)

  RAseq=seq(RAlo,RAhi,by=grid)
  Decseq=seq(Declo,Dechi,by=grid)
  tempgrid=expand.grid(RAseq,Decseq)

  select=selectRADec(RA_data,Dec_data,RAlo=RAlo,RAhi=RAhi,Declo=Declo,Dechi=Dechi)$select
  RA_data=RA_data[select]
  Dec_data=Dec_data[select]
  pri_data=pri_data[select]
  tempdata=cbind(RA_data, Dec_data)
  tempdata=tempdata[pri_data>=pri_base,]
  denmat=coordmatch(tempgrid, tempdata, rad=rad, radunit='deg', kstart = 100, smallapprox=TRUE)$Nmatch
  invisible(list(x=RAseq, y=Decseq, z=matrix(denmat, nrow=length(RAseq))))
}
