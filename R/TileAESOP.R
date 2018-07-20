TileAESOP=function(RA_data, Dec_data, RAlo = 129, RAhi = 141, Declo = -2, Dechi = 3, grid=0.1, Nsamp=1e4, rad=sqrt(4.06/pi)){
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }

  tempgrid=expand.grid(seq(RAlo,RAhi,by=grid),seq(Declo,Dechi,by=grid))
  tempdata=cbind(RA_data, Dec_data)
  select=selectRADec(RA_data,Dec_data,RAlo=RAlo,RAhi=RAhi,Declo=Declo,Dechi=Dechi)$select
  Nsamp=min(Nsamp, length(RA_data))
  select=select[sample(length(select),Nsamp)]
  tempdata=tempdata[select,]
  Nmatch=coordmatch(tempgrid, tempdata, rad=rad, radunit='deg', kstart = ceiling(Nsamp/100))$Nmatch
  Nmatch[!is.finite(Nmatch)]=0
  return=list(useloc=as.numeric(tempgrid[which.max(Nmatch),]), grid=cbind(tempgrid,Nmatch))
}
