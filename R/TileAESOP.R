TileAESOP=function(RA_data, Dec_data, weight_data=1, RAlo = 129, RAhi = 141, Declo = -2, Dechi = 3, grid=0.1, Nsamp=1e4, rad=sqrt(4.06/pi)){
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    RA_data=as.matrix(RA_data)
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }
  if(length(weight_data)==1){weight_data=rep(weight_data, length(RA_data))}
  if(length(weight_data)!=length(RA_data)){stop('Length of data inputs do not match!')}

  weight_data[weight_data<0]=0

  tempgrid=expand.grid(seq(RAlo,RAhi,by=grid),seq(Declo,Dechi,by=grid))
  tempdata=cbind(RA_data, Dec_data)
  select=selectRADec(RA_data,Dec_data,RAlo=RAlo,RAhi=RAhi,Declo=Declo,Dechi=Dechi)$select
  Nsamp=min(Nsamp, length(RA_data))
  select=select[sample(length(select),Nsamp)]
  tempdata=tempdata[select,]
  weight_data=weight_data[select]
  match_mat=coordmatch(tempgrid, tempdata, rad=rad, radunit='deg', kstart = ceiling(Nsamp/100))$ID
  match_mat[match_mat>0]=weight_data[match_mat]
  Wmatch=rowSums(match_mat, na.rm=TRUE)
  Wmatch[!is.finite(Wmatch)]=0
  return=list(useloc=as.numeric(tempgrid[which.max(Wmatch),]), grid=cbind(tempgrid,Wmatch))
}
