Greedy4MOST=function(tiles=1:10, RA_data, Dec_data, pri_data, T_data, T_AESOP=20, RAlo=157.3, RAhi=225, Declo=-4, Dechi=4, grid=0.1, Nsamp=1e4, rad=sqrt(4.06/pi), pri_base=100, verbose=TRUE){
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    RA_data=as.matrix(RA_data)
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }
  if(length(T_data)==1){T_data=rep(T_data, length(RA_data))}
  if(length(T_data)!=length(RA_data)){stop('Length of data inputs do not match!')}

  T_data[T_data<0]=0

  Ndata=length(pri_data)

  tileout={}
  fibreout={}

  for(i in tiles){
    tempTile=TileAESOP(RA_data=RA_data, Dec_data=Dec_data, weight_data=T_data, RAlo = RAlo, RAhi = RAhi, Declo = Declo, Dechi = Dechi, grid=grid, Nsamp=Nsamp, rad=rad)$useloc
    if(verbose){
      message(paste(c(i, tempTile),collapse = ' '))
    }
    tempFib=FibreAESOP(RA_data=RA_data, Dec_data=Dec_data, RA_AESOP=tempTile[1], Dec_AESOP=tempTile[2], pri_data=pri_data)

    tileout=rbind(tileout, cbind(Tile=i, RA_AESOP=tempTile[1], Dec_AESOP=tempTile[2]))
    fibreout=rbind(fibreout,cbind(Tile=i, tempFib$best_fib_lo))

    T_data[tempFib$best_fib_lo$galaxyID]=T_data[tempFib$best_fib_lo$galaxyID]-T_AESOP
    pri_data[1:Ndata %in% tempFib$best_fib_lo$galaxyID & T_data<=0 & pri_data<pri_base]=pri_data[1:Ndata %in% tempFib$best_fib_lo$galaxyID & T_data<=0 & pri_data<pri_base]-1
    pri_data[1:Ndata %in% tempFib$best_fib_lo$galaxyID & T_data<=0 & pri_data>=pri_base]=pri_base-10
  }

invisible(list(data=data.table(RA_data=RA_data, Dec_data=Dec_data, pri_data=pri_data, T_data=T_data), fibreout=as.data.table(fibreout), tileout=as.data.table(tileout)))
}