Greedy4MOST=function(tiles=1:10, RA_data, Dec_data, pri_data, T_data, weight_data='T_data', T_AESOP=20, RAlo=157.3, RAhi=225, Declo=-4, Dechi=4, grid=0.1, Nsamp=1e4, rad=sqrt(4.06/pi), pri_base=0, verbose=TRUE, seed=Sys.time()){
  if(!missing(seed)){
    set.seed(as.integer(seed))
  }
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    RA_data=as.matrix(RA_data)
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }
  if(length(pri_data)==1){T_data=rep(pri_data, length(RA_data))}
  if(length(pri_data)!=length(RA_data)){stop('Length of pri_data inputs do not match!')}
  if(length(T_data)==1){T_data=rep(T_data, length(RA_data))}
  if(length(T_data)!=length(RA_data)){stop('Length of T_data inputs do not match!')}

  #Check assertions:
  assertIntegerish(tiles)
  assertNumeric(RA_data)
  assertNumeric(Dec_data)
  assertIntegerish(pri_data)
  assertNumeric(T_data)
  if(length(weight_data)==1){
    assertCharacter(weight_data)
  }else{
    assertNumeric(weight_data)
  }
  assertNumeric(T_AESOP)
  assertScalar(RAlo)
  assertScalar(RAhi)
  assertScalar(Declo)
  assertScalar(Dechi)
  assertScalar(grid)
  assertInt(Nsamp)
  assertScalar(rad)
  assertInt(pri_base)
  assertFlag(verbose)
  assertScalar(seed)

  T_data[T_data<0]=0

  Ndata=length(pri_data)

  tileout={}
  fibreout={}
  success=rep(0,length(RA_data))

  for(i in tiles){
    if(RAlo==RAhi & Declo==Dechi){
      tempTile=c(RAlo, Declo)
    }else{
      if(weight_data[1]=='T_data'){
        tempTile=TileAESOP(RA_data=RA_data, Dec_data=Dec_data, weight_data=T_data, RAlo = RAlo, RAhi = RAhi, Declo = Declo, Dechi = Dechi, grid=grid, Nsamp=Nsamp, rad=rad)$useloc
      }else if(weight_data[1]=='pri_data'){
        tempTile=TileAESOP(RA_data=RA_data, Dec_data=Dec_data, weight_data=pri_data, RAlo = RAlo, RAhi = RAhi, Declo = Declo, Dechi = Dechi, grid=grid, Nsamp=Nsamp, rad=rad)$useloc
      }else{
        tempTile=TileAESOP(RA_data=RA_data, Dec_data=Dec_data, weight_data=weight_data, RAlo = RAlo, RAhi = RAhi, Declo = Declo, Dechi = Dechi, grid=grid, Nsamp=Nsamp, rad=rad)$useloc
      }
    }

    if(verbose){
      message(paste(c(i, round(tempTile,2)),collapse = ' '))
    }
    tempFib=FibreAESOP(RA_data=RA_data, Dec_data=Dec_data, RA_AESOP=tempTile[1], Dec_AESOP=tempTile[2], pri_data=pri_data)

    tileout=rbind(tileout, cbind(Tile=i, RA_AESOP=tempTile[1], Dec_AESOP=tempTile[2]))
    fibreout=rbind(fibreout,cbind(Tile=i, tempFib$best_fib_lo))

    T_data[tempFib$best_fib_lo$galaxyID]=T_data[tempFib$best_fib_lo$galaxyID]-T_AESOP
    pri_data[1:Ndata %in% tempFib$best_fib_lo$galaxyID & T_data<=0 & pri_data<=pri_base]=pri_data[1:Ndata %in% tempFib$best_fib_lo$galaxyID & T_data<=0 & pri_data<=pri_base]-1
    pri_data[1:Ndata %in% tempFib$best_fib_lo$galaxyID & T_data<=0 & pri_data>pri_base]=pri_base
    success[success==0 & pri_data<=pri_base]=i
  }

invisible(list(data=data.table(RA_data=RA_data, Dec_data=Dec_data, pri_data=pri_data, T_data=T_data, success=success), fibreout=as.data.table(fibreout), tileout=as.data.table(tileout)))
}
