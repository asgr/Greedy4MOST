selectRADec=function(RA, Dec, RAlo=0, RAhi=90, Declo=-10, Dechi=10){
  if (is.matrix(RA) || is.data.frame(RA)) {
    Dec = RA[, 2]
    RA = RA[, 1]
  }

  RA=RA %% 360
  RAlo=RAlo %% 360
  RAhi=RAhi %% 360

  if(RAlo<RAhi){
    RAselect = RA>RAlo & RA<RAhi
  }else{
    RAselect = RA>RAlo | RA<RAhi
  }

  if(Declo<Dechi){
    Decselect = Dec>Declo & Dec<Dechi
  }else{
    Decselect = Dec>Declo | Dec<Dechi
  }

  select=which(RAselect & Decselect)

  invisible(list(data=cbind(RA=RA[select], Dec=Dec[select]), select=select))
}
