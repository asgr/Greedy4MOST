FibreAESOP=function(RA_data, Dec_data, RA_AESOP=0, Dec_AESOP=0, pri_data=9, res_data='lo', assign_AESOP=TRUE, avoid=0.1, fibs=c('LR-A', 'LR-B')){
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }

  #Create reference list
  ref=1:length(RA_data)
  if(length(pri_data)==1){pri_data=rep(pri_data,length(RA_data))}
  if(length(assign_AESOP)==1){assign_AESOP=rep(assign_AESOP,dim(AESOP_fibres)[1])}
  if(length(pri_data) != length(RA_data)){stop('pri_data is not the right length')}
  if(length(assign_AESOP) != dim(AESOP_fibres)[1]){stop('assign_AESOP is not the right length')}

  AESOP_pos=MoveAESOP(RA_AESOP=RA_AESOP, Dec_AESOP=Dec_AESOP)
  data_car=sph2car(RA_data, Dec_data)
  #select1=which(RA_data>min(AESOP_pos[,'RA']) & RA_data<max(AESOP_pos[,'RA']) & Dec_data>min(AESOP_pos[,'Dec']) & Dec_data<max(AESOP_pos[,'Dec']))
  fibradmax=max(AESOP_fibres$patrol_asec)*pi/180/3600
  select1=data_car[,1]>min(AESOP_pos$car[,1]-fibradmax) &
          data_car[,1]<max(AESOP_pos$car[,1]+fibradmax) &
          data_car[,2]>min(AESOP_pos$car[,2]-fibradmax) &
          data_car[,2]<max(AESOP_pos$car[,2]+fibradmax) &
          data_car[,3]>min(AESOP_pos$car[,3]-fibradmax) &
          data_car[,3]<max(AESOP_pos$car[,3]+fibradmax)

  #Remake reference list based on selecting only objects near the tile

  ref=ref[select1]
  pri_data=pri_data[select1]

  intern_clean=coordmatch(cbind(RA_data[ref], Dec_data[ref]), rad=avoid/AESOP_platescale)
  if(length(intern_clean$bestmatch$refID)>0){
    keepLHS=pri_data[intern_clean$bestmatch$refID]>=pri_data[intern_clean$bestmatch$compareID]
    keep=c(intern_clean$bestmatch$refID[keepLHS], intern_clean$bestmatch$compareID[!keepLHS])
  }else{
    keep={}
  }

  select2=sort(c(which(intern_clean$ID[,1]==0),keep))

  #Remake reference list based on avoiding lower ranked objects that are within the distance specified by 'avoid'.

  ref=ref[select2]
  pri_data=pri_data[select2]

  select_AESOP=which(AESOP_fibres$Spectro %in% fibs & assign_AESOP)

  pri_data_list=sort(unique(pri_data),decreasing = T)
  select_AESOP_temp=select_AESOP
  best_fib={}

  for(pri in pri_data_list){
    reftemp=ref[pri_data==pri]
    match_fib=coordmatch(AESOP_pos$sph[select_AESOP_temp,], cbind(RA_data[reftemp], Dec_data[reftemp]), rad=AESOP_fibres$patrol_asec[select_AESOP_temp])
    best_fib=rbind(best_fib, data.table(fibreID=select_AESOP_temp[match_fib$bestmatch$refID], galaxyID=reftemp[match_fib$bestmatch$compareID], sep=match_fib$bestmatch$sep))
    select_AESOP_temp=select_AESOP_temp[-match_fib$bestmatch$refID]
    if(length(select_AESOP_temp)==0){break}
  }


  setorder(best_fib, fibreID)

  data_pos=data.table(RA_data=RA_data[best_fib$galaxyID], Dec_data=Dec_data[best_fib$galaxyID])

  invisible(list(AESOP_pos=AESOP_pos$sph[best_fib$fibreID,], data_pos=data_pos, best_fib=best_fib))
}
