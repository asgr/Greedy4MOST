FibreAESOP=function(RA_data, Dec_data, RA_AESOP=0, Dec_AESOP=0, pri_data=9, res_data='lo', assign_AESOP=TRUE, avoid=0.9){
  if (is.matrix(RA_data) || is.data.frame(RA_data)) {
    Dec_data = RA_data[, 2]
    RA_data = RA_data[, 1]
  }

  #Create reference list
  ref=1:length(RA_data)

  if(length(pri_data)==1){pri_data=rep(pri_data,length(RA_data))}
  if(length(res_data)==1){res_data=rep(res_data,length(RA_data))}
  if(length(assign_AESOP)==1){assign_AESOP=rep(assign_AESOP,dim(AESOP_fibres)[1])}


  if(length(pri_data) != length(RA_data)){stop('pri_data is not the right length')}
  if(length(assign_AESOP) != dim(AESOP_fibres)[1]){stop('assign_AESOP is not the right length')}

  pri_data_orig=pri_data

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
  res_data=res_data[select1]

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
  res_data=res_data[select2]

  pri_data_list=sort(unique(pri_data),decreasing = T)

  check=1:length(ref)

  while(length(check)>0){ #This loop is here to make sure we don't have any bad 3D solutions
    best_fib_lo={}
    best_fib_hi={}
    select_AESOP_lo=which(assign_AESOP & AESOP_fibres$Spectro %in% c('LR-A', 'LR-B'))
    select_AESOP_hi=which(assign_AESOP & AESOP_fibres$Spectro %in% c('HR'))
    for(pri in pri_data_list){
      reftemp=ref[pri_data==pri & res_data %in% 'lo' & 1:length(ref) %in% check]
      if(length(reftemp)==0){break}
      match_fib=coordmatch(AESOP_pos$sph[select_AESOP_lo,,drop=FALSE], cbind(RA_data[reftemp], Dec_data[reftemp]), rad=AESOP_fibres$patrol_asec[select_AESOP_lo])
      best_fib_lo=rbind(best_fib_lo, data.table(fibreID=select_AESOP_lo[match_fib$bestmatch$refID], galaxyID=reftemp[match_fib$bestmatch$compareID], sep=match_fib$bestmatch$sep))
      select_AESOP_lo=select_AESOP_lo[-match_fib$bestmatch$refID]
      if(length(select_AESOP_lo)==0){setorder(best_fib_lo, fibreID); break}
    }

    for(pri in pri_data_list){
      reftemp=ref[pri_data==pri & res_data %in% 'hi']
      if(length(reftemp)==0){break}
      match_fib=coordmatch(AESOP_pos$sph[select_AESOP_hi,], cbind(RA_data[reftemp], Dec_data[reftemp]), rad=AESOP_fibres$patrol_asec[select_AESOP_hi])
      best_fib_hi=rbind(best_fib_hi, data.table(fibreID=select_AESOP_hi[match_fib$bestmatch$refID], galaxyID=reftemp[match_fib$bestmatch$compareID], sep=match_fib$bestmatch$sep))
      select_AESOP_hi=select_AESOP_hi[-match_fib$bestmatch$refID]
      if(length(select_AESOP_hi)==0){setorder(best_fib_hi, fibreID); break}
    }

    data_pos_lo=data.table(RA_data=RA_data[best_fib_lo$galaxyID], Dec_data=Dec_data[best_fib_lo$galaxyID])
    data_pos_hi=data.table(RA_data=RA_data[best_fib_hi$galaxyID], Dec_data=Dec_data[best_fib_hi$galaxyID])

    car_temp=sph2car(rbind(data_pos_lo, data_pos_hi))
    car_temp=rotate3d(car_temp, x=0, y=0, z=1, angle=-(pi/2-RA_AESOP*pi/180))
    car_temp=rotate3d(car_temp, x=1, y=0, z=0, angle=-(pi/2-Dec_AESOP*pi/180))
    car_temp_mm=car_temp[,1:2]/(pi/180/3600/AESOP_platescale)
    spines=data.table(
      x0_mm=AESOP_fibres[c(best_fib_lo$fibreID,best_fib_hi$fibreID),x0_mm],
      y0_mm=AESOP_fibres[c(best_fib_lo$fibreID,best_fib_hi$fibreID),y0_mm],
      x1_mm=car_temp_mm[,1],
      y1_mm=car_temp_mm[,2])
    spines[,sep_mm:=sqrt((x0_mm-x1_mm)^2+(y0_mm-y1_mm)^2)]

    collide=checkgood(spines[,list(x0_mm,y0_mm)],spines[,list(x1_mm,y1_mm)]) #check for bad 3D solution
    if(length(collide)==0){
      break
    }else{ #remove lower ranked colliding fibre from list and try again from scratch
      galIDs=c(best_fib_lo$galaxyID,best_fib_hi$galaxyID)
      pri_check=matrix(pri_data_orig[galIDs[collide]], ncol=2)
      remove=collide[pri_check[,1]<=pri_check[,2],1]
      check=check[-which(ref[check] %in% galIDs[remove])]
    }
  }


  invisible(list(
    AESOP_pos_lo=AESOP_pos$sph[best_fib_lo$fibreID,],
    AESOP_pos_hi=AESOP_pos$sph[best_fib_hi$fibreID,],
    data_pos_lo=data_pos_lo,
    data_pos_hi=data_pos_hi,
    best_fib_lo=best_fib_lo,
    best_fib_hi=best_fib_hi,
    spines=spines))
}
