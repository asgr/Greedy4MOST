# For the Python people:
# # -----------
# import numpy as np
# x1=np.array([3.,-2.,5.]) # start of the first line (x,y,z)
# x2=np.array([7.,-2.,3.]) # end of the first line
# x3=np.array([3.,2.,-1.]) # start of the second line (x,y,z)
# x4=np.array([4.,4.,6.]) # end of the second line
# # -----------
# a = x2 - x1
# b = x4 - x3
# c = x3 - x1
# cross = np.cross(a,b) # cross product
# dist = np.linalg.norm(np.dot(c,cross))/np.linalg.norm(cross)
# print(dist) # should be 5.36

checkgood=function(xy_start, xy_end, z_start=0, z_end=250, threshold=0.1+0.085){
  if(dim(xy_start)[2]!=2){stop('xy_start must be Nx2 dimensions')}
  if(dim(xy_end)[2]!=2){stop('xy_end must be Nx2 dimensions')}
  assertNumeric(z_start, len=1)
  assertNumeric(z_end, len=1)
  assertNumeric(threshold, len=1)

  xyz_start=as.matrix(cbind(xy_start, z_start))
  xyz_end=as.matrix(cbind(xy_end, z_end))

  endsep=sqrt(rowSums((xy_start-xy_end)^2)) #xy start to end distance for each fibre

  dists=nn2(xy_end, xy_start) #Calculate distances between all fibre start and end points

  mightcollide=dists$nn.dists<endsep #which fibres are closer to a start point than the fibre's own end point (so crossing)
  Nrep=rowSums(mightcollide) #Find our how many colliding fibres are near to each fibre
  dists$nn.idx[!mightcollide]=0 #Set everything else to 0 (so only things above 0 might be colliding)

  #The next line is complicated looking, but it just flattens the matches so each match pair appears once.
  checkgrid=cbind(rep(1:length(dists$nn.idx[,1]), times=Nrep), unlist(t(dists$nn.idx)[t(dists$nn.idx)>0]))

  #Use the linedist function only on pairs which are known to cross within the geometry of AESOP
  linecheck=linedist(xyz_start[checkgrid[,1],,drop=FALSE], xyz_end[checkgrid[,1],,drop=FALSE], xyz_start[checkgrid[,2],,drop=FALSE], xyz_end[checkgrid[,2],,drop=FALSE])
  invisible(checkgrid[linecheck<=threshold,,drop=FALSE]) #Return just those within the matching threshold
}

linedist=function(start1=cbind(0,0,0), end1=cbind(0,0,250), start2=cbind(0,0,0), end2=cbind(0,0,250)){

  if(dim(start1)[2]!=3){stop('start1 must be Nx3 dimensions')}
  if(dim(end1)[2]!=3){stop('end1 must be Nx3 dimensions')}
  if(dim(start2)[2]!=3){stop('start2 must be Nx3 dimensions')}
  if(dim(end2)[2]!=3){stop('end2 must be Nx3 dimensions')}

  avec=end1-start1
  bvec=end2-start2
  cvec=start2-start1
  cross_temp=cbind(avec[,2]*bvec[,3]-avec[,3]*bvec[,2],
                   avec[,3]*bvec[,1]-avec[,1]*bvec[,3],
                   avec[,1]*bvec[,2]-avec[,2]*bvec[,1])
  dot_temp=abs(cvec[,1]*cross_temp[,1]+cvec[,2]*cross_temp[,2]+cvec[,3]*cross_temp[,3])
  cross_temp=sqrt(cross_temp[,1]^2+cross_temp[,2]^2+cross_temp[,3]^2)
  invisible(as.numeric(unlist(dot_temp/cross_temp)))
}
