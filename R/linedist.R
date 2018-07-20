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

checkgood=function(xy_start, xy_end, z_start=0, z_end=250, threshold=0.1){
  xyz_start=as.matrix(cbind(xy_start, z_start))
  xyz_end=as.matrix(cbind(xy_end, z_end))

  endsep=sqrt(rowSums((xy_start-xy_end)^2))

  dists=nn2(xy_end, xy_start)
  mightcollide=dists$nn.dists<endsep
  dists$nn.idx[!mightcollide]=0
  check=which(tabulate(c(which(dists$nn.idx[,1]>0), dists$nn.idx[dists$nn.idx[,1]>0,1]))>=2)
  check=dists$nn.idx[dists$nn.idx[check,1],1]
  check=check[check>0]

  checkgrid=cbind(check,dists$nn.idx[check,1])

  linecheck=linedist(xyz_start[checkgrid[,1],], xyz_end[checkgrid[,1],], xyz_start[checkgrid[,2],], xyz_end[checkgrid[,2],])
  invisible(checkgrid[linecheck<=threshold,])
}

linedist=function(start1=cbind(0,0,0), end1=cbind(0,0,250), start2=cbind(0,0,0), end2=cbind(0,0,250)){

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
