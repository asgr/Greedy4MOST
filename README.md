Greedy 4MOST
================

Overview
--------

For most people the *Greedy4MOST* function is what they will want to use, since it will simulate observing multiple tiles in one go with little user input.

For more advanced users, you can interface directly with *Tile4MOST* (tells you the next most optimal tile position based on the chosen weighting scheme) and *Fibre4MOST* (assigns AESOP fibres to objects optimally given a set of weights).

WAVES North Example
-------------------

### Load data:

This is some full WAVES simulation data made by Luke Davies.

``` r
load('~/Downloads/WAVESMatched_New.Rdata')
```

### Prepare things for WAVES:

We manipulate it so that expected observing times cannot be longer than 10 hours (we won't be obseving longer than this). We also change the baseline 100 priorities so that objects that will take more exposure time get bumped up to higher priority. Given our exposure time clipping above this means the starting priorities span 100-106.

``` r
WAVES_wide_N=WAVESfull[RA>157.3 & RA<225 & SDSS_z_Apparent<20.5, list(CATAID, RA, DEC,
PRIORITY, TEXP_D, SDSS_z_Apparent)]
WAVES_wide_N=WAVES_wide_N[-which(is.na(TEXP_D)),]
WAVES_wide_N[TEXP_D>600,TEXP_D:=600]
WAVES_wide_N[,PRIORITY:=PRIORITY+as.integer(floor(TEXP_D/100))]
```

### Let's get Greedy:

We request 1000 tile and pass it the above WAVES North data. We also need to specify the allowed limits for placing AESOP field centres. You might want to restrict it to be well inside the survey region, here we give it freedom to place the centre all the way to the edge.

``` r
simWAVES=Greedy4MOST(tile=1:1000, RA_data=WAVES_wide_N$RA, Dec_data=WAVES_wide_N$DEC,
pri_data=WAVES_wide_N$PRIORITY, T_data=WAVES_wide_N$TEXP_D, RAlo=157.3, RAhi=225,
Declo=-4, Dechi=4)
```

Check completeness:

``` r
length(which(simWAVES$data$T_data<=0))/dim(simWAVES$data)[1]
```

Check where we put the tiles:

``` r
magplot(simWAVES$tileout[,list(RA_AESOP,Dec_AESOP)],asp=1)
rect(157.3, -4, 225, 4, border='red')
```

And we can check the final completeness:

``` r
temporig=denmap(WAVES_wide_N$RA, WAVES_wide_N$DEC, WAVES_wide_N$PRIORITY)
tempfinal=denmap(simWAVES$data$RA_data, simWAVES$data$Dec_data, simWAVES$data$pri_data)
magimage(x=tempfinal$x, y=tempfinal$y, z=tempfinal$z/temporig$z)
```

We can also make a tiling movie with these outputs easily:

``` r
for(i in 1:1000){
  select=which(simWAVES$data$success>0 & simWAVES$data$success<=i)
  tempcurrent=denmap(simWAVES$data$RA_data[select], simWAVES$data$Dec_data[select])

  png(paste0('comp',formatC(i,flag=0, width=3),'.png'),width=1200,height=300)
  par(mar=c(3.1,3.1,1.1,1.1))
  magimage(x=temporig$x, y=temporig$y, z=tempcurrent$z/temporig$z, magmap=FALSE,
  zlim=c(0,1), xlab='RA / deg', ylab='Dec / deg')
  legend('topright',legend=paste('Tile',i))
  dev.off()
}
```


