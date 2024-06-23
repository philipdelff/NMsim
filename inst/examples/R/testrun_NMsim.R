### The purpose of this script is to provide concise code to check
### whether NMsim successfully runs using various methods on a given
### system.

res.req <- require(NMsim)
if(!res.req) stop("NMsim could not be loaded. Please make sure it's installed.")

file.mod <- system.file("examples/nonmem/xgxr021.mod",package="NMsim")
if(!file.exists(file.mod)) stop("the test control stream wasn't found")

dt.dos <- NMcreateDoses(TIME=0,AMT=100)
dt.sim <- addEVID2(dt.dos,time.sim=1,CMT=2)
dt.sim$ROW <- 1:nrow(dt.sim)


#### 1. Using NMsim's internal Nonmem execution method. 

## you must know the full path to the Nonmem executable.  Maybe something like:
path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 
## In windows the executable is a .bat file.

simres.1 <- NMsim(file.mod,
                data=dt.sim,
                dir.sims="~/simulations",
                path.nonmem=path.nonmem)

simres.1
stopifnot(sum((simres.2$PRED-c(0,.96356))^2)<.001)

### If you got 1 to work, you are all set. Just remember to always let NMsim know where to find your nonmem executable. You can do this with - adjust to your config.
NMdataConf(path.nonmem="/opt/NONMEM/nm75/run/nmfe75")

#### 2. using PSN's execute. 
simres.2 <- NMsim(file.mod,
                data=dt.sim,
                dir.sims="~/simulations",
                method.execute="PSN")

simres.2
stopifnot(sum((simres.2$PRED-c(0,.96356))^2)<.001)

#### If 2 did not work:
## is PSN's `execute` in your system path? Open a terminal/dos prompt
## and type `execute`. If `execute` was found, this may be a NMsim
## bug. If not, how do you normally run `execute`? Please see vignette
## on configuration of NMsim.

### I have an installation like this. Notice you have to provide a
### directory that contains files called "execute" and "update_inits",
### as far as I can see without file name extension.
## dir.psn="c:/Users/me/software/PsN-5.3.1/strawberry/perl/bin"

##### 2.b Only if you couldn't get 2. to run
if(FALSE){
simres.2 <- NMsim(file.mod,
                data=dt.sim,
                dir.sims="~/simulations",
                method.execute="PSN",
                ## method.update.inits="nmsim"
                dir.psn="/dir/with/psn"
                )

simres.2
stopifnot(sum((simres.2$PRED-c(0,.96356))^2)<.001)
}

## you can use NMdata::NMdataConf to set the default value for dir.psn. 

