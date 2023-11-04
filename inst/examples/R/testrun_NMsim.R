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

#### 1. using PSN's execute
simres.1 <- NMsim(file.mod,
                data=dt.sim,
                dir.sims="~/simulations",
                method.execute="PSN")

simres.1
stopifnot(sum((simres.1[,PRED]-c(0,.96356))^2)<.001)

#### If 1 did not work:
## is PSN's `execute` in your system path? Open a terminal/dos prompt
## and type `execute`. If `execute` was found, this may be a NMsim
## bug. If not, how do you normally run `execute`? Please provide the
## directory in which `execute` and `update_inits` are found like
## this:

### I have an installation like this. Notice you have to provide a
### directory that contains files called "execute" and "update_inits",
### as far as I can see without file name extension.
## dir.psn="c:/Users/me/software/PsN-5.3.1/strawberry/perl/bin"

simres.1 <- NMsim(file.mod,
                data=dt.sim,
                dir.sims="~/simulations",
                method.execute="PSN",
                ## method.update.inits="nmsim"
                dir.psn="/dir/with/psn"
                )

simres.1
stopifnot(sum((simres.1[,PRED]-c(0,.96356))^2)<.001)


## you can use NMdata::NMdataConf to set the default value for dir.psn. 

#### 2. Using NMsim's internal Nonmem execution method. 
## This is not yet expected to work on windows. It does unlock
## important features so if possible, this method is often
## recommended.

## you must know the full path to the Nonmem executable.  Maybe something like:
path.nonmem <- "/opt/NONMEM/nm75/run/nmfe75" 

simres.2 <- NMsim(file.mod,
                data=dt.sim,
                dir.sims="~/simulations",
                path.nonmem=path.nonmem)

simres.2
stopifnot(sum((simres.2[,PRED]-c(0,.96356))^2)<.001)
