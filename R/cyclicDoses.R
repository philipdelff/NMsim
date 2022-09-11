##' create intermittent dosing regimen

##' @param hours.of.day dosing time of day (hours)
##' @param cycles.days list of numeric vectors. The vectors represent
##'     nested regimens. See details.
##' @param days.total Total number of days for the regimen to
##'     continue.
##' @param dose Will be copied to amt column
##'     
##' @details \itemize{ \item list(c(4,3)) means three days on, four
##'     days off.  \item list(c(4,3),c(3,1)) means three days on, four
##'     days off for three cycles, then one cycle (of 4+3 days) off.
##'     }
##' @examples
##' Complicated like ph2
##' dt <- cyclicDoseTab(c(0,12),cycles.days=list(c(4,3),c(21,7)),days.total=56,dose=400)
##' ggplot(dt,aes(times/24,amt))+geom_point()+geom_vline(xintercept=c(0,28,56))


cyclicDoses <- function(hours.of.day,cycles.days,days.total,dose){

    
    ## repeat each dosing cycle in a new dosing cycle.
    ## repeat dosing times and waiting times until reaching length of simulation

    ## cycles.days=list(c(4,3),c(21,7))
    
    if(missing(cycles.days)) cycles.days <- list(c(0,1))

    ### treating the le
    cycles.days <- c(cycles.days,list(c(days.total,0)))
    
    d <- c(outer(c(hours.of.day),
    (seq(1,cycles.days[[1]][1])-1)*24,'+')
    )
    I <- 2
    while(I <= length(cycles.days)){
        d <- c(outer(d,
        (seq(1,by=sum(cycles.days[[I-1]]),length.out=ceiling(cycles.days[[I]][1]/sum(cycles.days[[I-1]])))-1)*24,'+'
        ))
        nextdose <- sum(cycles.days[[I]])*24
        I <- I+1
    }
    d <- d[d<=(days.total*24)]
    doses <- data.table(TIME=d
                      ,AMT = dose 
                        )
    
    doses[]
    
}
