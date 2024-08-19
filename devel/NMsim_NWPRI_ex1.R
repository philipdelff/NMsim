file.mod <- "example_nonmem_models/lorlatinib_sim_est/mod_lorlatinib_estimate.mod"


## need a relevant simulation data set


simres <- NMsim(file.mod,
                data=data.sim,
                method.sim=NMsim_NWPRI
                dir.sims
                )
                
