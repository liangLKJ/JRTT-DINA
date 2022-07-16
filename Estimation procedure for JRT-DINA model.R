#-------------------Step1：read data----------------------------#
#Q matrix
Q <- data.matrix(read.csv("real data/Q matrix.csv"));
#response
PISA2015_math_response <- read.csv("real data/response.csv")
Y <- data.matrix(PISA2015_math_response)
#RTs
PISA2015_math_time <- read.csv("real data/RTs.csv")
for(f in 1:nrow(Y)){
  for(e in 1:ncol(Y)){
    PISA2015_math_time[f,e] <- PISA2015_math_time[f,e]/1000
  }
}
Timelog <- data.matrix(log(PISA2015_math_time))

#------------Step2: Set Monitored Parameters and Their Initial Values---------#
N<-nrow(Y)  #sample size
I<-nrow(Q)  #item size
K<-ncol(Q)  #attributes
jags.data<-list("N", "I", "K","Q","Y","Timelog")
jags.parameters <- c("s", "g", "alpha","beta","delta","theta","gamma","lambda",
                     "zeta","tau","den_epsilon","sigma_item","sigma_theta","prob","att_prob")
jags.inits <- NULL 

#--------------Step 3: Parameter Estimation--------------#
time1 <- as.POSIXlt(Sys.time())
sim <- jags(data = jags.data,inits = jags.inits, parameters.to.save =
              jags.parameters,model.file = "2  JRT-DINA.txt",n.chains = 2,n.iter = 20000,
            DIC = TRUE)
time2 <- as.POSIXlt(Sys.time())
use.time <- difftime(time2, time1, units="hours")

#--------------Step 4: Save Estimated Parameters---------------#
sim1 <- sim$BUGSoutput

write.csv(sim1$summary,"2  JRT-DINA_result/JRT_DINA_summary.csv")
write.csv(use.time, "2  JRT-DINA_result/JRT_DINA_time.csv")

#person parameters
person_parameter_summary <- cbind(sim1$mean$theta,sim1$mean$tau)
colnames(person_parameter_summary) <- c("theta","tau")                    
write.csv(person_parameter_summary,"2  JRT-DINA_result/person_parameters/JRT-DINA_person_parameter_summary.csv")
E.alpha <- sim1$median$alpha
write.csv(E.alpha,"2  JRT-DINA_result/person_parameters/JRT-DINA_Model_alpha.csv")

#item parameters
K_parameter_summary <- cbind(sim1$mean$gamma,sim1$mean$lambda)
colnames(K_parameter_summary) <- c("gamma","lambda")
write.csv(K_parameter_summary,"2  JRT-DINA_result/item_parameters/JRT-DINA_K_parameter_summary.csv")


item_parameter_summary <- cbind(sim1$mean$s, sim1$mean$g,
                                sim1$mean$beta,sim1$mean$delta,
                                sim1$mean$zeta,sqrt(1/sim1$mean$den_epsilon))
colnames(item_parameter_summary) <- c("s","g",
                                      "beta","delta",
                                      "zeta","den_epsilon")
write.csv(item_parameter_summary,"2  JRT-DINA_result/item_parameters/JRT-DINA_item_parameter_summary.csv")




