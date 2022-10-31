library(simmer)
library(tidyverse)
library(data.table)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./sim/01_dbm_lora.R")
source("./sim/02_dbm_lora_one_channel.R")
dir.create(paste0("./results"), recursive = T)


# Single Channel Simulation ----
period<-c(900,900)
run_time<- 1800
run=1
delta<-2
scenario<-"dbm_one_channel"
devices <- 500
print(scenario)
sim<- run_dbm_one_channel(period, run_time, devices, delta,scenario,run)

write.csv(sim$packetloss, file=paste0("./results/packetloss_",scenario,"_period_",period[1],"-",period[2],"_delta_",delta,"_devices_",devices,"_run-time_",run_time,"_run_",run, ".csv"))
write.csv(sim$devices, file=paste0("./results/devices_",scenario,"_period_",period[1],"-",period[2],"_delta_",delta,"_devices_",devices,"_run-time_",run_time,"_run_",run, ".csv"))
write.csv(sim$timeline, file=paste0("./results/timeline_",scenario,"_period_",period[1],"-",period[2],"_delta_",delta,"_devices_",devices,"_run-time_",run_time,"_run_",run, ".csv"))

# Multi Channel Simulation ----
period<-c(900,900)
run_time<- 1800
run=1
delta<-2
scenario<-"dbm_multi_channel"
devices <- 500
print(scenario)
sim<- run_dbm(period, run_time, devices, delta,scenario,run)

write.csv(sim$packetloss, file=paste0("./results/packetloss_",scenario,"_period_",period[1],"-",period[2],"_delta_",delta,"_devices_",devices,"_run-time_",run_time,"_run_",run, ".csv"))
write.csv(sim$devices, file=paste0("./results/devices_",scenario,"_period_",period[1],"-",period[2],"_delta_",delta,"_devices_",devices,"_run-time_",run_time,"_run_",run, ".csv"))
write.csv(sim$timeline, file=paste0("./results/timeline_",scenario,"_period_",period[1],"-",period[2],"_delta_",delta,"_devices_",devices,"_run-time_",run_time,"_run_",run, ".csv"))





