run_dbm<-function(period, run_time, dev_num, delta,scenario,run, ...){
  
  library(simmer)
  library(tidyverse)
  library(data.table)
  library(futile.logger)
  
  source("~/R/WirelessSim/01_helper_functions.R")
  
  default_period_list<<-period
  rcw <<- 1
  toa <<- 1
  time_after_rcw<<- 0
  run_time<<- run_time
  collision<<-0
  packets<<-0
  dev_num<<-dev_num
  delta<<-delta
  
  chsf<-c()
  for (i in 7:12){
    for (j in 1:8) {
      y<-paste0("channel_",j,"sf_", i)
      chsf<-c(chsf,y)
    }
  }
  
  
  ToA <- function(l_payload, sf, bw, h, crc, cr, de,...) {
    t_sym = (2^sf)/bw
    t_pream = (4.25 + 8)*t_sym
    n_pay = 8 + max(ceiling((8*l_payload - 4 * sf + 16*crc + 28 - 20*h)/(4*(sf - 2 *de)))* (cr+4), 0)
    t_pay = n_pay * t_sym
    t_packet = (t_pream + t_pay)/1000
    toa<<-t_packet
    return(t_packet)
    #return(10)
  }
  
  T_rcw <- function(sf, bw, time, ...){
    if(sf < 11){
      n_dsym <- 12
    }
    else{
      n_dsym <- 8
    }
    t_sym <- (2^sf)/bw
    t_rcw <- (n_dsym * t_sym +((2^sf)+32)/bw)/1000
    rcw <<- t_rcw
    time_after_rcw<<- time+rcw
    return(round(t_rcw,4))
    #return(5)
  }
  
  draw_channel <- function(devId){
    channel<-c(1:8)
    id = runif(1, 1, 8)
    df_device_timings$channel[df_device_timings$devId == devId] <<-channel[id]
    return(channel[id])
  }
  draw_sf <- function(devId){
    sf<-c(7:12)
    index = runif(1, 1, 6)
    payload<-10
    if(sf[index] == 7){
      payload<-runif(1, 10, 222)
    }
    if(sf[index] == 8){
      payload<-runif(1, 10, 222)
    }
    if(sf[index] == 9){
      payload<-runif(1, 10, 115)
    }
    if(sf[index] == 10){
      payload<-runif(1, 10, 51)
    }
    if(sf[index] == 11){
      payload<-runif(1, 10, 51)
    }
    if(sf[index] == 12){
      payload<-runif(1, 10, 51)
    }
    return(c(sf[index],payload))
  }
  
  check_channel_pre<-function(time, devID, status, ...){
    if(status >= 1){
      update_pre(devID, time)
      #flog.info(paste0("Heard Prede __ " ,devID))
      return(0)
    }
    return(0)
  }
  check_channel_post<-function(time, devID, status, ...){
    if(status >= 1){
      update_suc(devID, time)
      #flog.info(paste0("Heard Suc __ " ,devID))
      return(0)
    }
    return(0)
  }
  
  # Main device behavior
  
  df_device_timings <<- data.frame(devId=character(),
                                   time=numeric(),
                                   pre_time=numeric(),
                                   suc_time=numeric(),
                                   period=numeric(),
                                   channel=numeric(),
                                   sf=numeric(),
                                   payload=numeric(),
                                   stringsAsFactors=FALSE) 
  df_timings <<- data.frame(devId=character(),
                            time=numeric(),
                            pre_time=numeric(),
                            suc_time=numeric(),
                            period=numeric(),
                            channel=numeric(),
                            sf=numeric(),
                            payload=numeric(),
                            stringsAsFactors=FALSE) 
  
  df_packetloss <<- data.frame(devId=character(),
                               time=numeric(),
                               packet_status=numeric(),
                               toa=numeric(),
                               rcw=numeric(),
                               stringsAsFactors=FALSE) 
  update_packetloss_timeline<-function(devId,time,packet_status,toa,rcw,status, ...){
    if(status > 1){
      packet_status<-0
    }
    df_packetloss<<-df_packetloss %>% add_row(devId=devId,time=time,packet_status=packet_status,toa=toa,rcw=rcw)
    return(0)
  }
  
  create_df_timings <- function(devId,time,pre_time,suc_time,default_period, SF, payload, ...){
    if(nrow(df_device_timings) <= 0){
      df_device_timings<<-df_device_timings %>% add_row(devId=devId, time=time, pre_time=pre_time, suc_time=suc_time, period=default_period, sf=SF, payload=payload)
      df_device_timings$channel[df_device_timings$devId == devId]<<-0
      df_device_timings$sf[df_device_timings$devId == devId]<<-0
      return(0)
    }
    if(!devId %in% df_device_timings$devId){
      df_device_timings<<-df_device_timings %>% add_row(devId=devId, time=time, pre_time=pre_time, suc_time=suc_time, period=default_period, sf=SF, payload=payload)
      df_device_timings$channel[df_device_timings$devId == devId]<<-0
      df_device_timings$sf[df_device_timings$devId == devId]<<-0
      return(0)
    }
    else{
      df_device_timings$pre_time[df_device_timings$devId == devId] <<- pre_time
      df_device_timings$suc_time[df_device_timings$devId == devId] <<- suc_time 
      df_device_timings$time[df_device_timings$devId == devId] <<- time 
      df_device_timings$period[df_device_timings$devId == devId] <<- default_period
      df_device_timings$sf[df_device_timings$devId == devId] <<- SF
      df_device_timings$payload[df_device_timings$devId == devId] <<- payload
    }
    #df_timings <<- rbind(df_timings, df_device_timings)
    return(0)
  }
  
  update_pre<-function(devId, time, ...){
    df_device_timings$pre_time[df_device_timings$devId == devId] <<- time 
    return(0)
  }
  update_suc<-function(devId, time, ...){
    df_device_timings$suc_time[df_device_timings$devId == devId] <<- time 
    return(0)
  }
  update_send<-function(devId, time, ...){
    if(df_device_timings$time[df_device_timings$devId == devId] == -1){
      df_device_timings$time[df_device_timings$devId == devId] <<- time
      return(0)
    }
    return(0)
  }
  
  simul_packet<-function(now, end, ...){
    if((now-end)==0){
      return(1)
    }
    return(1)
  }
  
  listen_for_pre <- trajectory() %>% 
    set_attribute("Receiving_Predecessor", function() update_pre(get_name(env), simmer::now(env))) %>% 
    timeout(function() (as.numeric(get_attribute(env, "time_to_go"))- as.numeric(simmer::now(env))))
  listen_for_suc <- trajectory() %>%
    set_attribute("in_succ", function() as.numeric(simmer::now(env))) %>% 
    #timeout(function() print_log(paste0("Condition: ", ceiling(simmer::now(env)-get_attribute(env,"endTransmission"))), get_name(env))) %>% 
    branch(
      function() simul_packet(simmer::now(env),get_attribute(env,"endTransmission")), continue=TRUE,
      trajectory() %>%
        #timeout(function() print_log("Processing Successor Trap", get_name(env))) %>% 
        set_attribute("Receiving_Successor", function() as.numeric(simmer::now(env))) %>% 
        timeout(function() update_suc(get_name(env), simmer::now(env)))) %>% 
    #timeout(function() print_log("Simultaneous send and trap", get_name(env))) %>% 
    timeout(function() (as.numeric(get_attribute(env, "time_to_go"))- as.numeric(simmer::now(env))))
  
  update_collision<-function(time, devID, status, ...){
    if(status > 1){
      collision<<- collision +1
      return(0)
    }
    return(0)
  }
  update_packets<-function(){
    packets<<- packets +1
    return(0)
  }
  
  calculate_period <- function(devid, duration, ...){
    pre<- df_device_timings %>% filter(devId==devid) %>% dplyr::select(pre_time)
    suc<- df_device_timings %>% filter(devId==devid) %>% dplyr::select(suc_time)
    period<- df_device_timings %>% filter(devId==devid) %>% dplyr::select(period)
    transmission<- df_device_timings %>% filter(devId==devid) %>% dplyr::select(time)
    if(delta == 0){
      return(as.numeric(period))
    }
    if(pre==-1 && suc==-1){
      #prob <- runif(1, min = 0, max = 1)
      #if(prob > 0.9){
      
      #  period <- period + sample(c(-1,1), 1)*2*toa
      #}
      #flog.info(paste0("Heard None __ " ,devid))
      return(as.numeric(period))
    }
    if(pre==-1 && suc!=-1){
      period <- period - delta*duration
      df_device_timings$period[df_device_timings$devId == devid] <<-as.numeric(period)
      #flog.info(paste0("Heard Successor __ " ,devid))
      return(as.numeric(period))
    }
    if(pre!=-1 && suc!=-1){
      t_s<- (pre-suc)/2
      t_i<-transmission
      gamma<-((suc-t_s) - t_i)
      period <- period + gamma
      df_device_timings$period[df_device_timings$devId == devid] <<- as.numeric(period)
      return(as.numeric(period))
    }
    return(as.numeric(period))
  }
  
  dbm_device <- trajectory() %>% 
    set_attribute("period" , function() runif(1,default_period_list[1], default_period_list[2])) %>% 
    set_attribute(c("SF", "Payload"), function() draw_sf(get_name(env))) %>%
    timeout(function() rnorm(1, mean=get_attribute(env,"period"), sd=2.5)) %>% 
    set_attribute("Channel", function() draw_channel(get_name(env))) %>%
    # Rollback to here
    set_attribute("period" , function() runif(1,default_period_list[1], default_period_list[2])) %>% 
    timeout(function() create_df_timings( get_name(env),-1, -1, -1, get_attribute(env, "period"), get_attribute(env, "SF"), get_attribute(env, "Payload"))) %>% 
    simmer::select(function(){paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF"))}) %>%
    timeout(function() check_channel_pre(as.numeric(simmer::now(env)),get_name(env), get_server_count(env,paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")) ) )) %>% 
    set_attribute("Trapping_signal_Before_Transmission", function() as.numeric(simmer::now(env))) %>% 
    trap(function() paste0("blocking_channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")), handler = listen_for_pre) %>% # listen to channel and sf pair
    set_attribute("Trcw1", function() T_rcw(as.numeric(get_attribute(env,"SF")),125,simmer::now(env))) %>%
    set_attribute("time_to_go", function() (as.numeric(get_attribute(env, "Trcw1"))+as.numeric(simmer::now(env)))) %>%
    timeout(function() as.numeric(get_attribute(env, "Trcw1"))) %>% 
    set_attribute("Un-Trapping_signal_Before_Transmission", function() as.numeric(simmer::now(env))) %>% 
    untrap(function() paste0("blocking_channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF"))) %>% 
    timeout(function() check_channel_pre(as.numeric(simmer::now(env)),get_name(env), get_server_count(env,paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")) ) )) %>% 
    seize_selected(continue=c(TRUE), 
                   post.seize = trajectory() %>%
                     timeout(function()update_packets()) %>%
                     timeout(function()update_collision(as.numeric(simmer::now(env)),get_name(env), get_server_count(env,paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")) ))) %>%
                     timeout(function()update_packetloss_timeline(get_name(env),simmer::now(env),1,ToA(as.numeric(get_attribute(env,"Payload")),as.numeric(get_attribute(env,"SF")),125,0,1,1,1),T_rcw(as.numeric(get_attribute(env,"SF")),125,simmer::now(env)),get_server_count(env,paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")) ))) %>% 
                     set_attribute("Send_signal_Begin_ToA", function() as.numeric(simmer::now(env))) %>% 
                     send(function() paste0("blocking_channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF"))) %>% 
                     set_attribute("ToA", function() ToA(as.numeric(get_attribute(env,"Payload")),as.numeric(get_attribute(env,"SF")),125,0,1,1,1)) %>% 
                     timeout(function() ToA(as.numeric(get_attribute(env,"Payload")),as.numeric(get_attribute(env,"SF")),125,0,1,1,1)) %>% 
                     timeout(function()update_send(get_name(env), simmer::now(env))) %>% 
                     set_attribute("Send_signal_End_ToA", function() as.numeric(simmer::now(env))) %>% 
                     send(function() paste0("blocking_channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF"))) %>% 
                     release_selected()) %>% 
    set_attribute("endTransmission", function() as.numeric(simmer::now(env))) %>% 
    timeout(function() check_channel_post(as.numeric(simmer::now(env)),get_name(env), get_server_count(env,paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")) ) )) %>%  
    set_attribute("Trapping_signal_After_Transmission", function() as.numeric(simmer::now(env))) %>% 
    trap(function() paste0("blocking_channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")), handler = listen_for_suc) %>% # listen to channel and sf pair
    set_attribute("Trcw2", function() T_rcw(as.numeric(get_attribute(env,"SF")),125,simmer::now(env))) %>%
    set_attribute("time_to_go", function() (as.numeric(get_attribute(env, "Trcw2"))+as.numeric(simmer::now(env)))) %>%
    timeout(function() as.numeric(get_attribute(env, "Trcw2"))) %>% 
    set_attribute("Un-Trapping_signal_After_Transmission", function() as.numeric(simmer::now(env))) %>% 
    untrap(function() paste0("blocking_channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF"))) %>% 
    timeout(function() check_channel_post(as.numeric(simmer::now(env)),get_name(env), get_server_count(env,paste0("channel_",get_attribute(env,"Channel"),"sf_",get_attribute(env,"SF")) ) )) %>% 
    set_attribute("period", function() calculate_period(get_name(env), ToA(as.numeric(get_attribute(env,"Payload")),as.numeric(get_attribute(env,"SF")),125,0,1,1,1))) %>% 
    timeout(function() as.numeric(get_attribute(env, "period"))-(ToA(as.numeric(get_attribute(env,"Payload")),as.numeric(get_attribute(env,"SF")),125,0,1,1,1) + 2*as.numeric(get_attribute(env, "Trcw2"))) ) %>%
    rollback(25)
  
  env <- simmer()
  for (i in chsf)
    env %>% add_resource(i, Inf,0)
  env %>%
    add_generator("device", dbm_device, distribution = at(rep(0,dev_num)), mon=2) # alle gleichzeitig
  df_results <- data.frame(packetloss=numeric(),
                           packets=numeric(),
                           collision=numeric(),
                           stringsAsFactors=FALSE) 
  
  env %>% 
    reset() %>% 
    run(run_time, progress = flog.info)
  arrs<-get_mon_arrivals(env, ongoing = TRUE) %>% filter(finished==FALSE)
  atts<-get_mon_attributes(env) %>% filter(key=="period")
  ress <- get_mon_resources(env)
  packetloss<-collision/packets
  #print(packetloss)
  df_results <- df_results %>% add_row(packetloss=packetloss, packets=packets, collision=collision)
  
  df_results$run_time=run_time
  df_results$dev_num=dev_num
  df_results$delta=delta
  df_results$scenario=scenario
  df_results$period=paste0(default_period_list[1],"-",default_period_list[2])
  df_results$run=run
  
  
  
  df_device_timings$run_time=run_time
  df_device_timings$dev_num=dev_num
  df_device_timings$delta=delta
  df_device_timings$scenario=scenario
  df_device_timings$period_str=paste0(default_period_list[1],"-",default_period_list[2])
  df_device_timings$run=run
  
  
  df_timeline <- get_mon_attributes(env) %>% mutate(id = row_number()) 
  df_timeline$run_time=run_time
  df_timeline$dev_num=dev_num
  df_timeline$delta=delta
  df_timeline$scenario=scenario
  df_timeline$period_str=paste0(default_period_list[1],"-",default_period_list[2])
  df_timeline$run=run
  
  df_packetloss$run_time=run_time
  df_packetloss$dev_num=dev_num
  df_packetloss$delta=delta
  df_packetloss$scenario=scenario
  df_packetloss$period=paste0(default_period_list[1],"-",default_period_list[2])
  df_packetloss$run=run
  
  
  return(list(result=df_results, timeline=df_timeline, devices=df_device_timings, packetloss=df_packetloss))
}
