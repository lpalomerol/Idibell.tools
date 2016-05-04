#CONSTANTS

v1_5 = '1.5'
v2_1 = '2.1'
v4_0 = '4.0'

load_follow_up_data <- function(file, version='', new_time_event_file = ''){
  if(version == ''){
    stop('Undefined version')
  } else if (version == v1_5 || version == v2_1){
    followup_file = build_followup_file(file)
  } else if (version == v4_0){
    followup_file = build_followup_file_v4_0(file, new_time_event_file)
  } else{
    stop('Invalid version')
  }
  normalize_followup_dataframe(followup_file)
}

build_followup_file <- function(filename){
  raw = readLines(filename)
  #Remove unwanted headers
  raw = raw[-2:-3]
  read.csv(textConnection(raw), header = TRUE, stringsAsFactors = FALSE, sep='\t')
}

build_followup_file_v4_0 <- function(followup_file, nte_file){
  merge(
    build_followup_file(followup_file),
    build_followup_file(nte_file),
    col='bcr_patient_barcode',
    all= TRUE
  )
}


normalize_followup_dataframe <- function(follow){
  keeps = c('bcr_patient_barcode', 
            'new_tumor_event_dx_days_to', 
            'last_contact_days_to', 'death_days_to')
  follow = follow[,(names(follow) %in% keeps)]
  
  colnames(follow)<-c("barcode","last","death","recur")
  follow[follow=="[Not Available]"]<-NA
  follow[follow=="[Not Applicable]"]<-NA
  follow[follow=="[Completed]"]<-NA
  follow$death<-as.numeric(follow$death)
  follow$last<-as.numeric(follow$last)
  follow$recur<-as.numeric(follow$recur)
  
  follow$bcr_followup_barcode = substring(follow$barcode, 0,12)
  follow
}

intersect_follow_ups <- function(master, slave){
  rbind(master, 
        slave[setdiff(rownames(slave), rownames(master)),]) 
}

add_event_data <- function(dataframe){
  dataframe = transform(dataframe,
                        time.death=round(death/30.5),
                        event.death=1-is.na(death),
                        time.recur=round(recur/30.5),
                        event.recur=1-is.na(recur)
  )
  dataframe[is.na(dataframe$death),"time.death"]<-round(dataframe[is.na(dataframe$death),"last"]/30.5)
  dataframe[is.na(dataframe$recur),"time.recur"]<-round(dataframe[is.na(dataframe$recur),"last"]/30.5)
  dataframe[is.na(dataframe$time.recur),"time.recur"]<-dataframe[is.na(dataframe$time.recur),"time.death"]
  
  dataframe  
}

summary.followup <- function(followup){
  list(
    nodeath = nrow(followup[is.na( followup$death) == TRUE,]),
    death = nrow(followup[!is.na( followup$death) == TRUE,]),
    norecur = nrow(followup[is.na( followup$recur) == TRUE,]),
    recur = nrow(followup[!is.na( followup$recur) == TRUE,])
  )
}


draw_survplot <- function(kmfit, label){
  plot(kmfit, mark=3, col=c('black', 'grey', 'red', 'blue'), 
       xlab="Survival time in days",
       ylab="Survival probabilities",
       lty=c('solid'),
       main=label)
  
  legend('bottomleft', c('group1(low)', 'group2', 'group3', 'group4(high)'), 
         col=c('black', 'grey', 'red', 'blue'),
         lty=c('solid'))
  
}

