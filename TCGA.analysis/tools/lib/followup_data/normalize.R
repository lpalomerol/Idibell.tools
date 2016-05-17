followup.normalizer <- function(follow){
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
