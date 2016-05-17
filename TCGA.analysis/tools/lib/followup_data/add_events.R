followup.add_events <- function(dataframe){
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

