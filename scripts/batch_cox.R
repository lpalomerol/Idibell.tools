args=commandArgs(trailingOnly=TRUE)

library(survival)

get_name <- function(filename){
  strsplit(filename, '\\.')[[1]][3]
}

get_patient_barcode <- function(sample){
  substr(sample, 0, 12)
}


generate_cox = function(model, variable){
  coxmodel = coxph(model ~ variable)
  print(summary(coxmodel))
  return(
    list(
      coeff = coxmodel$coefficients[1],
      hr = summary(coxmodel)$coefficients[2],
      low.95=(summary(coxmodel)$conf.int[,3]),
      high.95=(summary(coxmodel)$conf.int[,4]),
      p.wald=coef(summary(coxmodel))[,5],
      p.log=(summary(coxmodel))$logtest[['pvalue']]
    )
  )  
}

write_line <- function(file,model_name, name, cox, events, plot){
  line_to_write = paste(name, model_name,
                        paste(events$obs[2], 'of', events$n[2]),
                        paste(events$obs[1], 'of', events$n[1]),
                        cox$coeff, cox$hr,
                        cox$low.95, cox$high.95,
                        cox$p.wald, cox$p.log, 
                        plot,
                        sep=';')  
  write(line_to_write, file, append=TRUE)
}

generate_km_curve_plots <- function(model, variable, name){
  
  km = survfit(model ~ variable)
  
  plot_name =paste('km_', name, '.ps', sep='') 
  
  postscript(paste(OUTPUT_FOLDER, plot_name, sep='/'))
  plot(km, 
       col=c('green', 'red'),
       mark=3,
       xlab="Non relapse time",
       ylab="Non relapse probabilities",
       main=paste("Surv for  ", name,  sep='')
  )
  legend('bottomleft',
         c('Selected subset', 'Other'),
         col=c('red','green','blue'),
         lty=c("solid")
  )
  dev.off()
  return(plot_name)
}

get_model_data <- function(model, model_name, events_ds, filename, output_file){
  
  cox_values = generate_cox(model, as.matrix(events_ds[filename]))
  events = survdiff(model ~ as.matrix(events_ds[filename]))
  
  if(min(cox_values$p.wald, cox_values$p.log) < SIGNIFICANCE_LEVEL){
    plot=generate_km_curve_plots(model, as.matrix(events_ds[filename]), filename)
  } else {
    plot= ''
  }
  
  write_line(output_file,
             model_name, 
             filename,
             cox_values,
             events,
             plot)  
}


SIGNIFICANCE_LEVEL=0.05

RELAPSE_FILE_ID=1
FILES_TO_CHECK_ID=2
OUTPUT_FOLDER_ID=3

RELAPSE_FILE=args[RELAPSE_FILE_ID]
FILES_TO_CHECK=args[FILES_TO_CHECK_ID]
OUTPUT_FOLDER=args[OUTPUT_FOLDER_ID]
OUTPUT_FILE = paste(OUTPUT_FOLDER, '/results.csv', sep='')

events_ds = read.csv(RELAPSE_FILE, sep=';')
files_to_check_list = strsplit(FILES_TO_CHECK, ';')[[1]]

events_ds$barcode_patient = get_patient_barcode(events_ds$barcode)

model.relapse = Surv(events_ds$time.recur, events_ds$event.recur)
model.death = Surv(events_ds$time.death, events_ds$event.death)

write("NAME;EVENT;EVTS_SEL;EVTS_OTHER;COEF;HR;LOW.95;HIGH.95;P.WALD;P.LOG;PLOT.TERTILES", OUTPUT_FILE)

for(i in 1:length(files_to_check_list)){

  filename=get_name(files_to_check_list[i])
  file = read.csv(files_to_check_list[i], as.is=TRUE, head=FALSE)
  colnames(file) <- c('barcode_patient')
  file$barcode_patient=get_patient_barcode(file$barcode)
  events_ds[filename] = ifelse(events_ds$barcode_patient %in% file$barcode_patient, 1, 0)
  get_model_data(model.relapse, 'relapse', events_ds, filename, OUTPUT_FILE)
  get_model_data(model.death, 'death', events_ds, filename, OUTPUT_FILE)
}

