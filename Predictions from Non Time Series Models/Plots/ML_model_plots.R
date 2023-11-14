

library(reshape2)
library(ggplot2)

ML_plots = function(data, data1){
  ind = which(data$Date == '2021-01-20')
  
  p = ggplot()+
    geom_vline(xintercept = ind, linetype = 'dashed', col = 'navyblue', linewidth = 0.6)+
    geom_line(data = data, aes(x = 1 : nrow(data), y = Open, col = 'Original'))+
    geom_line(data = data1, aes(x = (ind+1) : nrow(data), y = LR_test, col = 'LR'))+
    geom_line(data = data1, aes(x = (ind+1) : nrow(data), y = RFR_test, col = 'RFR'))+
    labs(x = 'Date',
         y = 'Opening Price',
         color = 'Index')+
    scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
    theme(axis.text.x = element_text( size = 13),
          axis.text.y = element_text( size = 13),
          axis.title = element_text(size = 13),
          legend.text = element_text( size = 11),
          legend.title = element_text( size = 13, hjust = 0.5),
          legend.position = 'bottom')
  
  return(p)
}

ML_plots(read.csv('BPCL.NS.csv'), read.csv('BPCL.NS_pred_LR_RFR.csv'))
ML_plots(read.csv('CIPLA.NS.csv'), read.csv('CIPLA.NS_pred_LR_RFR.csv'))
ML_plots(read.csv('COROMANDEL.NS.csv'), read.csv('COROMANDEL.NS_pred_LR_RFR.csv'))
ML_plots(read.csv('HDFCBANK.NS.csv'), read.csv('HDFCBANK.NS_pred_LR_RFR.csv'))
ML_plots(read.csv('TCS.NS.csv'), read.csv('TCS.NS_pred_LR_RFR.csv'))


