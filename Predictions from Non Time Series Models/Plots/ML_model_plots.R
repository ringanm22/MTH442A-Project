

library(reshape2)
library(ggplot2)

ML_plots = function(data, data1){
  ind = which(data$Date == '2021-01-20')
  
  p = ggplot()+
    geom_vline(xintercept = ind, linetype = 'dashed', col = 'navyblue', linewidth = 0.6)+
    geom_line(data = data, aes(x = 1 : nrow(data), y = Close, col = 'Original'))+
    geom_line(data = data1, aes(x = (ind+1) : nrow(data), y = LR_test, col = 'LR'))+
    geom_line(data = data1, aes(x = (ind+1) : nrow(data), y = RFR_test, col = 'RFR'))+
    geom_line(data = data1, aes(x = (ind+1) : nrow(data), y = XGB_test, col = 'XGB'))+
    labs(x = 'Date',
         y = 'Closing Price',
         color = 'Index')+
    scale_x_continuous(breaks = seq(1,nrow(data),800), labels = data$Date[seq(1,nrow(data),800)])+
    theme(axis.text.x = element_text( size = 14),
          axis.text.y = element_text( size = 14),
          axis.title = element_text(size = 15),
          legend.text = element_text( size = 12),
          legend.title = element_text( size = 15, hjust = 0.5))
  
  return(p)
}

ML_plots(read.csv('BPCL.NS.csv'), read.csv('BPCL.NS_pred_LR_RFR_XGB.csv'))
ML_plots(read.csv('CIPLA.NS.csv'), read.csv('CIPLA.NS_pred_LR_RFR_XGB.csv'))
ML_plots(read.csv('COROMANDEL.NS.csv'), read.csv('COROMANDEL.NS_pred_LR_RFR_XGB.csv'))
ML_plots(read.csv('HDFCBANK.NS.csv'), read.csv('HDFCBANK.NS_pred_LR_RFR_XGB.csv'))
ML_plots(read.csv('TCS.NS.csv'), read.csv('TCS.NS_pred_LR_RFR_XGB.csv'))


