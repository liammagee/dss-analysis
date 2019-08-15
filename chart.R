library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(reshape2)

df <- data.frame('Date'=lubridate::ymd(), 'Partial.NS'=integer(), 'Total.NS'=integer(), 'Total.DSP'=integer())


addPeriod <- function(df, file, year, month, sheetTotal = 4, sheetPartial = 19) {
  period.ALL <- read_excel(file, sheetTotal)
  period.NS <- read_excel(file, sheetPartial)
  
  dsp.total <- as.numeric(period.ALL$X__9[12])
  ns.total <- as.numeric(period.ALL$X__15[12])
  ns.partial <- as.numeric(period.NS$X__2[3])
  df %>% add_row('Partial.NS'=ns.partial, 'Total.NS'=ns.total, 'Total.DSP'=dsp.total, 'Date' = ymd(paste(year, '/', month, '/', 1)) )
}

    
    
df <- addPeriod(df, 'dss-payment-demographics-dataset-december-2018.xlsx', 2018, 12)
df <- addPeriod(df, 'dssdemographics-september-2018-conf.xlsx', 2018, 9, sheetPartial = 18)
df <- addPeriod(df, 'dss-demographics-june-2018.xlsx', 2018, 6)
df <- addPeriod(df, 'dss-demographics-march-2018.xlsx', 2018, 3)

df <- addPeriod(df, 'dss-demographics-december-2017.xlsx', 2017, 12)
df <- addPeriod(df, 'dss-demographics-september-2017.xlsx', 2017, 9)
df <- addPeriod(df, 'dss-demographics-june-2017.xlsx', 2017, 6)
df <- addPeriod(df, 'dss-demographics-march-2017.xlsx', 2017, 3)

df <- addPeriod(df, 'dss-demographics-december-2016.xlsx', 2016, 12)
df <- addPeriod(df, 'dss-demographics-september-2016.xlsx', 2016, 9, sheetPartial = 18)
df <- addPeriod(df, 'dss-demographics-june-2016.xlsx', 2016, 6, sheetPartial = 18)
df <- addPeriod(df, 'dss-demographics-march-2016.xlsx', 2016, 3, sheetPartial = 18)

df <- addPeriod(df, 'demographics-december-2015-conf.xlsx', 2015, 12, sheetPartial = 18)
df <- addPeriod(df, '201509-dssdemographics.xlsx', 2015, 9, sheetPartial = 18)
df <- addPeriod(df, 'dss-demographics-june-2015.xlsx', 2015, 6, sheetPartial = 18)
df <- addPeriod(df, 'dss-demogrphics-march-2015.xlsx', 2015, 3, sheetPartial = 18)

df <- addPeriod(df, 'dssdemographicdecember2014.xlsx', 2014, 12, sheetTotal = 2, sheetPartial = 14)
df <- addPeriod(df, 'dssdemographicsseptember2014.xlsx', 2014, 9, sheetTotal = 2, sheetPartial = 14)
df <- addPeriod(df, 'june-2014-dss-demographics-suppressed-2014-final-ftb.xlsx', 2014, 6, sheetTotal = 2, sheetPartial = 14)
#df <- addPeriod(df, 'dssdemographicsmarch2014.xlsx', 'Mar14', 2014, 3, sheetTotal = 2, sheetPartial = 14)

#df <- addPeriod(df, 'dssdemographicsseptember2013.xlsx', 'Dec13', 2013, 12, sheetTotal = 2, sheetPartial = 14)
#df <- addPeriod(df, 'dssdemographicsseptember2013.xlsx', 'Sep13', 2013, 9, sheetTotal = 2, sheetPartial = 14)

df$Diff.DSP <- df$Total.DSP - df[length(df$Total.DSP),'Total.DSP']
df$Diff.Partial.NS <- df$Partial.NS - df[length(df$Partial.NS),'Partial.NS']

df$Diff.of.Diffs <- df$Diff.Partial.NS - df$Diff.DSP
 
dfm <- melt(df, id.vars = c("Date"), measure.vars = c('Total.DSP', 'Total.NS', 'Partial.NS', 'Diff.of.Diffs'))
dfm$strdate <- strftime(dfm$Date, "%b %y")


p<-ggplot(data=dfm, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.5, position=position_dodge(width = 0.8)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800), labels = paste(c(0, 200, 400, 600, 800), 'K', sep='')) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("Disability Support Pension", 
                                            "Newstart recipients", 
                                            "Newstart recipients with partial capacity to work", 
                                            "Shift from DSP to Newstart, with partial capacity to work")) +
  labs(x = "", y="") +
  theme(
    legend.position="bottom",
    legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
        axis.text.y = element_text())

p

ggsave("chart.png")