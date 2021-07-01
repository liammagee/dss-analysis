library(readxl)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(reshape2)

df <- data.frame('Date'=lubridate::ymd(), 'Partial.NS'=integer(), 'Total.NS'=integer(), 'Total.DSP'=integer(), 'Total.Aged'=integer(), 
                 'DSP.16.20'=integer(), 'DSP.21.24'=integer(), 'DSP.25.34'=integer(), 'DSP.35.44'=integer(), 'DSP.45.54'=integer(), 'DSP.55.64'=integer(), 'DSP.65'=integer())



addPeriod <- function(df, file, year, month, sheetTotal = 4, sheetAge = 7, sheetPartial = 19, colDSP = 10, rowDSPAge = 13, colAged = 4, colNS = 16, rowPartial = 3) {
  period.ALL <- read_excel(file, sheetTotal)
  period.AGES <- read_excel(file, sheetAge)
  period.NS <- read_excel(file, sheetPartial)
  
  
  dsp.total <- as.numeric(period.ALL[12, colDSP])
  aged.total <- as.numeric(period.ALL[12, colAged])
  ns.total <- as.numeric(period.ALL[12, colNS])
  ns.partial <- as.numeric(period.NS[rowPartial, 3])
  
  dsp.16.20 <- as.numeric(period.AGES[rowDSPAge, 84])
  dsp.21.24 <- as.numeric(period.AGES[rowDSPAge, 85])
  dsp.25.34 <- as.numeric(period.AGES[rowDSPAge, 86])
  dsp.35.44 <- as.numeric(period.AGES[rowDSPAge, 87])
  dsp.45.54 <- as.numeric(period.AGES[rowDSPAge, 88])
  dsp.55.64 <- as.numeric(period.AGES[rowDSPAge, 89])
  dsp.65 <- as.numeric(period.AGES[rowDSPAge, 90])
  # dsp.total <- as.numeric(period.ALL$X__9[12])
  # ns.total <- as.numeric(period.ALL$X__15[12])
  # ns.partial <- as.numeric(period.NS$X__2[3])
  df %>% add_row('Partial.NS'=ns.partial, 'Total.NS'=ns.total, 'Total.DSP'=dsp.total, 'Total.Aged'=aged.total, 
                 'DSP.16.20'=dsp.16.20, 'DSP.21.24'=dsp.21.24, 'DSP.25.34'=dsp.25.34, 'DSP.35.44'=dsp.35.44, 'DSP.45.54'=dsp.45.54, 'DSP.55.64'=dsp.55.64, 'DSP.65'=dsp.65,
                 'Date' = ymd(paste(year, '/', month, '/', 1)) )
}


# period.ALL <- read_excel('dss-demographics-march-2020.xlsx', 4)
# period.NS <- read_excel('dss-payment-demographics-dataset-december-2018.xlsx', 19)
# period.ALL[12,11]
# period.ALL$...11[12]
# period.ALL$...16[12]
# period.ALL$...3[3]
# dsp.total <- as.numeric(period.ALL$X__9[12])
# ns.total <- as.numeric(period.ALL$X__15[12])
# ns.partial <- as.numeric(period.NS$X__2[3])

loadData <- function(df) {
  df <- addPeriod(df, 'dss-demographics-march-2021-final.xlsx', 2021, 3, colNS = 15)
  
  
  df <- addPeriod(df, 'dss-demographics-december-2020-final.xlsx', 2020, 12, colNS = 15)
  df <- addPeriod(df, 'dss-demographics-september-2020.xlsx', 2020, 9, colNS = 15)
  df <- addPeriod(df, 'dss-demographics-june-2020.xlsx', 2020, 6, colNS = 15)
  df <- addPeriod(df, 'dss-demographics-march-2020.xlsx', 2020, 3, colDSP = 11, rowDSPAge = 14, colNS = 16)
  
  df <- addPeriod(df, 'dss-demographics-dec-2019-final.xlsx', 2019, 12)
  df <- addPeriod(df, 'dss-demographics-sept-2019-final.xlsx', 2019, 9)
  df <- addPeriod(df, 'dss-demographics-june-2019-4.xlsx', 2019, 6, rowPartial = 12)
  df <- addPeriod(df, 'dssdemographics-march-2019-conf-final.xlsx', 2019, 3)
  
  df <- addPeriod(df, 'dss-payment-demographics-dataset-december-2018.xlsx', 2018, 12)
  df <- addPeriod(df, 'dssdemographics-september-2018-conf.xlsx', 2018, 9, sheetPartial = 18)
  df <- addPeriod(df, 'dss-demographics-june-2018.xlsx', 2018, 6)
  df <- addPeriod(df, 'dss-demographics-march-2018.xlsx', 2018, 3)
  
  df <- addPeriod(df, 'dss-demographics-december-2017.xlsx', 2017, 12)
  df <- addPeriod(df, 'dss-demographics-september-2017.xlsx', 2017, 9)
  df <- addPeriod(df, 'dss-demographics-june-2017.xlsx', 2017, 6)
  df <- addPeriod(df, 'dss-demographics-march-2017.xlsx', 2017, 3)
  
  df <- addPeriod(df, 'dss-demographics-december-2016.xlsx', 2016, 12, rowDSPAge = 12)
  df <- addPeriod(df, 'dss-demographics-september-2016.xlsx', 2016, 9, rowDSPAge = 12, sheetPartial = 18)
  df <- addPeriod(df, 'dss-demographics-june-2016.xlsx', 2016, 6, rowDSPAge = 12, sheetPartial = 18)
  df <- addPeriod(df, 'dss-demographics-march-2016.xlsx', 2016, 3, rowDSPAge = 12, sheetPartial = 18)
  
  df <- addPeriod(df, 'demographics-december-2015-conf.xlsx', 2015, 12, rowDSPAge = 12, sheetPartial = 18)
  df <- addPeriod(df, '201509-dssdemographics.xlsx', 2015, 9, rowDSPAge = 12, sheetPartial = 18)
  df <- addPeriod(df, 'dss-demographics-june-2015.xlsx', 2015, 6, rowDSPAge = 12, sheetPartial = 18)
  df <- addPeriod(df, 'dss-demogrphics-march-2015.xlsx', 2015, 3, rowDSPAge = 12, sheetPartial = 18)
  
  df <- addPeriod(df, 'dssdemographicdecember2014.xlsx', 2014, 12, rowDSPAge = 12, sheetTotal = 2, sheetAge = 5, sheetPartial = 14)
  df <- addPeriod(df, 'dssdemographicsseptember2014.xlsx', 2014, 9, rowDSPAge = 12, sheetTotal = 2, sheetAge = 5, sheetPartial = 14)
  df <- addPeriod(df, 'june-2014-dss-demographics-suppressed-2014-final-ftb.xlsx', 2014, 6, rowDSPAge = 13, sheetTotal = 2, sheetAge = 5, sheetPartial = 14)
  df
  
  # Revise history from June 2019
  
  period.NS <- read_excel('dss-demographics-june-2019-4.xlsx', 19)
  
  ns.p.6.14 <- as.numeric(period.NS$...4[18])
  ns.p.9.14 <- as.numeric(period.NS$...4[21])
  ns.p.12.14 <- as.numeric(period.NS$...4[24])
  ns.p.3.15 <- as.numeric(period.NS$...4[27])
  ns.p.6.15 <- as.numeric(period.NS$...4[30])
  ns.p.9.15 <- as.numeric(period.NS$...4[33])
  ns.p.12.15 <- as.numeric(period.NS$...4[36])
  ns.p.3.16 <- as.numeric(period.NS$...4[39])
  ns.p.6.16 <- as.numeric(period.NS$...4[42])
  ns.p.9.16 <- as.numeric(period.NS$...4[45])
  ns.p.12.16 <- as.numeric(period.NS$...4[48])
  ns.p.3.17 <- as.numeric(period.NS$...4[51])
  ns.p.6.17 <- as.numeric(period.NS$...4[54])
  ns.p.9.17 <- as.numeric(period.NS$...4[57])
  ns.p.12.17 <- as.numeric(period.NS$...4[60])
  ns.p.3.18 <- as.numeric(period.NS$...4[63])
  ns.p.6.18 <- as.numeric(period.NS$...4[66])
  ns.p.9.18 <- as.numeric(period.NS$...4[69])
  ns.p.12.18 <- as.numeric(period.NS$...4[72])
  ns.p.3.19 <- as.numeric(period.NS$...4[75])
  ns.p.6.19 <- as.numeric(period.NS$...4[78])
  
  
  df$Partial.NS.bck <- df$Partial.NS
  df$Partial.NS[8] <- ns.p.6.19
  df$Partial.NS[9] <- ns.p.3.19
  df$Partial.NS[10] <- ns.p.12.18
  df$Partial.NS[11] <- ns.p.9.18
  df$Partial.NS[12] <- ns.p.6.18
  df$Partial.NS[13] <- ns.p.3.18
  df$Partial.NS[14] <- ns.p.12.17
  df$Partial.NS[15] <- ns.p.9.17
  df$Partial.NS[16] <- ns.p.6.17
  df$Partial.NS[17] <- ns.p.3.17
  df$Partial.NS[18] <- ns.p.12.16
  df$Partial.NS[19] <- ns.p.9.16
  df$Partial.NS[20] <- ns.p.6.16
  df$Partial.NS[21] <- ns.p.3.16
  df$Partial.NS[22] <- ns.p.12.15
  df$Partial.NS[23] <- ns.p.9.15
  df$Partial.NS[24] <- ns.p.6.15
  df$Partial.NS[25] <- ns.p.3.15
  df$Partial.NS[26] <- ns.p.12.14
  df$Partial.NS[27] <- ns.p.9.14
  df$Partial.NS[28] <- ns.p.6.14
  #df <- addPeriod(df, 'dssdemographicsmarch2014.xlsx', 'Mar14', 2014, 3, sheetTotal = 2, sheetPartial = 14)
  
  #df <- addPeriod(df, 'dssdemographicsseptember2013.xlsx', 'Dec13', 2013, 12, sheetTotal = 2, sheetPartial = 14)
  #df <- addPeriod(df, 'dssdemographicsseptember2013.xlsx', 'Sep13', 2013, 9, sheetTotal = 2, sheetPartial = 14)
  df
  
}

df <- loadData(df)
df[,c('Date', 'DSP.45.54')]


df$Diff.DSP <- df$Total.DSP - df[1,'Total.DSP']
df$Diff.Aged <- df$Total.Aged - df[length(df$Total.Aged),'Total.Aged']
df$Diff.Total.NS <- df$Total.NS - df[length(df$Total.NS),'Total.NS']
df$Diff.Partial.NS <- df$Partial.NS - df[length(df$Partial.NS),'Partial.NS']

dfm <- melt(df, id.vars = c("Date"), measure.vars = c('Total.DSP', 'Total.NS', 'Partial.NS'))
dfm$strdate <- strftime(dfm$Date, "%b %y")
dfm
dfm2 <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.DSP', 'Diff.Partial.NS'))
dfm2$strdate <- strftime(dfm2$Date, "%b %y")
dfm2a <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.Aged', 'Diff.Partial.NS'))
dfm2a$strdate <- strftime(dfm2$Date, "%b %y")
dfm2c <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.Aged', 'Diff.Partial.NS'))
dfm2c$strdate <- strftime(dfm2$Date, "%b %y")

p1 <- ggplot(data=dfm, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.6, position=position_dodge(width = 0.7)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800), 
                     labels = paste(c(0, 200, 400, 600, 800), 'K', sep='')) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("Disability Support Pension", 
                                            "Newstart/JobSeeker recipients", 
                                            "Newstart/JobSeeker recipients with partial capacity to work")) +
  labs(title = "DSP, JobSeeker, JobSeeker with Partial Capacity to Work", subtitle = "June 2014 to March 2021. Source: DSS Payment Demographic Data.", x = "", y="") +
  theme(
    legend.position="bottom",
    legend.direction = "vertical",
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
        axis.text.y = element_text())

p1
ggsave("chart1.png", p1)



p2<-ggplot(data=dfm2, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.5, position=position_dodge(width = 0.8)) +
  scale_x_discrete(breaks = dfm2$strdate, labels = dfm2$strdate) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180), 
                     labels = paste(c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180), 'K', sep='')) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("Change in Disability Support Pension", 
                                                           "Change in Newstart/JobSeeker recipients with partial capacity to work")) +
  labs(title = "Changes from Lowpoint", subtitle = "(DSP lowpoint = Mar 21; JobSeeker with partial capacity to work lowpoint = Jun 14)", x = "", y="") +
  theme_bw()  +
  theme(
    legend.position="bottom",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())

p2
ggsave("chart2.png", p2)



# Percentile change
dsp <- rev(df$Total.DSP)
dsp.pct <- (dsp / lag(dsp) - 1) * 100
ns <- rev(df$Total.NS)
ns.pct <- (ns / lag(ns) - 1) * 100
pns <- rev(df$Partial.NS)
pns.pct <- (pns / lag(pns) - 1) * 100
date <- rev(df$Date)
strdate <- strftime(date, "%b %y")
res <- data.frame(date, strdate,  dsp, dsp.pct, ns, ns.pct, pns, pns.pct, stringsAsFactors = F)


# Get pop changes
pop.growth <- read.csv('Annual population growth rate(a)(b).csv') 
pop.figures <- read_excel('310101_21.xls', 2)
ann.pop <- pop.figures[142:167,12]
colnames(ann.pop)[0] <- 'erp'
head(ann.pop)

# June 2014 to September 2020
pop.growth <- pop.growth[57:79, 2]


# Remove last two quarters
res <- res[0:26,]
res$erp <- as.numeric(unlist(ann.pop))
#res$date <- as.Date(res$date, "%Y-%m-%d")
res$dsp <- as.numeric(as.character(res$dsp))
res$dsp.pct <- as.numeric(as.character(res$dsp.pct))
res$ns <- as.numeric(as.character(res$ns))
res$ns.pct <- as.numeric(as.character(res$ns.pct))
res$pns <- as.numeric(as.character(res$pns))
res$pns.pct <- as.numeric(as.character(res$pns.pct))
res$strdate <- as.character(res$strdate)
res$erp.pct <- (res$erp / lag(res$erp) - 1) * 100
res$erp.std <- (res$erp / tail(res$erp, n=1)) * 100
res$dsp.adj.pct <- res$dsp.pct- res$erp.pct
res$ns.adj.pct <- res$ns.pct - res$erp.pct
res$pns.adj.pct <- res$pns.pct - res$erp.pct
res$dsp.std <- res$dsp * res$erp.std
res$ns.std <- res$ns - res$erp.pct
res$pns.std <- res$pns - res$erp.pct
res

ann.pop.rev <- rev(as.numeric(unlist(ann.pop)))
df$erp <- head(ann.pop.rev, n =1)
df$erp[3:28] <- ann.pop.rev
df$erp.pct <- (df$erp / lag(df$erp) - 1) * 100
df$erp.std <- (df$erp / head(df$erp, n=1)) * 100
df$Total.DSP.std <- df$Total.DSP * (1 / (df$erp.std / 100))
df$Total.Aged.std <- df$Total.Aged * (1 / (df$erp.std / 100))
df$Total.NS.std <- df$Total.NS * ( 1 / (df$erp.std / 100))
df$Partial.NS.std <- df$Partial.NS * ( 1 / (df$erp.std / 100))
df$erp.std
dfm <- melt(df, id.vars = c("Date"), measure.vars = c('Total.DSP.std', 'Total.NS.std', 'Partial.NS.std'))
dfm$strdate <- strftime(dfm$Date, "%b %y")
dfm

df$Diff.Aged.std <- df$Total.Aged.std - df[length(df$Total.Aged),'Total.Aged.std']
df$Diff.Partial.NS.std <- df$Partial.NS.std - df[length(df$Partial.NS.std),'Partial.NS.std']


dfm2a <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.Aged', 'Diff.Partial.NS'))
dfm2a$strdate <- strftime(dfm2$Date, "%b %y")

dfm2b <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.Aged.std', 'Diff.Partial.NS.std'))
dfm2b$strdate <- strftime(dfm2$Date, "%b %y")


p1a <- ggplot(data=dfm, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.6, position=position_dodge(width = 0.7)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800), 
                     labels = paste(c(0, 200, 400, 600, 800), 'K', sep='')) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("Disability Support Pension, Adjusted for population", 
                                                           "Newstart/JobSeeker recipients, Adjusted for population", 
                                                           "Newstart/JobSeeker recipients with partial capacity to work, Adjusted for population")) +
  labs(title = "DSP, JobSeeker, JobSeeker with Partial Capacity to Work", 
       subtitle = "June 2014 to March 2021. Source: DSS Payment Demographic Data. ", x = "", y="") +
  theme(
    legend.position="bottom",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())

p1a
ggsave("chart1a.png", p1a)

df[,c('Date', 'Total.DSP')]



p2a<-ggplot(data=dfm2a, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.5, position=position_dodge(width = 0.8)) +
  scale_x_discrete(breaks = dfm2$strdate, labels = dfm2$strdate) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180), 
                     labels = paste(c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180), 'K', sep='')) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("Change in Aged Pension", 
                                                           "Change in Newstart/JobSeeker recipients with partial capacity to work")) +
  labs(title = "Changes from Starting Point", subtitle = "Aged Pension, JobSeeker with partial capacity to work lowpoint, June 2014 to March 2021", x = "", y="") +
  theme_bw()  +
  theme(
    legend.position="bottom",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())

p2a
ggsave("chart2a.png", p2a)


p2b<-ggplot(data=dfm2a, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.5, position=position_dodge(width = 0.8)) +
  scale_x_discrete(breaks = dfm2$strdate, labels = dfm2$strdate) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180), 
                     labels = paste(c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180), 'K', sep='')) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("Change in Aged Pension", 
                                                           "Change in Newstart/JobSeeker recipients with partial capacity to work")) +
  labs(title = "Changes from Starting Point, Adjusted for Population", subtitle = "Aged Pension, JobSeeker with partial capacity to work lowpoint, June 2014 to March 2021", x = "", y="") +
  theme_bw()  +
  theme(
    legend.position="bottom",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())

p2b
ggsave("chart2b.png", p2b)



p3<-ggplot(data=res, aes(x=reorder(strdate, date), y=dsp.adj.pct))+
  geom_bar(stat="identity", aes(fill=dsp.adj.pct), width = 0.5, position=position_dodge(width = 0.8)) +  
  scale_x_discrete(breaks = res$strdate, labels = res$strdate)+
  labs(title = "Disability Support Pension Figures", subtitle = "Quarterly Percentile Change, Adjusted for Population Growth", x = "Quarters (June 2014 - September 2020)", y="% Change from Previous Quarter", fill = "% Change") +
  theme_bw()  +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75),
    axis.text.y = element_text())
p3
ggsave("chart3.png", p3)
write.csv(res, "dsp.csv")

p4<-ggplot(data=res, aes(x=reorder(strdate, date), y=ns.adj.pct))+
  geom_bar(stat="identity", aes(fill=ns.adj.pct), width = 0.5, position=position_dodge(width = 0.8)) +  
  scale_x_discrete(breaks = res$strdate, labels = res$strdate)+
  labs(title = "JobSeeker Figures", subtitle = "Quarterly Percentile Change, Adjusted for Population Growth", x = "Quarters (June 2014 - September 2020)", y="% Change from Previous Quarter", fill = "% Change") +
  theme_bw()  +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75),
    axis.text.y = element_text())
p4
ggsave("chart4.png", p4)
write.csv(res, "ns.csv")

p5<-ggplot(data=res, aes(x=reorder(strdate, date), y=pns.adj.pct))+
  geom_bar(stat="identity", aes(fill=pns.adj.pct), width = 0.5, position=position_dodge(width = 0.8)) +  
  scale_x_discrete(breaks = res$strdate, labels = res$strdate)+
  labs(title = "JobSeeker with Partial Capacity to Work Figures", subtitle = "Quarterly Percentile Change, Adjusted for Population Growth", x = "Quarters (June 2014 - September 2020)", y="% Change from Previous Quarter", fill = "% Change") +
  theme_bw()  +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.75, hjust = 0.75),
    axis.text.y = element_text())
p5
ggsave("chart5.png", p5)
write.csv(res, "pns.csv")

df[,c('Date', 'Partial.NS')]
df[1, 'df$DSP.16.20']
(df$DSP.16.20 / df[1, 'df$DSP.16.20'])


df$Pct.DSP.16.20 <- ( ( df$DSP.16.20 / df[length(df$DSP.16.20), 'DSP.16.20'])  - 1) * 100
df$Pct.DSP.21.24 <- ( ( df$DSP.21.24 / df[length(df$DSP.21.24), 'DSP.21.24'])  - 1) * 100
df$Pct.DSP.25.34 <- ( ( df$DSP.25.34 / df[length(df$DSP.25.34), 'DSP.25.34'])  - 1) * 100
df$Pct.DSP.35.44 <- ( ( df$DSP.35.44 / df[length(df$DSP.35.44), 'DSP.35.44'])  - 1) * 100
df$Pct.DSP.45.54 <- ( ( df$DSP.45.54 / df[length(df$DSP.45.54), 'DSP.45.54'])  - 1) * 100
df$Pct.DSP.55.64 <- ( ( df$DSP.55.64 / df[length(df$DSP.55.64), 'DSP.55.64'])  - 1) * 100
df$Pct.DSP.65 <- ( ( df$DSP.65 / df[length(df$DSP.65), 'DSP.65']) - 1) * 100
df$Pct.Partial.NS <- ( ( df$Partial.NS / df[length(df$Partial.NS), 'Partial.NS'] ) - 1) * 100

dfm6 <- melt(df, id.vars = c("Date"), measure.vars = c('Pct.DSP.16.20', 'Pct.DSP.21.24', 'Pct.DSP.25.34', 'Pct.DSP.35.44', 'Pct.DSP.45.54', 'Pct.DSP.55.64', 'Pct.DSP.65', 'Pct.Partial.NS'))
dfm6$strdate <- strftime(dfm6$Date, "%b %y")
dfm6a <- melt(df, id.vars = c("Date"), measure.vars = c('Pct.DSP.16.20', 'Pct.DSP.21.24', 'Pct.DSP.25.34', 'Pct.DSP.35.44', 'Pct.DSP.45.54', 'Pct.DSP.55.64', 'Pct.DSP.65'))
dfm6a$strdate <- strftime(dfm6a$Date, "%b %y")

head(df)

p6 <- ggplot(data=dfm6, aes(x=reorder(strdate, Date), y=value)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.6, position=position_dodge(width = 0.7)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(labels = function(b) { paste(b, '%', sep='')}) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("DSP, Aged 16-20", 
                                                           "DSP, Aged 21-24", 
                                                           "DSP, Aged 25-34", 
                                                           "DSP, Aged 35-44", 
                                                           "DSP, Aged 45-54", 
                                                           "DSP, Aged 55-64", 
                                                           "DSP, Aged 65+",
                                                           "JobSeeker, PCW")) +
  labs(title = "DSP by Age; JobSeeker with Partial Capacity to Work (PCW)", subtitle = "Percentile Change from June 2014, to March 2021. Source: DSS Payment Demographic Data.", x = "", y="") +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())
p6
ggsave("chart6.png", p6)

p6a <- ggplot(data=dfm6a, aes(x=reorder(strdate, Date), y=value)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.6, position=position_dodge(width = 0.7)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(labels = function(b) { paste(b, '%', sep='')}) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("DSP, Aged 16-20", 
                                                           "DSP, Aged 21-24", 
                                                           "DSP, Aged 25-34", 
                                                           "DSP, Aged 35-44", 
                                                           "DSP, Aged 45-54", 
                                                           "DSP, Aged 55-64", 
                                                           "DSP, Aged 65+")) +
  labs(title = "DSP by Age", subtitle = "Percentile Change from June 2014, to March 2021. Source: DSS Payment Demographic Data.", x = "", y="") +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())
p6a
ggsave("chart6a.png", p6a)


df$Diff.DSP.16.20 <- ( ( df$DSP.16.20 - df[length(df$DSP.16.20), 'DSP.16.20']) ) 
df$Diff.DSP.21.24 <- ( ( df$DSP.21.24 - df[length(df$DSP.21.24), 'DSP.21.24']) ) 
df$Diff.DSP.25.34 <- ( ( df$DSP.25.34 - df[length(df$DSP.25.34), 'DSP.25.34']) ) 
df$Diff.DSP.35.44 <- ( ( df$DSP.35.44 - df[length(df$DSP.35.44), 'DSP.35.44']) ) 
df$Diff.DSP.45.54 <- ( ( df$DSP.45.54 - df[length(df$DSP.45.54), 'DSP.45.54']) ) 
df$Diff.DSP.55.64 <- ( ( df$DSP.55.64 - df[length(df$DSP.55.64), 'DSP.55.64']) ) 
df$Diff.DSP.65 <- ( ( df$DSP.65 - df[length(df$DSP.65), 'DSP.65'])) 

dfm7 <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.DSP.16.20', 'Diff.DSP.21.24', 'Diff.DSP.25.34', 'Diff.DSP.35.44', 'Diff.DSP.45.54', 'Diff.DSP.55.64', 'Diff.DSP.65', 'Diff.Partial.NS'))
dfm7$strdate <- strftime(dfm7$Date, "%b %y")
dfm7a <- melt(df, id.vars = c("Date"), measure.vars = c('Diff.DSP.16.20', 'Diff.DSP.21.24', 'Diff.DSP.25.34', 'Diff.DSP.35.44', 'Diff.DSP.45.54', 'Diff.DSP.55.64', 'Diff.DSP.65'))
dfm7a$strdate <- strftime(dfm7a$Date, "%b %y")



p7 <- ggplot(data=dfm7, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.6, position=position_dodge(width = 0.7)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(labels = function(b) { paste(b, 'K', sep='')}) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("DSP, Aged 16-20", 
                                                           "DSP, Aged 21-24", 
                                                           "DSP, Aged 25-34", 
                                                           "DSP, Aged 35-44", 
                                                           "DSP, Aged 45-54", 
                                                           "DSP, Aged 55-64", 
                                                           "DSP, Aged 65+",
                                                           "JobSeeker, PCW")) +
  labs(title = "DSP by Age; JobSeeker with Partial Capacity to Work", subtitle = "Change from June 2014, to March 2021. Source: DSS Payment Demographic Data.", x = "", y="") +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())


p7
ggsave("chart7.png", p7)

p7a <- ggplot(data=dfm7a, aes(x=reorder(strdate, Date), y=value / 1000)) +
  geom_bar(stat="identity", aes(fill=variable), width = 0.6, position=position_dodge(width = 0.7)) +
  theme_bw()  +
  scale_x_discrete(breaks = dfm$strdate, labels = dfm$strdate) +
  scale_y_continuous(labels = function(b) { paste(b, 'K', sep='')}) +
  scale_fill_brewer(palette="Dark2", name = "", labels = c("DSP, Aged 16-20", 
                                                           "DSP, Aged 21-24", 
                                                           "DSP, Aged 25-34", 
                                                           "DSP, Aged 35-44", 
                                                           "DSP, Aged 45-54", 
                                                           "DSP, Aged 55-64", 
                                                           "DSP, Aged 65+")) +
  labs(title = "DSP by Age", subtitle = "Change from June 2014, to March 2021. Source: DSS Payment Demographic Data.", x = "", y="") +
  theme(
    legend.position="right",
    legend.direction = "vertical",
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75),
    axis.text.y = element_text())


p7a
ggsave("chart7s.png", p7a)

