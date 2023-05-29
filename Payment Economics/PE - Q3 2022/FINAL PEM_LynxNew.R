# Payment Economics - R script
# Created by: Shaun Byck
# Last updated: 2021-04-15


##clear system memory and workspace
gc();
rm(list=ls());
gc();





##Specify list of packages used in code
usedPackagesList <- c("ggplot2","ggrepel","tidyr","dplyr","lubridate","readr","zoo",
                      "quantmod","bigrquery","DBI","stringr","gridExtra","grid","Quandl",
                      "bdscale", "bizdays", "data.table", "scales");

##check to see if required packages are installed.
#if not install and load them them
##else looad them
ceckPagagesInstalled <- lapply(
  usedPackagesList,
  FUN = function(x){
    if(!require(x,character.only = TRUE)){
      install.packages(x, dependencies = TRUE);
      require(x,character.only = TRUE)
    }
  }
);

##List all loaded packaged
search();

windowsFonts("Roboto-Light" = windowsFont("Roboto-Light"))

# Set working directory to save plots ###
# Change to your wd - will save ALL plots to this folder!
setwd("G:/Development Workspaces/R-Workspace/PEM/2022_q3");


##Setting the relevant date varriables for the quarter's PE report
### UPDATE THIS EACH PE RELEASE
startDateBQ <- "'2020-04-01'";
endDateBQ <- "'2022-09-30'";

LynxstartDateBQ <- "'2022-04-01'";
LynxstartCycleDateBQ <- "'2022-04-01'";
LynxstartEffectiveDateBQ <- "'2021-08-01'";
endDateBQ <- "'2022-09-30'";

##update these to the first and last days of the quaters being looked at in PE
startEffectiveDateBQ <- "'2022-04-01'";
endEffectiveDateBQ <- "'2022-09-30'";
startCycleDateBQ <- "'2022-04-01'";
endCycleDateBQ <- "'2022-09-30'"; 

##Change these filters to the date range for the quarter being looked at in PE (i.e. first and last days of the quarter)
comparisonFilterStartDate1 <- '2020-07-01'; 
comparisonFilterEndDate1 <- '2020-09-30';

comparisonFilterStartDate2 <- '2021-07-01'; 
comparisonFilterEndDate2 <- '2021-09-30';

comparisonFilterStartDate3 <- '2022-07-01'; 
comparisonFilterEndDate3 <- '2022-09-30';

##This filter is used to select all transactions from the start of the COVID plandemic.
##picking the whole month since some of the impacts of COVID had started prior to the 
##initiation of lockdowns
acssFilterEndDate <- '2020-03-01';

##Change these for the years and quaters being compared in the current PE report 
##(i.e. current year and quarter vs same quarter in previous year)
acssComparisonFilterBenchmarkYear <- '2022';
acssComparisonFilterComparisonYear <- '2021';
acssComparisonFilterBenchmarkQuarter <- 'Q3';
acssComparisonFilterComparisonQuarter <- 'Q3';

###SQL Queries used in code
##This is done upfront to make debugging the code easier

paymentsDataSQL <- str_c("
                         SELECT 
                         EXTRACT(YEAR from cycle_date) AS year,
                         cycle_date AS date,
                         SUM(value)/1e9 AS lynx_value,
                         FROM `prod-data-storage-prj.high_value_marts.vol_val_fact`
                         WHERE (cycle_date > ", startDateBQ, " AND cycle_date <= ", endDateBQ,")
                         GROUP BY cycle_date
                         ORDER BY cycle_date
                         ");

LynxDataSQL <- str_c("
                         SELECT 
                         EXTRACT(YEAR from cycle_date) AS year,
                         cycle_date AS date,
                         SUM(value)/1e9 AS lynx_value,
                         settlement_mechanism
                         FROM `prod-data-storage-prj.high_value_marts.vol_val_fact`
                         WHERE (cycle_date > ", LynxstartDateBQ, " AND cycle_date <= ", endDateBQ,")
                         GROUP BY cycle_date, settlement_mechanism
                         ORDER BY cycle_date, settlement_mechanism
                         ");


HighVolumerawSQL <- str_c("
                          SELECT 
                         EXTRACT(YEAR from cycle_date) AS year,
                         cycle_date AS date,
                         SUM(volume) AS lynx_volume,
                         FROM `prod-data-storage-prj.high_value_marts.vol_val_fact`
                         WHERE (cycle_date > ", startDateBQ, " AND cycle_date <= ", endDateBQ,")
                         GROUP BY cycle_date
                         ORDER BY cycle_date
                          ");

LynxVolumeDataSQL <- str_c("
                         SELECT 
                         EXTRACT(YEAR from cycle_date) AS year,
                         cycle_date AS date,
                         SUM(volume) AS lynx_volume,
                         settlement_mechanism
                         FROM `prod-data-storage-prj.high_value_marts.vol_val_fact`
                         WHERE (cycle_date > ", LynxstartDateBQ, " AND cycle_date <= ", endDateBQ,")
                         GROUP BY cycle_date, settlement_mechanism
                         ORDER BY cycle_date, settlement_mechanism
                         ");


#change the quarter!!
yoyLVTSSQL <- str_c("
                    SELECT
                    EXTRACT(YEAR FROM cycle_date) AS year,
                    EXTRACT(QUARTER FROM cycle_date) AS quarter,
                    SUM(t1_value + t2_value) AS value,
                    SUM(t1_volume + t2_volume) AS volume
                    FROM `prod-data-storage-prj.lvts_boc.daily_vol_val_boc1` AS t1
                    WHERE cycle_seq_num=(SELECT MAX(cycle_seq_num) 
                    FROM `prod-data-storage-prj.lvts_boc.daily_vol_val_boc1` as t2 where t2.cycle_date = t1.cycle_date)
                    AND EXTRACT(YEAR FROM cycle_date) in (2021,2022)
                    AND EXTRACT(QUARTER FROM cycle_date) = 1
                    GROUP BY year, quarter
                    ORDER BY year, quarter
                    ");

yoyLynxSQL <- str_c("
                    SELECT
                    EXTRACT(YEAR FROM cycle_date) AS year,
                    EXTRACT(QUARTER FROM cycle_date) AS quarter,
                    settlement_mechanism,
                    SUM(value) AS value,
                    SUM(volume) AS volume
                    FROM `prod-data-storage-prj.high_value_marts.vol_val_fact`
                    GROUP BY year, quarter, settlement_mechanism
                    ORDER BY year, quarter, settlement_mechanism
                    ");



Lynx_credit_sql <- str_c("
                      SELECT 
                      cycle_date AS Date,
                      participant_bic AS participant,
                      MAX(total_amount)/1e9 AS t2ndc
                      FROM `prod-data-storage-prj.lynx_research.credit_activity`
                      WHERE (cycle_date > '2022-07-01' AND cycle_date <= ", endEffectiveDateBQ,")
                      GROUP BY Date, participant 
                      ORDER BY Date, participant
                      ");


mnpLynxDataSQL <- str_c("
                    SELECT
                    cycle_date AS date,
                    inst_lynx_id AS participant,
                    SUM(value_received-value_sent) AS net_position
                    FROM `prod-data-storage-prj.high_value_marts.position_fact`
                    WHERE (cycle_date > ", LynxstartCycleDateBQ, " AND cycle_date <= ", endCycleDateBQ,")
                    GROUP BY date, participant
                    ");

acssDataSQL <- str_c("
                     SELECT
                     settlement_year AS year,
                     settlement_date AS date,
                     current_stream_name_en AS stream,
                     SUM(value) AS total_value,
                     SUM(volume) AS total_volume
                     FROM `prod-data-storage-prj.acss_marts.exchange_vol_val_fact`
                     WHERE (settlement_date > ", startDateBQ, " AND settlement_date <= ", endDateBQ,")
                     AND system_code = 'A'
                     GROUP BY year, date, stream
                     ORDER BY date, stream
                     ");


# custom plot theme (flexin on em!)
# define plot elements, eg colors, fonts, etc
my_theme <- function() {
  
  font <- "Roboto-Light"
  
  fontSize <- 11
  
  ggplot2::theme(plot.title = element_text(family=font,
                                           size=fontSize+2,
                                           face='bold',
                                           color='#4b4f54'),
                 
                 plot.subtitle = element_text(family=font,
                                              size=fontSize,
                                              color='#4b4f54'),
                 
                 legend.position ='top',
                 
                 legend.justification = 'left',
                 
                 legend.title=element_blank(),
                 
                 legend.text = element_text(family=font,
                                            size=fontSize,
                                            color='#4b4f54'),
                 
                 axis.title = element_text(family=font,
                                           size=fontSize,
                                           color='#4b4f54'),
                 
                 axis.text = element_text(family=font,
                                          size=fontSize,
                                          color='#4b4f54'),
                 
                 plot.caption = element_text(family=font,
                                             size=10,
                                             color='#4b4f54',
                                             hjust=0),
                 
                 panel.background = element_rect(fill='transparent'),
                 strip.background = element_blank(),
                 strip.placement = 'outside',
                 strip.text = element_text(size=11, family='Roboto-Light', color='#4b4f54', face='bold', hjust = 0)
  )
  
}




### Establish BigQuery connection ###
bQconnection <- dbConnect(
  bigrquery::bigquery(),
  project = "prod-data-storage-prj",
  dataset = "lvts_boc",
  billing = "acs-research-prj"
)

# test connection
bQconnection

1

# set so big query can download more records than default (100k or something)
options(scipen = 20)

### GDP 
gdpdataCVSFile <- read.csv("G:/Development Workspaces/R-Workspace/Nowcasting/gdp.csv");

gdp <- gdpdataCVSFile %>%
  mutate(VALUE = VALUE/1e3) %>%
  filter(REF_DATE >= '2020-01-01') %>%
  ungroup()

gdp <- gdp %>%
  mutate(day = ymd(str_c('2022-',str_sub(as.character(REF_DATE),6))))

gdpPlot <- ggplot(gdp, aes(x=day,y=VALUE,col=as.factor(year)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+ # can add this back for Q2-Q4 to highlight current Q (doesn't work much for Q1)
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  labs(x=element_blank(),
       y='CAD (000,000,000)',
       title='Figure 1: Real Canadian Monthly GDP follows an upward trend',
       subtitle=str_c('Value of Real GDP by year', sep = ''),
       caption='Source: Statistics Canada')+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1))+
  scale_x_date(date_breaks="1 month", date_labels="%b")

gdpPlot
ggsave("gdp.png",gdpPlot,width=6.5,height=4.5,units='in',dpi=300, scale=1)


## LVTS value/volume########################################################################################################################################################

#BoC vs non-BoC
payments2021 <- str_c("
SELECT
  cycle_date AS date,
  sending_inst_short_name_en AS participant,
  SUM(value) AS part_value,
  SUM(volume) AS part_volume
FROM `prod-data-storage-prj.high_value_marts.vol_val_fact`
WHERE EXTRACT(YEAR FROM cycle_date) IN (2022,2021,2020,2019,2018,2017,2016)
GROUP BY cycle_date, participant
")
payments2021 <- dbGetQuery(bQconnection,payments2021)


bocPayments <- payments2021 %>%
  mutate(boc = if_else(participant=='BCAN','Bank of Canada','Other High-value Participants')) %>%
  group_by(date, boc) %>%
  summarise(tot_value = sum(part_value)) %>%
  ungroup() %>%
  group_by(boc) %>%
  arrange(date) %>%
  mutate(ma_value = rollmean(tot_value, k=5, align='right', fill=NA)) %>%
  filter(date>='2021-07-01') # only want to show 2020 for this plot

# plot (fig 3b)
ggplot(bocPayments, aes(x=date,y=ma_value/1e9,col=boc))+
  geom_line(size = 1)+
  theme_bw()+
  my_theme()+
  scale_color_manual(values=c('#1380A1', '#990000'))+
  scale_x_date(date_breaks = "2 month" , date_labels='%b %y')+
  labs(y='CAD, billions',
       x=element_blank(),
       subtitle='5-day moving average, High-value payment value',
       title='Figure 2b: Bank of Canada driving historic High-vaue values')

ggsave("lvts_value_boc2.png",width=4,height=3,units='in',dpi=300, scale=1.5)


# High-value payment system values by year

paymentsData <- dbGetQuery(bQconnection,paymentsDataSQL)
# 5-day moving average
# filter for months in Q1 and 2019-2021 (last 3 years)

payments <- paymentsData %>%
  mutate(ma_value = rollmean(lynx_value, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  ungroup()

# strip just month and day from 'date' and paste 2021 - need same date for x axis in plot (can paste whichever year, will format x-axis %M)
# EXTACT(YEAR from cycle_date) AS year will give us year for grouping in plot
# paste "Tranche" for plotting

payments <- payments %>%
  mutate(day = ymd(str_c('2022-',str_sub(as.character(date),6))))

HighValuePlot <- ggplot(payments, aes(x=day,y=ma_value,col=as.factor(year)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+ # can add this back for Q2-Q4 to highlight current Q (doesn't work much for Q1)
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  labs(x=element_blank(),
       y='CAD (000,000,000)',
       title='Figure 2a: High-value payment value returned to its last year levels',
       subtitle=str_c('Value of High-value system payments by year, 5-day moving average',
                      sep = ' '))+
  scale_x_date(date_breaks="1 month", date_labels="%B")

HighValuePlot

ggsave("high_value_value.png",HighValuePlot,width=6.5,height=4.5,units='in',dpi=300, scale=1)
write.csv(paymentsData,'LVTS_Lynx_value2.csv')

# Payment volumes by year
HighVolumeraw <- dbGetQuery(bQconnection, HighVolumerawSQL)


# 5-day moving average 
# filter for months in Q4 2019-2021
HighVolume <- HighVolumeraw %>%
  mutate(ma_volume = rollmean(lynx_volume, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  ungroup()

HighVolume <- HighVolume %>%
  mutate(day = ymd(str_c('2022-',str_sub(as.character(date),6))))

HighVolumePlot <- ggplot(HighVolume, aes(x=day,y=ma_volume,col=as.factor(year)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+ # can add this back for Q2-Q4 to highlight current Q (doesn't work much for Q1)
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  labs(x=element_blank(),
       y='Number of transactions',
       title='Figure 3: High-value payment volumes up slightly year-over-year',
       subtitle=str_c('Volume of High-value system payments by year, 5-day moving average',
                      sep = ' '))+
  scale_x_date(date_breaks="1 month", date_labels="%B")

HighVolumePlot

ggsave("high_value_volume.png",HighVolumePlot,width=6.5,height=4.5,units='in',dpi=300, scale=1)


### Lynx values by settlement mechanism

LynxData <- dbGetQuery(bQconnection,LynxDataSQL)

# 5-day moving average by settlement mechanism
# filter for months in Q1 and 2019-2021 (last 3 years)
Lynx_payments <- LynxData %>%
  group_by(settlement_mechanism) %>%
  mutate(ma_value = rollmean(lynx_value, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  ungroup()

Lynx_payments <- Lynx_payments %>%
  mutate(day = ymd(str_c('2022-',str_sub(as.character(date),6))),
         settlement_mechanism = str_c(as.character(settlement_mechanism)))

LynxValuePlot <- ggplot(Lynx_payments, aes(x=day,y=ma_value,col=as.factor(settlement_mechanism)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+ # can add this back for Q2-Q4 to highlight current Q (doesn't work much for Q1)
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  facet_wrap(~settlement_mechanism, scales='free_y',ncol=1)+
  labs(x=element_blank(),
       y='CAD (000,000,000)',
       title='Figure 4: Value of Lynx payments by settlement mechanism',
       subtitle=str_c('5-day moving average, July - October 2022',
                      sep = ' '))+
  scale_x_date(date_breaks="1 month", date_labels="%B")

LynxValuePlot

ggsave("LynxValuePlot.png",LynxValuePlot,width=6.5,height=4.5,units='in',dpi=300, scale=1)

### Lynx volume by settlement mechanism

LynxVolumeData <- dbGetQuery(bQconnection,LynxVolumeDataSQL)

# 5-day moving average by settlement mechanism
# filter for months in Q1 and 2019-2021 (last 3 years)
Lynx_payments_volume <- LynxVolumeData %>%
  group_by(settlement_mechanism) %>%
  mutate(ma_volume = rollmean(lynx_volume, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  ungroup()

Lynx_payments_volume <- Lynx_payments_volume %>%
  mutate(day = ymd(str_c('2022-',str_sub(as.character(date),6))),
         settlement_mechanism = str_c(as.character(settlement_mechanism)))

LynxVolumePlot <- ggplot(Lynx_payments_volume, aes(x=day,y=ma_volume,col=as.factor(settlement_mechanism)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+ # can add this back for Q2-Q4 to highlight current Q (doesn't work much for Q1)
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  facet_wrap(~settlement_mechanism, scales='free_y',ncol=1)+
  labs(x=element_blank(),
       y='Number of transactions',
       title='Figure 5: Volume of Lynx payments by settlement mechanism',
       subtitle=str_c('5-day moving average, July - October 2022',
                      sep = ' '))+
  scale_x_date(date_breaks="1 month", date_labels="%B")

LynxVolumePlot

ggsave("LynxVolumePlot.png",LynxVolumePlot,width=6.5,height=4.5,units='in',dpi=300, scale=1)


### YoY LVTS value/volume growth by Tranche ###
yoyLynx <- dbGetQuery(bQconnection, yoyLynxSQL);

# yoy T1 value
(yoyLVTS$t1_val[2]-yoyLVTS$t1_val[1])/yoyLVTS$t1_val[1]*100

# yoy T1 volume
(yoyLVTS$t1_vol[2]-yoyLVTS$t1_vol[1])/yoyLVTS$t1_vol[1]*100

# yoy T2 value
(yoyLVTS$t2_val[2]-yoyLVTS$t2_val[1])/yoyLVTS$t2_val[1]*100

# yoy T2 volume
(yoyLVTS$t2_vol[2]-yoyLVTS$t2_vol[1])/yoyLVTS$t2_vol[1]*100

# data by paricipant if you want to do similar calculations for BoC/Non-BoC
yoyLVTS_part <- dbGetQuery(bQconnection, yoyLVTS_partSQL);

## Credit ###########################################################################################################################################

# end-of-cycle credit activity by participant for current Q
# end-of-cycle should match Lynx dashboard
Lynx_credit <- dbGetQuery(bQconnection, Lynx_credit_sql);

# system-wide T2 credit LYNX
Lynx_credit <- Lynx_credit %>%
  group_by(Date) %>%
  summarize(credit = sum(t2ndc)) %>% # system-wide T2NDC
  ungroup() 

# for some reason, holidays and weekends are gaps when plotting 'fullCredit' as is
# create business day calendar (removes weekends)
# manually remove holidays - couldn't figure a better/faster way to do, sorry!
create.calendar("QuantLib/Canada/TSX", weekdays=c("saturday","sunday"))
tsx <- bizseq(min(Lynx_credit$Date), max(Lynx_credit$Date)+1, "QuantLib/Canada/TSX")
tsx2 <- tsx[!tsx %in% c(as.Date("2021-09-06"),as.Date("2021-09-30"),as.Date("2021-10-11"),
                        as.Date("2021-12-25"),as.Date("2021-12-26"),as.Date("2021-11-11"),as.Date("2021-12-26"),as.Date("2021-12-27"),as.Date("2021-12-28"),as.Date("2022-01-01"),as.Date("2022-01-03"),as.Date("2022-04-15"),as.Date("2022-05-23"),as.Date("2022-07-01"),as.Date("2022-09-05"))]

Lynx_credit_plot<-ggplot(Lynx_credit, aes(x=Date,y=credit))+
  geom_col(width=0.7)+
  theme_bw()+
  my_theme()+
  scale_x_bd(business.dates=tsx2, max.major.breaks = 12, expand=c(0.01,0.01))+
  scale_fill_manual(values=c('#990000'))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(),
        axis.title.x=element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0, .05)))+
  labs(y='Credit limit, CAD bn',
       x=element_blank(),
       title='Figure 7: Lynx credit activity ',
       subtitle='Intraday credit limits High Value Payments System participants')

Lynx_credit_plot

ggsave("lynx_credit.png", Lynx_credit_plot, width=6.5,height=4.5,units='in',dpi=300, scale=1.25)

## Settlement balances ###########################################################################################################################################

# position.fact has eod position data
mnpLynxData <- dbGetQuery(bQconnection, mnpLynxDataSQL);

# aggregated (it is also anonymized if looking to share external)
setBalAggLynx <- mnpLynxData %>%
  mutate(part = if_else(participant == '1', 'Bank of Canada', 'Lynx Participants')) %>%
  group_by(date, part) %>%
  summarise(lynx_balances = sum(net_position))

setBalAggLynx <- drop_na(setBalAggLynx)

ggplot(setBalAggLynx, aes(x=date,y=lynx_balances/1e9, col=part))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+
  geom_line()+
  theme_bw()+
  my_theme()+
  labs(x=element_blank(),
       y='End-of-day Lynx net position, CAD (000,000,000)',
       title='Figure 6: High-Value Payments System settlement balances',
       subtitle='Multilateral net position of all High-Value Payments System participants at end-of-day\n(i.e., value received - value sent)')+ 
  # can remove \n (ie new line) if smaller plot - up to you
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  scale_y_continuous(position='right')+
  scale_color_manual(values=c('#1380A1', '#990000'))+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(hjust=0))

ggsave("settlement_bal_agg.png",width=5,height=4.5,units='in',dpi=300, scale=1.25)

## ACSS##################################################################################################################################################################################################################

# pull ACSS stream-level data daily (highest needed)
# system code = 'A' is ACSS // 'U' is USBE data (if you need, dont use in PE typically)
acssData <- dbGetQuery(bQconnection, acssDataSQL);

# 5-day moving average value and volume by stream
acssMA <- acssData %>%
  group_by(stream) %>%
  mutate(ma_volume = rollmean(total_volume, k=5, align='right', fill=NA),
         ma_value = rollmean(total_value, k=5, align='right', fill=NA)) %>%
  ungroup() %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  mutate(year = format(date, format = '%Y'),
         stream = if_else(stream=='Q-Point of Service - CR', 'Q-POS - CR',
                          if_else(stream=='P-Point of Service - DR', 'P-POS - DR', stream)), # to fit title on stream-level plots
         day = ymd(str_c('2021-',str_sub(as.character(date),6))))

ggplot(acssMA %>% filter(!stream %in% c('I-Images', 'R-Image Returns')), # I and R replaced w O and S ~2014, can check for sure if u want
       aes(x=day,y=ma_value/1e6, col=year))+
  #geom_rect(aes(xmin = as.Date('2020-10-01'), xmax = as.Date('2020-10-31'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.05)+
  geom_line(size=0.5)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  facet_wrap(~stream, scales='free',labeller=label_wrap_gen())+
  theme_bw()+
  my_theme()+
  theme(strip.text = element_text(size=10))+
  labs(x='',
       y='CAD (000,000)',
       title='Figure 10: ACSS payment values',
       subtitle='Value of ACSS payments by stream, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  scale_y_continuous(labels=comma)

ggsave("streams_value.png",width=10,height=10,units='in',dpi=300, scale=1.5)


ggplot(acssMA %>% filter(!stream %in% c('I-Images', 'R-Image Returns')), 
       aes(x=day,y=ma_volume/1e3, col=year, fill=year))+
  #geom_rect(aes(xmin = as.Date('2020-10-01'), xmax = as.Date('2020-10-31'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.05)+
  geom_line(size=0.5)+
  #geom_point(size=1,pch=21,col='white')+
  scale_fill_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  facet_wrap(~stream, scales='free', labeller=label_wrap_gen())+
  theme_bw()+
  my_theme()+
  theme(strip.text = element_text(size=10))+
  labs(x='',
       y='Number of transactions (000)',
       title='Figure 9: ACSS payment volumes',
       subtitle='Volume of ACSS payments by stream, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  scale_y_continuous(labels=comma)

ggsave("streams_volume.png",width=10,height=10,units='in',dpi=300, scale=1.5)


### ACSS total value and volume (ie no stream)

# take 5-day moving average
acssTotalsMA <- acssData %>%
  group_by(date) %>%
  summarize(volume = sum(total_volume),
            value = sum(total_value)) %>%
  ungroup() %>%
  mutate(ma_volume = rollmean(volume, k=5, align='right', fill=NA),
         ma_value = rollmean(value, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  mutate(year = format(date, format = '%Y'),
         day = ymd(str_c('2022-',str_sub(as.character(date),6))))

ggplot(acssTotalsMA, aes(x=day,y=ma_value/1e9, col=year, fill=year))+
  #geom_rect(aes(xmin = as.Date('2020-10-01'), xmax = as.Date('2020-10-31'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.05)+
  geom_line(size=1)+
  #geom_point(size=2,pch=21,col='white')+
  scale_fill_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  labs(x='',
       y='CAD (000,000,000)',
       title="ACSS values back to normal, at last years' level",
       subtitle='Total value of ACSS payments, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%B")

ggsave("total_value.png",width=6,height=4,units='in',dpi=300, scale=1)


ggplot(acssTotalsMA, aes(x=day,y=ma_volume, col=year, fill=year))+
  #geom_rect(aes(xmin = as.Date('2020-10-01'), xmax = as.Date('2020-10-31'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.05)+
  geom_line(size=1)+
  #geom_point(size=2,pch=21,col='white')+
  scale_fill_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  labs(x='',
       y='Number of transactions',
       title="ACSS volumes back at last years' level, slightly down in December",
       subtitle='Total volume of ACSS payments, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  scale_y_continuous(labels=comma)

ggsave("total_volume.png",width=6,height=4,units='in',dpi=300, scale=1.5)


## YoY by month
acssMonths <- acssData %>%
  filter(date>='2021-01-01') %>%
  mutate(year = year(date),
         month = month(date),
         quarter = quarters(date)) %>%
  filter(year==acssComparisonFilterBenchmarkYear & quarter==acssComparisonFilterBenchmarkQuarter |
           year==acssComparisonFilterComparisonYear & quarter==acssComparisonFilterComparisonQuarter);
  #filter(year=='2021' & quarter=='Q1' | year=='2020' & quarter=='Q1')# change for quarter, Q2, etc
 
YoYmonth <- acssMonths %>%
 group_by(year, month) %>%
  summarise(volume = sum(total_volume),
            value = sum(total_value)) %>%
  ungroup() %>%
  arrange(year, month) %>%
  mutate(val_change = (value - lag(value,3))/lag(value,3)*100,
         vol_change = (volume - lag(volume,3))/lag(volume,3)*100)


YoYquarter <- acssMonths %>%
  group_by(year, quarter) %>%
  summarise(volume = sum(total_volume),
            value = sum(total_value)) %>% 
  ungroup() %>%
  arrange(year, quarter) %>%
  mutate(val_change = (value - lag(value,1))/lag(value,1)*100,
         vol_change = (volume - lag(volume,1))/lag(volume,1)*100)



##

byMo <- acssData %>%
mutate(month = format(date, format='%b'),
       month2 = factor(month, levels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec'), 
                       ordered=T)) %>% 
  # quickly get dates for multi-year plotting
  group_by(year, month2) %>%
  summarise(`Value (CAD)` = sum(total_value),
            `Number of transactions` = sum(total_volume)) %>%
  ungroup() %>%
  mutate(date = as.Date(paste(month2, '1', year), format='%b %d %Y')) %>%
  filter(date >= '2021-09-01') %>%
  filter(date < '2022-10-01') %>%
  select(-year,-month2) %>%
  gather(type,val,-date)

ggplot(byMo, aes(x=date, y=val/1e6, fill=type))+
  geom_col(alpha=0.75)+
  geom_smooth(method='lm', se=F, col='black',linetype='dashed', size=0.5)+
  facet_wrap(~type, scales='free')+
  theme_bw()+
  my_theme()+
  labs(x=element_blank(),
       y='000,000',
       title='Figure 8: ACSS payment values and volumes by month',
       subtitle='Monthly value and volume of ACSS payments, September 2021 - September 2022')+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=c('#990000', '#1380A1'))+
  scale_x_date(date_breaks="2 month", date_labels="%b %y")

ggsave('acss_monthly.png',width=6.5,height=4,units='in',dpi=300, scale=1.5)



