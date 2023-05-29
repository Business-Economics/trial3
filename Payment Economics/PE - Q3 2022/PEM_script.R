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
                      "bdscale", "bizdays", "data.table");

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

# Set working directory to save plots ###
# Change to your wd - will save ALL plots to this folder!
setwd("C:/Users/asmirnova.IO/Documents/Development Workspaces/R-Workspace/PEM/2021_q3");


##Setting the relevant date varriables for the quarter's PE report
### UPDATE THIS EACH PE RELEASE
startDateBQ <- "'2018-12-01'";
endDateBQ <- "'2021-09-30'";

##update these to the first and last days of the quaters being looked at in PE
startEffectiveDateBQ <- "'2021-01-01'";
endEffectiveDateBQ <- "'2021-09-30'";
startCycleDateBQ <- "'2021-01-01'";
endCycleDateBQ <- "'2021-09-30'"; 

##Change these filters to the date range for the quarter being looked at in PE (i.e. first and last days of the quarter)
comparisonFilterStartDate1 <- '2019-01-01'; 
comparisonFilterEndDate1 <- '2019-09-30';

comparisonFilterStartDate2 <- '2020-01-01'; 
comparisonFilterEndDate2 <- '2020-09-30';

comparisonFilterStartDate3 <- '2021-01-01'; 
comparisonFilterEndDate3 <- '2021-09-30';

##This filter is used to select all transactions from the start of the COVID plandemic.
##picking the whole month since some of the impacts of COVID had started prior to the 
##initiation of lockdowns
acssFilterEndDate <- '2020-03-01';

##Change these for the years and quaters being compared in the current PE report 
##(i.e. current year and quarter vs same quarter in previous year)
acssComparisonFilterBenchmarkYear <- '2021';
acssComparisonFilterComparisonYear <- '2020';
acssComparisonFilterBenchmarkQuarter <- 'Q3';
acssComparisonFilterComparisonQuarter <- 'Q3';

###SQL Queries used in code
##This is done upfront to make debugging the code easier
paymentsDataSQL <- str_c("
                         SELECT 
                         EXTRACT(YEAR from cycle_date) AS year,
                         cycle_date AS date,
                         SUM(payment_amt)/1e9 AS lvts_value,
                         payment_tranche AS t
                         FROM `payment_transaction` AS t1
                         WHERE (cycle_date > ", startDateBQ, " AND cycle_date <= ", endDateBQ,")
                         AND cycle_seq_num=(SELECT MAX(cycle_seq_num) FROM `payment_transaction` as t2 where t2.cycle_date = t1.cycle_date)
                         AND status = 'D'
                         GROUP BY cycle_date, t
                         ORDER BY cycle_date, t
                         ");

volume2020rawSQL <- str_c("
                          SELECT 
                          EXTRACT(YEAR from cycle_date) AS year,
                          cycle_date AS date,
                          SUM(t1_volume) AS T1_volume,
                          SUM(t2_volume) AS T2_volume
                          FROM `daily_vol_val_boc1` AS t1
                          WHERE (cycle_date > ", startDateBQ, " AND cycle_date <= ", endDateBQ,")
                          AND cycle_seq_num=(SELECT MAX(cycle_seq_num) FROM `daily_vol_val_boc1` as t2 
                          WHERE t2.cycle_date = t1.cycle_date)
                          GROUP BY cycle_date
                          ORDER BY cycle_date
                          ");

yoyLVTSSQL <- str_c("
                    SELECT
                    EXTRACT(YEAR FROM cycle_date) AS year,
                    EXTRACT(QUARTER FROM cycle_date) AS quarter,
                    SUM(t1_value) AS t1_val,
                    SUM(t2_value) AS t2_val,
                    SUM(t1_volume) AS t1_vol,
                    SUM(t2_volume) AS t2_vol
                    FROM `prod-data-storage-prj.lvts_boc.daily_vol_val_boc1` AS t1
                    WHERE cycle_seq_num=(SELECT MAX(cycle_seq_num) 
                    FROM `prod-data-storage-prj.lvts_boc.daily_vol_val_boc1` as t2 where t2.cycle_date = t1.cycle_date)
                    AND EXTRACT(YEAR FROM cycle_date) in (2020,2021)
                    AND EXTRACT(QUARTER FROM cycle_date) = 1
                    GROUP BY year, quarter
                    ORDER BY year, quarter
                    ");

yoyLVTS_partSQL <- str_c("
                         SELECT
                         EXTRACT(YEAR FROM cycle_date) AS year,
                         EXTRACT(QUARTER FROM cycle_date) AS quarter,
                         CASE 
                         WHEN part_id_from ='BCANCA' THEN 'BoC'
                         ELSE 'Other_LVTS'
                         END AS range_group,
                         SUM(t1_value) AS t1_val,
                         SUM(t2_value) AS t2_val,
                         SUM(t1_volume) AS t1_vol,
                         SUM(t2_volume) AS t2_vol
                         FROM `prod-data-storage-prj.lvts_boc.daily_vol_val_boc1` AS t1
                         WHERE cycle_seq_num=(SELECT MAX(cycle_seq_num) 
                         FROM `prod-data-storage-prj.lvts_boc.daily_vol_val_boc1` AS t2 
                         WHERE t2.cycle_date = t1.cycle_date)
                         AND EXTRACT(YEAR FROM cycle_date) in (2020,2021)
                         AND EXTRACT(QUARTER FROM cycle_date) = 1
                         GROUP BY year, quarter, range_group
                         ORDER BY year, quarter, range_group
                         ");

T1NDCdataSQL <- str_c("
                      SELECT 
                      effective_date AS Date,
                      part_id_grte AS participant,
                      MAX(credit_limit_amt)/1e9 AS t1ndc
                      FROM `mulilateral_credit_limits_t1_boc10` AS t1
                      WHERE (effective_date > ", startEffectiveDateBQ, " AND effective_date <= ", endEffectiveDateBQ,")
                      AND cycle_seq_num=(SELECT MAX(cycle_seq_num) FROM `mulilateral_credit_limits_t1_boc10` as t2 
                      WHERE t2.effective_date = t1.effective_date)
                      AND effective_time = '20:30:00'
                      GROUP BY effective_date, participant 
                      ORDER BY effective_date, participant
                      ");

T2NDCdataSQL <- str_c("
                      SELECT 
                      effective_date AS Date,
                      part_id_grte AS participant,
                      MAX(credit_limit_amt)/1e9 AS t2ndc
                      FROM `mulilateral_credit_limits_t2_boc10` AS t1
                      WHERE (effective_date > ", startEffectiveDateBQ, " AND effective_date <= ", endEffectiveDateBQ,")
                      AND cycle_seq_num=(SELECT MAX(cycle_seq_num) FROM `mulilateral_credit_limits_t2_boc10` as t2 
                      WHERE t2.effective_date = t1.effective_date)
                      AND effective_time = '20:30:00'
                      GROUP BY effective_date, participant 
                      ORDER BY effective_date, participant
                      ");

mnpDataSQL <- str_c("
                    SELECT
                    cycle_date AS date,
                    inst_lvts_id AS participant,
                    SUM(value_received-value_sent) AS net_position
                    FROM `prod-data-storage-prj.high_value_marts.position_fact`
                    WHERE (cycle_date > ", startCycleDateBQ, " AND cycle_date <= ", endCycleDateBQ,")
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

## LVTS value/volume########################################################################################################################################################

# Payments values (in B) by tranche

paymentsData <- dbGetQuery(bQconnection,paymentsDataSQL)

# 5-day moving average by tranche
# filter for months in Q1 and 2019-2021 (last 3 years)
payments <- paymentsData %>%
  group_by(t) %>%
  mutate(ma_value = rollmean(lvts_value, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  ungroup()

# strip just month and day from 'date' and paste 2021 - need same date for x axis in plot (can paste whichever year, will format x-axis %M)
# EXTACT(YEAR from cycle_date) AS year will give us year for grouping in plot
# paste "Tranche" for plotting
payments <- payments %>%
  mutate(day = ymd(str_c('2021-',str_sub(as.character(date),6))),
         t = str_c("Tranche ", as.character(t)))

lvtsValuePlot <- ggplot(payments, aes(x=day,y=ma_value,col=as.factor(year)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+ # can add this back for Q2-Q4 to highlight current Q (doesn't work much for Q1)
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  facet_wrap(~t, scales='free_y',ncol=1)+
  labs(x=element_blank(),
       y='CAD (000,000,000)',
       title='Figure 1: LVTS value remains exceptional in T1, normal in T2',
       subtitle=str_c('Value of LVTS payments by tranche, 5-day moving average',
                      sep = ' '))+
  scale_x_date(date_breaks="1 month", date_labels="%B")

lvtsValuePlot

ggsave("lvts_value_tranche.png",lvtsValuePlot,width=6.5,height=4.5,units='in',dpi=300, scale=1.5)
write.csv(payments,'LVTS_value.csv')

# Payment volumes by tranche
volume2020raw <- dbGetQuery(bQconnection, volume2020rawSQL);

# 5-day moving average by tranche
# filter for months in Q1 2019-2021
volume2020 <- volume2020raw %>%
  mutate(t1_vol_roll = rollmean(T1_volume, k=5, align='right', fill=NA),
         t2_vol_roll = rollmean(T2_volume, k=5, align='right', fill=NA)) %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  ungroup()

# strip just month and day from 'date' and paste 2021 - need same date for x axis in plot
# EXTACT(YEAR from cycle_date) AS year will give us year for grouping in plot
# same as value plot tbh
volume2020 <- volume2020 %>%
  mutate(day = ymd(str_c('2021-',str_sub(as.character(date),6))))

# use gather to create long data for plot
volume2020 <- volume2020 %>%
  select(-T1_volume, -T2_volume) %>%
  gather(tranche, volume, -year, -date, -day)

# create Tranche variable
volume2020 <- volume2020 %>%
  mutate(tranche = if_else(tranche == 't2_vol_roll', 'Tranche 2', 'Tranche 1'))


volumePlot <- ggplot(volume2020, aes(x=day,y=volume,col=as.factor(year)))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+
  geom_line(size=1)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  theme_bw()+
  my_theme()+
  facet_wrap(~tranche, scales='free_y',ncol=1)+
  labs(title='Figure 2: LVTS volumes up slightly year-over-year',
       subtitle='Volume of LVTS payments by tranche, 5-day moving average',
       x='',
       y='Number of transactions')+
  scale_y_continuous(labels=comma_format())+
  scale_x_date(date_breaks="1 month", date_labels="%B")

volumePlot

ggsave("lvts_volume.png",volumePlot,width=6.5,height=4.5,units='in',dpi=300, scale=1.5)

### YoY LVTS value/volume growth by Tranche ###
yoyLVTS <- dbGetQuery(bQconnection, yoyLVTSSQL);

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

# end-of-cycle T2NDC by participant for current Q
# end-of-cycle should match LVTS dashboard
T2NDCdata <- dbGetQuery(bQconnection, T2NDCdataSQL);

# system-wide T2 credit
T2NDC <- T2NDCdata %>%
  group_by(Date) %>%
  summarize(credit = sum(t2ndc)) %>% # system-wide T2NDC
  ungroup() %>%
  mutate(tranche = 'T2')

# end-of-cycle T1NDC by participant
T1NDCdata <- dbGetQuery(bQconnection, T1NDCdataSQL);

# system-wide T1 credit
T1NDC <- T1NDCdata %>%
  group_by(Date) %>%
  summarize(credit = sum(t1ndc)) %>%
  ungroup() %>%
  mutate(tranche = 'T1')

fullCredit <- rbind(T2NDC, T1NDC)

# for some reason, holidays and weekends are gaps when plotting 'fullCredit' as is
# create business day calendar (removes weekends)
# manually remove holidays - couldn't figure a better/faster way to do, sorry!
create.calendar("QuantLib/Canada/TSX", weekdays=c("saturday","sunday"))
tsx <- bizseq(min(fullCredit$Date), max(fullCredit$Date)+1, "QuantLib/Canada/TSX")
tsx2 <- tsx[!tsx %in% c(as.Date("2020-05-18"),as.Date("2020-04-10"),as.Date("2020-07-01"),
                        as.Date("2020-09-07"),as.Date("2020-10-12"),as.Date("2020-11-11"))]

ggplot(fullCredit, aes(x=Date,y=credit,fill=tranche))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), 
  #ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+
  geom_col(width=0.7)+
  theme_bw()+
  my_theme()+
  scale_x_bd(business.dates=tsx2, labels=date_format('%b'), max.major.breaks = 12, expand=c(0.01,0.01))+
  scale_fill_manual(values=c('#1380A1', '#990000'))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(hjust=1),
        axis.title.x=element_blank())+
  scale_y_continuous(expand = expansion(mult = c(0, .05)))+
  labs(y='Credit limit, CAD bn',
       x=element_blank(),
       title='Figure 4: LVTS credit stable throughout Q1',
       subtitle='Intraday credit limits in T1 and T2 for all LVTS participants')

ggsave("t1_t2_credit.png",width=6.5,height=4.5,units='in',dpi=300, scale=1.5)

## Settlement balances ###########################################################################################################################################

# position.fact has eod position data
mnpData <- dbGetQuery(bQconnection, mnpDataSQL);

# aggregated (it is also anonymized if looking to share external)
setBalAgg <- mnpData %>%
  mutate(part = if_else(participant == 'BCANCA', 'Bank of Canada', 'LVTS Participants')) %>%
  group_by(date, part) %>%
  summarise(lvts_balances = sum(net_position))

ggplot(setBalAgg, aes(x=date,y=lvts_balances/1e9, col=part))+
  #geom_rect(aes(xmin = as.Date('2020-11-01'), xmax = as.Date('2020-11-30'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.075)+
  geom_line()+
  theme_bw()+
  my_theme()+
  labs(x=element_blank(),
       y='End-of-day LVTS net position, CAD (000,000,000)',
       title='Figure 3: LVTS settlement balances',
       subtitle='Multilateral net position of all LVTS participants at end-of-day\n(i.e., value received - value sent)')+ 
  # can remove \n (ie new line) if smaller plot - up to you
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  scale_y_continuous(position='right')+
  scale_color_manual(values=c('#1380A1', '#990000'))+
  theme(legend.title=element_blank(),
        axis.text.x=element_text(hjust=0))

ggsave("settlement_bal_agg.png",width=4,height=4.5,units='in',dpi=300, scale=1.25)

## ACSS##################################################################################################################################################################################################################

# pull ACSS stream-level data daily (highest needed)
# system code = 'A' is ACSS // 'U' is USBE data (if you need, dont use in PE typically)
acssData <- dbGetQuery(bQconnection, acssDataSQL);

# 5-day moving average value and volume by stream
acssMA <- acssData %>%
  group_by(stream) %>%
  mutate(ma_volume = rollmean(total_value, k=5, align='right', fill=NA),
         ma_value = rollmean(total_volume, k=5, align='right', fill=NA)) %>%
  ungroup() %>%
  filter(date >= comparisonFilterStartDate1 & date <= comparisonFilterEndDate1 |
           date >= comparisonFilterStartDate2 & date <= comparisonFilterEndDate2 | 
           date >= comparisonFilterStartDate3 & date <= comparisonFilterEndDate3) %>%
  mutate(year = format(date, format = '%Y'),
         stream = if_else(stream=='Q-Point of Service - CR', 'Q-POS - CR',
                          if_else(stream=='P-Point of Service - DR', 'P-POS - DR', stream)), # to fit title on stream-level plots
         day = ymd(str_c('2021-',str_sub(as.character(date),6))))

ggplot(acssMA %>% filter(!stream %in% c('I-Images', 'R-Image Returns')), # I and R replaced w O and S ~2014, can check for sure if u want
       aes(x=day,y=ma_value/1e3, col=year))+
  #geom_rect(aes(xmin = as.Date('2020-10-01'), xmax = as.Date('2020-10-31'), ymin=-Inf, ymax=Inf), col=NA, fill='grey', alpha=0.05)+
  geom_line(size=0.5)+
  scale_color_manual(values=c('#FAAB18', '#1380A1', '#990000'))+
  facet_wrap(~stream, scales='free',labeller=label_wrap_gen())+
  theme_bw()+
  my_theme()+
  theme(strip.text = element_text(size=10))+
  labs(x='',
       y='CAD (000)',
       title='Figure 8: ACSS payment values',
       subtitle='Value of ACSS payments by stream, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  scale_y_continuous(labels=comma)

ggsave("streams_value.png",width=7.5,height=7.5,units='in',dpi=300, scale=1.4)


ggplot(acssMA %>% filter(!stream %in% c('I-Images', 'R-Image Returns')), 
       aes(x=day,y=ma_volume/1e6, col=year, fill=year))+
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
       y='Number of transactions (000,000)',
       title='Figure 7: ACSS payment volumes',
       subtitle='Volume of ACSS payments by stream, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%b")+
  scale_y_continuous(labels=comma)

ggsave("streams_volume.png",width=7.5,height=7.5,units='in',dpi=300, scale=1.4)


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
         day = ymd(str_c('2021-',str_sub(as.character(date),6))))

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

ggsave("total_value.png",width=6,height=4,units='in',dpi=300, scale=1.5)


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
       title='ACSS volumes take slight hit, down 10% from October 2019',
       subtitle='Total volume of ACSS payments, 5-day moving average')+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  scale_y_continuous(labels=comma)

ggsave("total_volume.png",width=6,height=4,units='in',dpi=300, scale=1.5)


## YoY by month
acssMonths <- acssData %>%
  filter(date>='2020-01-01') %>%
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
  filter(date >= as.Date(parse_date_time(acssFilterEndDate, c('ymd', 'ymd_HMS')))) %>%
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
       title='Figure 5: ACSS payment values and volumes by month',
       subtitle='Monthly value and volume of ACSS payments, March 2020 - March 2021')+
  scale_y_continuous(labels=comma)+
  scale_fill_manual(values=c('#990000', '#1380A1'))

ggsave('acss_monthly.png',width=6.5,height=4,units='in',dpi=300, scale=1.5)
