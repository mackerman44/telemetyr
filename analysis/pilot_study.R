#-----------------------------------------------
# A script for the Lemhi River radio telemetry pilot study.
# Includes reading, cleaning, reduction, analysis, and visualization of data
# and roughly based on the original pilotStudy.R script used to generate the
# original report
#
# Created by Mike Ackerman on 4/8/2020
#-----------------------------------------------

#-------------------------
# load necessary libraries
#-------------------------
library(tidyverse)
library(magrittr)
library(telemetyr)

#-------------------------
# read in pilot study data from NAS
# be sure to be connected to Biomark VPN
#-------------------------
# for Mike
pilot_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018"
# for Kevin
pilot_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018"

<<<<<<< HEAD
# read in the "raw" text files
pilot_raw = read_txt_data(path = pilot_path)

#-------------------------
# start data cleaning
#-------------------------
# fix one receiver (poor coding in file)
pilot_raw %<>%
  mutate(receiver = if_else(receiver == "039",
                            "TT1",
                            receiver))

# clean raw data a little bit
pilot_clean = clean_raw_data(pilot_raw)

=======
#-------------------------
# deal with data that had previously been missing in the pilot study due to errors when resetting receiver
# timers after downloading data
#-------------------------
# for Mike
miss_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"
# for Kevin
miss_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"


# read in csv format data
pilot_csv_df = read_csv_data(path = pilot_path) %>%
  bind_rows(read_csv_data(path = miss_path)) %>%
  arrange(receiver, tag_id, start)
# save as .rda object
save(pilot_csv_df, file = "data/raw/pilot_csv_df.rda")

#-------------------------
# this function reads in the "raw" text files
# pilot_txt_df = read.txt.data(path = pilot_path)
pilot_raw = read_txt_data(path = pilot_path) %>%
  bind_rows(read_txt_data(path = miss_path))
# save as .rda object
pilot_txt_df = pilot_raw %>%
  select(-file_name,
         -file)
save(pilot_txt_df, file = "data/raw/pilot_txt_df.rda")

# fix a couple receiver codes
pilot_raw %<>%
  mutate(receiver = recode(receiver,
                           'BR1' = 'TB1',      # recode BR1 to TB1
                           'BR2' = 'TB2',      # recode BR2 to TB2
                           '039' = 'TT1'))     # recode 039 to TT1
# clean raw data a little bit
pilot_clean = clean_raw_data(pilot_raw)
>>>>>>> 654604b9508625741999f11edfde7fcab5f61b86
# fix tag codes
pilot_round = round_tag_codes(pilot_clean,
                              round_to = 5)

# summarise data to make it more like csv output
pilot_summ = summarise_txt_data(pilot_round)
<<<<<<< HEAD

#-------------------------
# read in pilot study on/off and volt/temp data from NAS
#-------------------------
pilot_on_off_df = read_on_off_data(path = pilot_path)
pilot_volt_temp_df = read_volt_temp_data(path = pilot_path)

#-------------------------
# deal with data that had previously been missing in the pilot study due to errors when resetting receiver
# timers after downloading data
#-------------------------
# for Mike
miss_path = "S:/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"
# for Kevin
miss_path = "~/../../Volumes/ABS/telemetry/lemhi/fixed_site_downloads/2017_2018_missing_A_data"

# operation times
timer_df = parse_timer(pilot_summ)
=======

save(pilot_summ, file = "data/prepped/pilot_summ.rda")
>>>>>>> 654604b9508625741999f11edfde7fcab5f61b86

# receiver names
receiver_nms = c('LH1','LH2',
                 'DC1','DC2',
                 'MB1','MB2',
                 'TR1','TR2',
                 'RR1','RR2',
                 'BG1','BG2',
                 'NF1','NF2',
                 'DW1','DW2',
                 'LR1','LR2',
                 'SR1','SR2',
                 'CC1','CC2',
                 'VC1','VC2',
                 'SB1','SB2',
                 'TB1','TB2')

ops = rcvr_ops_summary(timer_df)

# Calculate the proportion of time that each receiver was operation from the time it first came online to the final time the
# timer tag was observed
p_time_df = op_plot_df %>%
  mutate(site = substr(receiver, 1, 2)) %>%
  left_join(op_plot_df %>%
              filter(operational == T) %>%
              group_by(receiver) %>%
              summarise(end_hr = max(floor_date(hr, unit = 'hours'), na.rm = T)) %>%
              ungroup()) %>%
  filter(hr >= start_hr) %>%
  filter(hr <= end_hr) %>%
  group_by(receiver) %>%
  summarise(p_op = sum(operational == T) / length(operational)) %>%
  ungroup()
p_time_df

# plot operational times for each of the receivers
p_time_p = op_plot_df %>%
  ggplot(aes(x = hr,
             y = fct_rev(receiver),
             color = operational)) +
  geom_line(size = 2,
            color = 'black') +
  geom_point(data = op_plot_df %>%
               filter(!operational),
             size = 1.5, color = 'orange') +
  theme_bw() +
  labs(x = 'Date - Hour',
       y = 'Receiver') +
  theme(axis.text.x = element_text(color = 'black', size = 12),
        axis.text.y = element_text(color = 'black', size = 12),
        axis.title.x = element_text(color = 'black', size = 14),
        axis.title.y = element_text(color = 'black', size = 14))
p_time_p

# save a copy of operations plot
ggsave('figures/operationalPlot.pdf',
       p_time_p,
       width = 16,
       height = 6,
       units = 'in')

