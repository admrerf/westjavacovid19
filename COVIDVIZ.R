library(tidyverse)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(lubridate)

#download data
covid <- fromJSON("https://covid19-public.digitalservice.id/api/v1/sebaran_v2/jabar")

#cek isinya apa aja
str(covid)

covid_df <- data.frame(covid$data$content)
head(covid_df)
tail(covid_df)

#mulai bersihin dari nama kabupaten dulu
unique(covid_df$nama_kab)

covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Bogor", "KAB. BOGOR", "KABUPATEN BOGOR")] <- "Kabupaten Bogor"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Bandung", "KAB. BANDUNG", "KABUPATEN BANDUNG")] <- "Kabupaten Bandung"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Tasikmalaya", "KAB. TASIKMALAYA", "KABUPATEN TASIKMALAYA")] <- "Kabupaten Tasikmalaya"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Ciamis", "KAB. CIAMIS", "KABUPATEN CIAMIS")] <- "Kabupaten Ciamis"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Kuningan", "KAB. KUNINGAN", "KABUPATEN KUNINGAN")] <- "Kabupaten Kuningan"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Sumedang", "KAB. SUMEDANG", "KABUPATEN SUMEDANG")] <- "Kabupaten Sumedang"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Cirebon", "KAB. CIREBON", "KABUPATEN CIREBON")] <- "Kabupaten Cirebon"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Indramayu", "KAB. INDRAMAYU", "KABUPATEN INDRAMAYU")] <- "Kabupaten Indramayu"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Purwakarta", "KAB. PURWAKARTA", "KABUPATEN PURWAKARTA")] <- "Kabupaten Purwakarta"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Karawang", "KAB. KARAWANG", "KABUPATEN KARAWANG")] <- "Kabupaten Karawang"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Kuningan", "KAB. KUNINGAN", "KABUPATEN KUNINGAN")] <- "Kabupaten Kuningan"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Sukabumi", "KAB. SUKABUMI", "KABUPATEN SUKABUMI")] <- "Kabupaten Sukabumi"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Garut", "KAB. GARUT", "KABUPATEN GARUT")] <- "Kabupaten Garut"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Cianjur", "KAB. CIANJUR", "KABUPATEN CIANJUR")] <- "Kabupaten Cianjur"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Majalengka", "KAB. MAJALENGKA", "KABUPATEN MAJALENGKA")] <- "Kabupaten Majalengka"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Subang", "KAB. SUBANG", "KABUPATEN SUBANG")] <- "Kabupaten Subang"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Bandung Barat", "KAB. BANDUNG BARAT", "KABUPATEN BANDUNG BARAT")] <- "Kabupaten Bandung Barat"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Bekasi", "KAB. BEKASI", "KABUPATEN BEKASI")] <- "Kabupaten Bekasi"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kabupaten Pangandaran", "KAB. PANGANDARAN", "KABUPATEN PANGANDARAN")] <- "Kabupaten Pangandaran"

#kota
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Bogor", "KOTA BOGOR")] <- "Kota Bogor"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Bandung", "KOTA BANDUNG")] <- "Kota Bandung"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Depok", "KOTA DEPOK")] <- "Kota Depok"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Cirebon", "KOTA CIREBON")] <- "Kota Cirebon"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Sukabumi", "KOTA SUKABUMI")] <- "Kota Sukabumi"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Bekasi", "KOTA BEKASI")] <- "Kota Bekasi"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Tasikmalaya", "KOTA TASIKMALAYA")] <- "Kota Tasikmalaya"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Cimahi", "KOTA CIMAHI", "CITEUREUP KOTA CIMAHI")] <- "Kota Cimahi"
covid_df$nama_kab[covid_df$nama_kab %in% c("Kota Banjar", "KOTA BANJAR")] <- "Kota Banjar"

unique(covid_df$tanggal_konfirmasi)
#changing date type from character to date
covid_df$tanggal_konfirmasi <- ymd(covid_df$tanggal_konfirmasi)
#, !is.na(nama_kab), nama_kab != "Jawa Barat"
#summing covid cases based on month and year
#covid_df1 <- covid_df %>% filter(!is.na(tanggal_konfirmasi)) %>% 
#  mutate(bln_konfirm = format(tanggal_konfirmasi, "%b"), thn_konfirm = format(tanggal_konfirmasi, "%Y")) %>% 
#  group_by(thn_konfirm, bln_konfirm) %>% 
#  summarize(total_konfirm = n()) %>% arrange(bln_konfirm) %>% arrange(thn_konfirm)


covid_df1 <- covid_df %>% filter(!is.na(tanggal_konfirmasi)) %>% 
  group_by(tanggal_konfirmasi) %>% 
  mutate(thn_bln_konfirm = month(tanggal_konfirmasi), thn_konfirm = year(tanggal_konfirmasi)) %>%
  summarize(total_konfirm = n()) %>%
  arrange(tanggal_konfirmasi) %>%
  ungroup()


ggplot(covid_df1, aes(x = tanggal_konfirmasi, y = total_konfirm)) + 
  geom_line() +
  xlab("Bulan") + ylab("Jumlah kasus terkonfirmasi") + 
  labs(title = "Jumlah Kasus Terkonfirmasi Per Bulan (September 2020 - April 2021)") + facet_wrap(covid_df1$thn_konfirm)
