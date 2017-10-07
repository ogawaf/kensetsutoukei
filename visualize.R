library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(jpndistrict)
library(sf)
library(sp)
library(ggplot2)
library(mapview)
library(openxlsx)
options(stringsAsFactors = F)



jpn <- 
  raster::getData(country="JPN", level=1) %>% 
  st_as_sf() %>%
  st_transform(4612)

prefs <- read_csv("pref.csv", col_types=cols())

const_stat <- read.xlsx(startRow = 11, sheet = "sougchiiki ",xlsxFile = "建設総合統計_出来高.xlsx")

const_stat <-
  const_stat %>% mutate(観測年月 =as.Date(観測年月, origin = "1899-12-30"))

# 都道府県別の出来高推移
const_stat %>% 
  gather(key = area, value = yield, -`観測年月`) %>%  
  filter(area %in% prefs$pref_name) %>% 
  mutate(yield=as.numeric(yield)) %>% 
  ggplot() +
  geom_path(aes(x = 観測年月, y = yield)) +
  facet_wrap(~area) +
  theme(axis.text.x = element_text(angle=30))

# 月別の出来高増加率の推移
const_stat %>% 
  gather(key = area, value = yield, -`観測年月`) %>%  
  filter(area %in% prefs$pref_name) %>% 
  mutate(yield=as.numeric(yield)) %>% 
  group_by(area) %>% 
  mutate(yield_pre = lag(yield), 
         yield_increase_rate = yield / yield_pre) %>% 
  ggplot() +
  geom_path(aes(x = 観測年月, y = yield_increase_rate)) +
  facet_wrap(~area) +
  theme(axis.text.x = element_text(angle=30)) +
  labs(y = "出来高増加率（%）", title="月別の出来高増加率の推移")
ggsave("月別の出来高増加率の推移.png")

# 都道府県別の毎年12月の出来高増加率の推移
const_stat %>% 
  gather(key = area, value = yield, -`観測年月`) %>%  
  filter(area %in% prefs$pref_name) %>% 
  mutate(yield=as.numeric(yield), month_no = lubridate::month(観測年月)) %>%
  filter(month_no==12) %>% 
  group_by(area) %>% 
  mutate(yield_pre = lag(yield), 
         yield_increase_rate = yield / yield_pre) %>% 
  ggplot() +
  geom_path(aes(x = 観測年月, y = yield_increase_rate, group=area)) +
  geom_hline(yintercept = 1, linetype=2, color="darkgrey") +
  facet_wrap(~area) +
  theme(axis.text.x = element_text(angle=30)) +
  labs(y="出来高増加率", title="都道府県別の毎年12月の出来高増加率の推移")
ggsave("都道府県別の毎年12月の出来高増加率の推移.png")


const_stat_geo <- 
  const_stat %>% 
  gather(key = area, value = yield, -`観測年月`) %>%  
  filter(!is.na(観測年月)) %>% 
  filter(area %in% prefs$pref_name) %>% 
  mutate(yield=as.numeric(yield)) %>% 
  left_join(as.data.frame(jpn %>% select(NL_NAME_1)), by=c("area"="NL_NAME_1")) %>% 
  st_as_sf(crs=4612)

# 4月の建設統計（日本地図表示）
const_stat_geo %>% 
  mutate(month_no=lubridate::month(観測年月)) %>% 
  filter(month_no == 4) %>% 
  ggplot() +
  geom_sf(aes(fill=yield), color=NA) +
  facet_wrap(~観測年月) +
  scale_fill_viridis_c(trans="log")

const_stat_geo %>% 
  mutate(month_no=lubridate::month(観測年月)) %>% 
  filter(month_no == 4) %>% 
  ggplot() +
  geom_path(aes(x=観測年月, y=yield, group=area)) +
  facet_wrap(~area) +
  theme(axis.text.x = element_text(angle=30)) +
  labs(y="出来高（億円）")
ggsave("4月の各都道府県の出来高.png")


const_stat_geo %>% 
  mutate(month_no=lubridate::month(観測年月)) %>% 
  ggplot() +
  geom_path(aes(x=観測年月, y=yield, color=area)) +
  theme(axis.text.x = element_text(angle=30)) +
  labs(y="出来高（億円）")
ggsave("都道府県別の出来高推移.png")

const_stat_geo %>% 
  mutate(month_no=lubridate::month(観測年月)) %>% 
  filter(area!="東京都") %>% 
  filter(area %in% c("岩手県","宮城県","福島県")) %>% 
  ggplot() +
  geom_path(aes(x=観測年月, y=yield, color=area), size=2) +
  theme(axis.text.x = element_text(angle=30)) +
  labs(y="出来高（億円）")
ggsave("東北三県における出来高の推移.png")

const_stat_geo %>% 
  mutate(month_no=lubridate::month(観測年月)) %>% 
  filter(area!="東京都") %>% 
  filter(area %in% c("岩手県","宮城県","福島県")) %>% 
  as.data.frame() %>% 
  group_by(観測年月) %>%  
  mutate(tohoku_base=sum(yield), 
         yield_rate_tohoku_base = (yield / tohoku_base) %>% round(2)) %>% 
  ggplot() +
  #geom_bar(aes(x=観測年月, y=yield, fill=area), stat="identity", position="fill") 
  geom_bar(aes(x=観測年月, y=yield_rate_tohoku_base), stat="identity") +
  facet_wrap(~area) +
  geom_vline(xintercept = as.Date("2011-04-01"), color="blue", size=2)+
  theme(axis.text.x = element_text(angle=30)) +
  labs(y="東北3県における出来高割合（%）")
  ggsave("東北三県における出来高割合の推移.png")

