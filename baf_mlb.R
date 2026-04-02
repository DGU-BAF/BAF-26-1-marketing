rm(list=ls())
library(readxl)

throw21 <- read_excel("C:/Users/justi/Desktop/baf_project_mlb/mlb_throw_21.xlsx")
throw22 <- read_excel("C:/Users/justi/Desktop/baf_project_mlb/mlb_throw_22.xlsx")
throw23 <- read_excel("C:/Users/justi/Desktop/baf_project_mlb/mlb_throw_23.xlsx")
throw24 <- read_excel("C:/Users/justi/Desktop/baf_project_mlb/mlb_throw_24.xlsx")
throw25 <- read_excel("C:/Users/justi/Desktop/baf_project_mlb/mlb_throw_25.xlsx")

str(throw21)
View(throw21[1:10,])

library(dplyr)
#시즌 추가
throw21 <- throw21 %>% mutate(season = 2021)
throw22 <- throw22 %>% mutate(season = 2022)
throw23 <- throw23 %>% mutate(season = 2023)
throw24 <- throw24 %>% mutate(season = 2024)
throw25 <- throw25 %>% mutate(season = 2025)

#모든 칼럼 동일 확인
identical(names(throw21), names(throw25))
identical(names(throw21), names(throw24))
identical(names(throw21), names(throw23))
identical(names(throw21), names(throw22))

#합치기 
pitch_all <- bind_rows(throw21, throw22, throw23, throw24, throw25)

# Pos 변수 결측치 확인
unique(pitch_all$Pos)
sum(is.na(pitch_all$Pos))

# 같은 행 반복 확인
sum(duplicated(pitch_all))

#선수명 정리
library(stringr)

pitch_all <- pitch_all %>%
  mutate(player_clean = str_remove_all(Player, "[\\*#]"))

# IP 변수 이닝 변수 변환
convert_ip <- function(x) {
  whole <- floor(x)
  frac <- round(x - whole, 1)
  add <- case_when(
    frac == 0.0 ~ 0,
    frac == 0.1 ~ 1/3,
    frac == 0.2 ~ 2/3,
    TRUE ~ NA_real_
  )
  whole + add
}
pitch_all <- pitch_all %>%
  mutate(IP_true = convert_ip(IP))

sum(is.na(pitch_all$IP_true))

# FIP는 ERA와 같은 스케일로 해석되도록 만든 지표(홈런, 볼넷, 삼진)
# ex) ERA - FIP > 0 투수 본인 퍼포먼스는 나쁘지 않을 수도 있다.
# ERA_FIG_diff 파생변수 생성

pitch_all <- pitch_all %>%
  mutate(
    ERA_FIP_diff = ERA - FIP        
  )

str(pitch_all)

# RK 변수 제거, 승률 데이터 W_L 변경
pitch_eda = pitch_all %>% 
  select(-Rk)

pitch_eda = pitch_eda %>% 
  rename(W_L = 'W-L%')

# Team Totals 분리
pitch_team <- pitch_eda %>%
  filter(Player == "Team Totals")

pitch_player <- pitch_eda %>%
  filter(Player != "Team Totals")

nrow(pitch_player)
nrow(pitch_team)
View(pitch_team)

str(pitch_eda)

library(ggplot2)
library(scales)

str(pitch_team)

#시즌별 투수 수
pitch_player %>% 
  count(season) %>% 
  ggplot(aes(x= factor(season), y= n, group=1)) +
  geom_line()+
  geom_point(size=3)+
  labs(title = '시즌별 투수 수', x = '시즌',y = '투수 수')

# 시즌별 승률
ggplot(pitch_team, aes(x = factor(season), y = W_L, group = 1)) +
  geom_line() +
  geom_point(size = 3)+
  labs(title = '시즌별 팀 승률', x = '시즌', y= '승률')

# 시즌별 팀 WAR
ggplot(pitch_team, aes(x = factor(season), y = WAR, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "시즌별 팀 투수 WAR", x = "시즌", y = "WAR")

# 시즌별 팀 ERA
ggplot(pitch_team, aes(x = factor(season), y = ERA, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "시즌별 팀 ERA", x = "시즌", y = "ERA")

# 시즌별 팀 FIP
ggplot(pitch_team, aes(x = factor(season), y = FIP, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "시즌별 팀 FIP", x = "시즌", y = "FIP")

# 시즌별 팀 WHIP
ggplot(pitch_team, aes(x = factor(season), y = WHIP, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "시즌별 팀 WHIP", x = "시즌", y = "WHIP")

# 시즌별 팀 총 이닝
ggplot(pitch_team, aes(x = factor(season), y = IP_true, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "시즌별 팀 총 이닝", x = "시즌", y = "IP_true")

# WAR 히스토그램
ggplot(pitch_player, aes(x = WAR)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "skyblue") +
  facet_wrap(~season) +
  labs(title = "시즌별 선수 WAR 분포", x = "WAR", y = "빈도")

ggplot(pitch_player, aes(x = factor(season), y = WAR)) +
  geom_boxplot(fill = "skyblue")

# 시즌별 WAR 상위 5명
pitch_player %>%
  group_by(season) %>%
  slice_max(order_by = WAR, n = 5, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(player_clean, WAR), y = WAR)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~season, scales = "free_y") +
  labs(title = "시즌별 WAR 상위 5명", x = "선수", y = "WAR")

# 시즌별 이닝 상위 5명
pitch_player %>%
  group_by(season) %>%
  slice_max(order_by = IP_true, n = 5, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(player_clean, IP_true), y = IP_true)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~season, scales = "free_y") +
  labs(title = "시즌별 이닝 상위 5명", x = "선수", y = "IP_true")
