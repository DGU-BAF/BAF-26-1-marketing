team_ip_coverage <- pitcher_cluster_all %>%
  group_by(season, team, role) %>%
  summarise(clustered_ip = sum(ip, na.rm = TRUE), .groups = "drop")
team_total_ip_raw <- pitcher_role %>%
  mutate(team = if_else(team == "ATH", "OAK", team)) %>%
  filter(team %in% mlb_teams) %>%
  group_by(season, team, role) %>%
  summarise(total_ip_raw = sum(ip, na.rm = TRUE), .groups = "drop")
team_ip_coverage_check <- team_ip_coverage %>%
  left_join(team_total_ip_raw, by = c("season", "team", "role")) %>%
  mutate(coverage = clustered_ip / total_ip_raw)
View(team_ip_coverage_check)

summary(team_ip_coverage_check$coverage)
plot(team_ip_coverage_check$coverage)
team_ip_coverage_check[team_ip_coverage_check$coverage <= 0.5666,]

View(head(pitcher_cluster_all))

rm(list=ls())

setwd("C:/Users/justi/Desktop/baf_project_mlb_0515")
getwd()
list.files()

library(dplyr)
library(readr)
library(purrr)
library(stringr)

#전처리----
## 1. 선수 이름 정리 함수 -----------------------------------------

clean_bref_name <- function(x) {
  x |>
    str_remove_all("[*#]") |>   # Baseball-Reference의 명예의전당/현역 표시 제거용
    str_squish()
}

convert_savant_name <- function(x) {
  # "Wheeler, Zack" -> "Zack Wheeler"
  if_else(
    str_detect(x, ","),
    str_replace(x, "^([^,]+),\\s*(.+)$", "\\2 \\1"),
    x
  ) |>
    str_squish()
}


## 2. 연도별 ref + savant merge 함수 -------------------------------

read_merge_pitching_one_year <- function(year) {
  
  ref_file <- paste0(year, "_ref_pitch.csv")
  savant_file <- paste0(year, "_pitch_savent.csv")
  
  ref <- read_csv(ref_file, show_col_types = FALSE) |>
    filter(Rk != "Rk") |>       # 중간 반복 헤더 제거
    mutate(
      Season = year,
      Player_clean = clean_bref_name(Player)
    )
  
  savant <- read_csv(savant_file, show_col_types = FALSE) |>
    mutate(
      Season = year,
      Player_clean = convert_savant_name(player_name)
    )
  
  # Baseball-Reference에는 트레이드 선수의 팀별 행 + TOT 행이 있을 수 있음
  # Savant는 보통 선수-시즌 단위라서 ref도 선수-시즌 1행으로 맞추는 게 안전함
  ref_total <- ref |>
    group_by(Season, Player_clean) |>
    arrange(desc(Team == "TOT"), .by_group = TRUE) |>
    slice(1) |>
    ungroup()
  
  merged <- ref_total |>
    left_join(
      savant,
      by = c("Season", "Player_clean"),
      suffix = c("_ref", "_savant")
    )
  
  return(merged)
}


## 3. 2021~2025 전체 merge -----------------------------------------

pitcher_merged_21_25 <- map_dfr(2021:2025, read_merge_pitching_one_year)


## 4. 확인 ----------------------------------------------------------

glimpse(pitcher_merged_21_25)

pitcher_merged_21_25 |>
  count(Season)

pitcher_merged_21_25 |>
  summarise(
    n_total = n(),
    n_matched_savant = sum(!is.na(player_id)),
    n_unmatched_savant = sum(is.na(player_id))
  )

View(head(pitcher_merged_21_25))

# 클러스터링----
library(tidyverse)
library(janitor)
library(cluster)
library(corrplot)
library(car)

pitcher_raw <- pitcher_merged_21_25 %>%
  clean_names()

##선발/ 볼펜 기준 만들기----
pitcher_role <- pitcher_raw %>%
  mutate(
    starter_ratio = gs / g,
    ip_per_game = ip / g,
    
    role = case_when(
      gs >= 5 & starter_ratio >= 0.5 ~ "starter",
      TRUE ~ "reliever"
    )
  )

##최소표본 기준----
pitcher_role_filtered <- pitcher_role %>%
  filter(
    case_when(
      role == "starter" ~ ip >= 30,
      role == "reliever" ~ ip >= 10,
      TRUE ~ FALSE
    )
  )

##클러스터링 변수----
cluster_vars <- c(
  "xobp",
  "xslg",
  "xba",
  "bb_percent",
  "k_percent",
  "swing_miss_percent",
  "hardhit_percent",
  "barrels_per_bbe_percent"
)


##선발/볼펜 분리----
starter_data <- pitcher_role_filtered %>%
  filter(role == "starter") %>%
  drop_na(all_of(cluster_vars))

reliever_data <- pitcher_role_filtered %>%
  filter(role == "reliever") %>%
  drop_na(all_of(cluster_vars))

starter_data %>% count(season)
reliever_data %>% count(season)

nrow(starter_data)
nrow(reliever_data)

##상관행렬 확인----
check_correlation <- function(data, cluster_vars, title = "Correlation Matrix") {
  
  cor_data <- data %>%
    select(all_of(cluster_vars)) %>%
    drop_na()
  
  cor_mat <- cor(cor_data, use = "pairwise.complete.obs")
  
  print(round(cor_mat, 2))
  
  corrplot(
    cor_mat,
    method = "color",
    type = "upper",
    order = "hclust",
    tl.cex = 0.8,
    tl.col = "black",
    addCoef.col = "black",
    number.cex = 0.7,
    title = title,
    mar = c(0, 0, 2, 0)
  )
  
  return(cor_mat)
}
#선발
starter_cor <- check_correlation(
  starter_data,
  cluster_vars,
  title = "Starter Pitcher Correlation Matrix"
)
#볼펜
reliever_cor <- check_correlation(
  reliever_data,
  cluster_vars,
  title = "Reliever Pitcher Correlation Matrix"
)

##vif 확인----
check_vif_all <- function(data, cluster_vars) {
  vif_data <- data %>%
    select(all_of(cluster_vars)) %>%
    drop_na()
  
  map_dfr(cluster_vars, function(v) {
    others <- setdiff(cluster_vars, v)
    fit <- lm(
      as.formula(paste(v, "~", paste(others, collapse = " + "))),
      data = vif_data
    )
    r2 <- summary(fit)$r.squared
    tibble(
      variable = v,
      vif = 1 / (1 - r2)
    )
  }) %>%
    arrange(desc(vif))
}
#선발
starter_vif <- check_vif_all(starter_data, cluster_vars)
starter_vif
#불펜
reliever_vif <- check_vif_all(reliever_data, cluster_vars)
reliever_vif

##pca 사용 개수 근거----
run_pca_check <- function(data, cluster_vars) {
  
  pca_data <- data %>%
    select(all_of(cluster_vars)) %>%
    drop_na()
  
  pca_fit <- prcomp(
    pca_data,
    center = TRUE,
    scale. = TRUE
  )
  
  pca_var <- tibble(
    PC = paste0("PC", seq_along(pca_fit$sdev)),
    eigenvalue = pca_fit$sdev^2,
    prop_var = eigenvalue / sum(eigenvalue),
    cum_var = cumsum(prop_var)
  )
  
  return(
    list(
      pca = pca_fit,
      pca_var = pca_var
    )
  )
}

starter_pca_check <- run_pca_check(starter_data, cluster_vars)

starter_pca_check$pca_var

reliever_pca_check <- run_pca_check(reliever_data, cluster_vars)

reliever_pca_check$pca_var

##설명 분산 그래프----
plot_pca_variance <- function(pca_var, title = "PCA Explained Variance") {
  
  ggplot(pca_var, aes(x = seq_along(PC), y = cum_var)) +
    geom_line() +
    geom_point(size = 2) +
    geom_hline(yintercept = 0.8, linetype = "dashed") +
    geom_hline(yintercept = 0.9, linetype = "dashed") +
    scale_x_continuous(
      breaks = seq_along(pca_var$PC),
      labels = pca_var$PC
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = title,
      x = "Principal Components",
      y = "Cumulative Explained Variance"
    ) +
    theme_minimal()
}
#선발
plot_pca_variance(
  starter_pca_check$pca_var,
)
#불펜
plot_pca_variance(
  reliever_pca_check$pca_var,
  title = "Reliever Pitcher PCA Cumulative Explained Variance"
)
##scree plot----
plot_scree <- function(pca_var, title = "Scree Plot") {
  
  ggplot(pca_var, aes(x = seq_along(PC), y = prop_var)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(
      breaks = seq_along(pca_var$PC),
      labels = pca_var$PC
    ) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = title,
      x = "Principal Components",
      y = "Proportion of Variance Explained"
    ) +
    theme_minimal()
}
plot_scree(
  starter_pca_check$pca_var,
  title = "Starter Pitcher Scree Plot"
)
plot_scree(
  reliever_pca_check$pca_var,
  title = "Reliever Pitcher Scree Plot"
)
##k-means k 개수 결정----
check_kmeans_k <- function(data, cluster_vars, n_pcs = 3, k_max = 8, seed = 123) {
  
  x_raw <- data %>%
    select(all_of(cluster_vars)) %>%
    drop_na()
  
  x_scaled <- scale(x_raw)
  
  pca_fit <- prcomp(
    x_scaled,
    center = FALSE,
    scale. = FALSE
  )
  
  pc_data <- as.data.frame(pca_fit$x) %>%
    select(1:n_pcs)
  
  names(pc_data) <- paste0("PC", 1:n_pcs)
  
  set.seed(seed)
  
  wss <- map_dbl(1:k_max, function(k) {
    kmeans(
      pc_data,
      centers = k,
      nstart = 50
    )$tot.withinss
  })
  
  set.seed(seed)
  
  between_ratio <- map_dbl(1:k_max, function(k) {
    km <- kmeans(
      pc_data,
      centers = k,
      nstart = 50
    )
    
    km$betweenss / km$totss
  })
  
  set.seed(seed)
  
  silhouette_scores <- map_dbl(2:k_max, function(k) {
    km <- kmeans(
      pc_data,
      centers = k,
      nstart = 50
    )
    
    ss <- silhouette(
      km$cluster,
      dist(pc_data)
    )
    
    mean(ss[, 3])
  })
  
  k_summary <- tibble(
    k = 1:k_max,
    wss = wss,
    between_ratio = between_ratio,
    silhouette = c(NA, silhouette_scores)
  )
  
  return(
    list(
      pca = pca_fit,
      pc_data = pc_data,
      k_summary = k_summary
    )
  )
}
#선발
starter_k_check <- check_kmeans_k(
  data = starter_data,
  cluster_vars = cluster_vars,
  n_pcs = 3,
  k_max = 8,
  seed = 123
)

starter_k_check$k_summary
#불펜
reliever_k_check <- check_kmeans_k(
  data = reliever_data,
  cluster_vars = cluster_vars,
  n_pcs = 3,
  k_max = 8,
  seed = 123
)
reliever_k_check$k_summary
##elbow plot----
plot_elbow <- function(k_summary, title = "Elbow Method") {
  
  ggplot(k_summary, aes(x = k, y = wss)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(breaks = k_summary$k) +
    labs(
      title = title,
      x = "Number of clusters",
      y = "Total within-cluster sum of squares"
    ) +
    theme_minimal()
}
plot_elbow(
  starter_k_check$k_summary,
  title = "Starter Pitcher Elbow Method"
)
plot_elbow(
  reliever_k_check$k_summary,
  title = "Reliever Pitcher Elbow Method"
)

##silhouette plot----
plot_silhouette <- function(k_summary, title = "Average Silhouette Width") {
  
  ggplot(k_summary %>% filter(!is.na(silhouette)),
         aes(x = k, y = silhouette)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(breaks = k_summary$k) +
    labs(
      title = title,
      x = "Number of clusters",
      y = "Average silhouette width"
    ) +
    theme_minimal()
}
plot_silhouette(
  starter_k_check$k_summary,
  title = "Starter Pitcher Average Silhouette Width"
)
plot_silhouette(
  reliever_k_check$k_summary,
  title = "Reliever Pitcher Average Silhouette Width"
)
##betweenss ratio plot----
plot_between_ratio <- function(k_summary, title = "Between-cluster SS Ratio") {
  
  ggplot(k_summary, aes(x = k, y = between_ratio)) +
    geom_line() +
    geom_point(size = 2) +
    scale_x_continuous(breaks = k_summary$k) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
      title = title,
      x = "Number of clusters",
      y = "Between SS / Total SS"
    ) +
    theme_minimal()
}
#선발
plot_between_ratio(
  starter_k_check$k_summary,
  title = "Starter Pitcher Between-cluster SS Ratio"
)
#불펜
plot_between_ratio(
  reliever_k_check$k_summary,
  title = "Reliever Pitcher Between-cluster SS Ratio"
)
#k-means----
run_pitcher_kmeans <- function(data, cluster_vars, k = 4, seed = 123) {
  
  # 1. 클러스터링 변수만 선택
  x_raw <- data %>%
    select(all_of(cluster_vars))
  
  # 2. 표준화
  x_scaled <- scale(x_raw)
  
  # 3. PCA
  pca_fit <- prcomp(
    x_scaled,
    center = FALSE,
    scale. = FALSE
  )
  
  # 4. PC1~PC3 사용
  pca_scores <- as.data.frame(pca_fit$x) %>%
    select(PC1, PC2, PC3)
  
  # 5. K-means
  set.seed(seed)
  
  km_fit <- kmeans(
    pca_scores,
    centers = k,
    nstart = 50
  )
  
  # 6. 원자료에 클러스터 번호 붙이기
  result <- data %>%
    bind_cols(pca_scores) %>%
    mutate(
      cluster = factor(km_fit$cluster)
    )
  
  return(
    list(
      data = result,
      pca = pca_fit,
      kmeans = km_fit,
      pca_scores = pca_scores
    )
  )
}

##선발투수 ----
starter_kmeans <- run_pitcher_kmeans(
  data = starter_data,
  cluster_vars = cluster_vars,
  k = 4,
  seed = 123
)

starter_cluster_result <- starter_kmeans$data

starter_cluster_result %>%
  count(cluster)

starter_kmeans$kmeans$size
starter_kmeans$kmeans$centers
starter_kmeans$kmeans$tot.withinss
starter_kmeans$kmeans$betweenss / starter_kmeans$kmeans$totss

##불펜투수 ----
reliever_kmeans <- run_pitcher_kmeans(
  data = reliever_data,
  cluster_vars = cluster_vars,
  k = 4,
  seed = 123
)

reliever_cluster_result <- reliever_kmeans$data

reliever_cluster_result %>%
  count(cluster)

reliever_kmeans$kmeans$size
reliever_kmeans$kmeans$centers
reliever_kmeans$kmeans$tot.withinss
reliever_kmeans$kmeans$betweenss / reliever_kmeans$kmeans$totss

##원변수 평균 확인----
#선발
starter_cluster_summary <- starter_cluster_result %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(
      all_of(cluster_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    war_mean = mean(war, na.rm = TRUE),
    fip_mean = mean(fip, na.rm = TRUE),
    whip_mean = mean(whip, na.rm = TRUE),
    ip_mean = mean(ip, na.rm = TRUE),
    .groups = "drop"
  )

View(starter_cluster_summary)
# 1C 에이스형 압도적 고성능
#낮은 피출루·피장타 지표와 높은 탈삼진 능력을 동시에 보이는 고성능 선발 유형이다.
#WAR, FIP, WHIP 기준에서도 가장 우수하여 팀 선발진의 핵심 전력으로 해석된다.

# 2C 파워는 부족 생산성
#비교적 안정적인 출루 억제와 탈삼진 능력을 보이는 실전형 선발 유형이다. 
#WAR와 FIP를 고려할 때 평균 이상의 선발 자원으로 해석할 수 있다.

# C3는 높은 피출루·피장타 허용과 낮은 탈삼진 능력을 보이는 저성과 선발 유형이다. 
#WAR가 음수이고 FIP와 WHIP가 가장 높아, 팀 성과에 부정적인 영향을 줄 가능성이 큰 클러스터로 해석된다.

#선발 C4는 탈삼진 능력은 낮지만, C3에 비해 장타와 강한 타구 허용을 억제하는 유형이다. 
#에이스형은 아니지만 일정 수준의 이닝을 소화할 수 있는 중간급 선발 자원으로 해석된다.

# 볼펜
reliever_cluster_summary <- reliever_cluster_result %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(
      all_of(cluster_vars),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}_mean"
    ),
    war_mean = mean(war, na.rm = TRUE),
    fip_mean = mean(fip, na.rm = TRUE),
    whip_mean = mean(whip, na.rm = TRUE),
    ip_mean = mean(ip, na.rm = TRUE),
    .groups = "drop"
  )

View(reliever_cluster_summary)
#C1은 헛스윙 능력은 일정 수준 보유하고 있으나, 높은 볼넷률과 출루 허용으로 인해 안정성이 떨어지는 유형이다. 
#장타 억제는 가능하지만 제구 불안이 실점 위험을 높이는 클러스터로 해석된다.

#C2는 높은 탈삼진 능력과 낮은 피출루·피장타 허용을 동시에 보이는 최상위 불펜 유형이다. 
#WAR, FIP, WHIP 기준에서도 가장 우수하여 필승조 또는 핵심 불펜 자원으로 해석된다.

#C3는 탈삼진 능력은 제한적이지만, 낮은 볼넷률을 바탕으로 출루를 관리하는 유형이다. 
#압도적인 필승조보다는 안정적인 중간계투 또는 관리형 불펜 자원으로 해석할 수 있다.

#C4는 높은 피출루·피장타 허용과 낮은 탈삼진 능력을 보이는 저성과 유형이다. 
#강한 타구와 배럴 허용도 높아 실점 위험이 가장 큰 불펜 클러스터로 해석된다.

##시각화----
#선발
ggplot(starter_cluster_result,
       aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(
    title = "Starter Pitcher K-means Clusters",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  theme_minimal()

#불펜
ggplot(reliever_cluster_result,
       aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(
    title = "Reliever Pitcher K-means Clusters",
    x = "PC1",
    y = "PC2",
    color = "Cluster"
  ) +
  theme_minimal()

#회귀----
library(tidyverse)
library(janitor)
library(broom)
library(car)

##팀 코드 필터링----
multi_team_codes <- c("2TM", "3TM", "4TM", "5TM")

mlb_teams <- c(
  "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET",
  "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK",
  "PHI", "PIT", "SDP", "SEA", "SFG", "STL", "TBR", "TEX", "TOR", "WSN"
)

pitcher_cluster_all <- bind_rows(
  starter_cluster_result %>% mutate(role = "starter"),
  reliever_cluster_result %>% mutate(role = "reliever")
) %>%
  mutate(
    team = if_else(team == "ATH", "OAK", team)
  ) %>%
  filter(
    !team %in% multi_team_codes,
    team %in% mlb_teams
  )
## 팀- 시즌별 클러스터 ip 비중----
team_cluster_wide <- pitcher_cluster_all %>%
  filter(!is.na(season), !is.na(team), !is.na(ip), !is.na(cluster)) %>%
  group_by(season, team, role, cluster) %>%
  summarise(
    cluster_ip = sum(ip, na.rm = TRUE),
    n_pitchers = n(),
    .groups = "drop"
  ) %>%
  group_by(season, team, role) %>%
  mutate(
    total_role_ip = sum(cluster_ip, na.rm = TRUE),
    ip_share = cluster_ip / total_role_ip
  ) %>%
  ungroup() %>%
  mutate(
    role_cluster = paste0(role, "_c", cluster, "_ip_share")
  ) %>%
  select(season, team, role_cluster, ip_share) %>%
  pivot_wider(
    names_from = role_cluster,
    values_from = ip_share,
    values_fill = 0
  )

team_cluster_wide %>%
  count(season)

#2025 팀 ATH = OAK 합치기
pitcher_cluster_all_before_filter <- bind_rows(
  starter_cluster_result %>% mutate(role = "starter"),
  reliever_cluster_result %>% mutate(role = "reliever")
)

pitcher_cluster_all_before_filter %>%
  filter(season == 2025) %>%
  distinct(team) %>%
  arrange(team)
setdiff(
  pitcher_cluster_all_before_filter %>%
    filter(season == 2025) %>%
    distinct(team) %>%
    pull(team),
  mlb_teams
)

##팀성과 데이터----
##팀성과 데이터 만들기----
library(tidyverse)
library(janitor)
gameinfo <- read_csv("MLB_allgameinfo.csv", show_col_types = FALSE) %>%
  clean_names()
gameinfo_regular <- gameinfo %>%
  filter(
    !visteam %in% c("ALS", "NLS"),
    !hometeam %in% c("ALS", "NLS")
  )
game_team_long <- bind_rows(
  gameinfo_regular %>%
    transmute(
      season = season,
      team_raw = hometeam,
      opponent_raw = visteam,
      runs_for = hruns,
      runs_against = vruns,
      win = if_else(wteam == hometeam, 1, 0),
      loss = if_else(lteam == hometeam, 1, 0),
      home = 1
    ),
  
  gameinfo_regular %>%
    transmute(
      season = season,
      team_raw = visteam,
      opponent_raw = hometeam,
      runs_for = vruns,
      runs_against = hruns,
      win = if_else(wteam == visteam, 1, 0),
      loss = if_else(lteam == visteam, 1, 0),
      home = 0
    )
) %>%
  mutate(
    team = dplyr::recode(
      team_raw,
      "ANA" = "LAA",
      "CHA" = "CHW",
      "CHN" = "CHC",
      "LAN" = "LAD",
      "SLN" = "STL",
      "NYA" = "NYY",
      "NYN" = "NYM",
      "SFN" = "SFG",
      "SDN" = "SDP",
      "KCA" = "KCR",
      "TBA" = "TBR",
      "WAS" = "WSN",
      "FLO" = "MIA",
      "ATH" = "OAK",
      .default = team_raw
    )
  )

team_record_model <- game_team_long %>%
  group_by(season, team) %>%
  summarise(
    games = n(),
    w = sum(win, na.rm = TRUE),
    l = sum(loss, na.rm = TRUE),
    r = sum(runs_for, na.rm = TRUE),
    ra = sum(runs_against, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    win_pct = w / games,
    run_diff = r - ra,
    run_diff_per_game = run_diff / games,
    runs_per_game = r / games,
    runs_allowed_per_game = ra / games
  ) %>%
  select(
    season,
    team,
    games,
    w,
    l,
    win_pct,
    run_diff,
    run_diff_per_game,
    runs_per_game,
    runs_allowed_per_game
  )
##확인----
team_record_model %>%
  count(season)

team_record_model %>%
  filter(season == 2025) %>%
  arrange(team)

team_record_model %>%
  summarise(
    min_games = min(games),
    max_games = max(games),
    n_team_season = n()
  )

team_record_model %>%
  filter(games < 160) %>%
  arrange(season, team)
gameinfo_regular %>%
  count(season) %>%
  mutate(expected_team_games = n * 2)
##일단 21년 제외
team_record_model <- team_record_model %>%
  filter(season >= 2022)
team_model_data <- team_cluster_wide %>%
  filter(season >= 2022) %>%
  left_join(
    team_record_model,
    by = c("season", "team")
  )
team_record_model %>%
  filter(season >= 2022) %>%
  summarise(
    min_games = min(games),
    max_games = max(games),
    n_team_season = n()
  )

reg_vars <- c(
  # starter_c3_ip_share를 기준범주로 제외
  "starter_c1_ip_share",
  "starter_c2_ip_share",
  "starter_c4_ip_share",
  
  # reliever_c4_ip_share를 기준범주로 제외
  "reliever_c1_ip_share",
  "reliever_c2_ip_share",
  "reliever_c3_ip_share"
)
#model1 (득실차)----
model_run_diff <- lm(
  run_diff_per_game ~
    starter_c1_ip_share +
    starter_c2_ip_share +
    starter_c4_ip_share +
    reliever_c1_ip_share +
    reliever_c2_ip_share +
    reliever_c3_ip_share +
    factor(season),
  data = team_model_data
)

summary(model_run_diff)
tidy(model_run_diff, conf.int = TRUE)
glance(model_run_diff)
#model2 (승률)----
model_win_pct <- lm(
  win_pct ~
    starter_c1_ip_share +
    starter_c2_ip_share +
    starter_c4_ip_share +
    reliever_c1_ip_share +
    reliever_c2_ip_share +
    reliever_c3_ip_share +
    factor(season),
  data = team_model_data
)

summary(model_win_pct)
tidy(model_win_pct, conf.int = TRUE)
glance(model_win_pct)

#model3 (실점) best ----
model_ra_pg <- lm(
  runs_allowed_per_game ~
    starter_c1_ip_share +
    starter_c2_ip_share +
    starter_c4_ip_share +
    reliever_c1_ip_share +
    reliever_c2_ip_share +
    reliever_c3_ip_share +
    factor(season),
  data = team_model_data
)

summary(model_ra_pg)
tidy(model_ra_pg, conf.int = TRUE)
glance(model_ra_pg)

#다중공선성
car::vif(model_run_diff)
car::vif(model_win_pct)
car::vif(model_ra_pg)

#회귀진단
par(mfrow = c(2, 2))
plot(model_ra_pg)
par(mfrow = c(1, 1))
par(mfrow = c(2, 2))
plot(model_win_pct)
par(mfrow = c(1, 1))
#robust standard error 표본이 크지 않아서----
library(lmtest)
library(sandwich)

coeftest(model_run_diff, vcov = vcovHC(model_run_diff, type = "HC1"))
coeftest(model_win_pct, vcov = vcovHC(model_win_pct, type = "HC1"))
coeftest(model_ra_pg, vcov = vcovHC(model_ra_pg, type = "HC1"))

#저장
model_results <- bind_rows(
  tidy(model_run_diff, conf.int = TRUE) %>%
    mutate(model = "Run Differential per Game"),
  
  tidy(model_win_pct, conf.int = TRUE) %>%
    mutate(model = "Win Percentage"),
  
  tidy(model_ra_pg, conf.int = TRUE) %>%
    mutate(model = "Runs Allowed per Game")
) %>%
  select(model, term, estimate, std.error, statistic, p.value, conf.low, conf.high)

#모델 적합도 저장
model_fit_summary <- bind_rows(
  glance(model_run_diff) %>%
    mutate(model = "Run Differential per Game"),
  
  glance(model_win_pct) %>%
    mutate(model = "Win Percentage"),
  
  glance(model_ra_pg) %>%
    mutate(model = "Runs Allowed per Game")
) %>%
  select(model, r.squared, adj.r.squared, sigma, statistic, p.value, AIC, BIC, df.residual)

write_csv(model_fit_summary, "cluster_regression_fit_summary.csv")
write_csv(model_results, "cluster_regression_results.csv")

# 마이애미 따로 비교: 2022년 이후만 ----
miami_cluster_compare <- team_cluster_wide %>%
  filter(season >= 2022) %>%
  mutate(
    group = if_else(team == "MIA", "MIA", "League")
  ) %>%
  group_by(group) %>%
  summarise(
    across(
      where(is.numeric) & -season,
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

miami_cluster_compare

## 마이애미 시즌별 변화----
miami_by_season <- team_cluster_wide %>%
  filter(season >= 2022, team == "MIA") %>%
  arrange(season)

###그래프----
library(tidyverse)
library(scales)

# 마이애미 시즌별 클러스터 비중 long 형태로 변환
miami_by_season_long <- miami_by_season %>%
  pivot_longer(
    cols = contains("_ip_share"),
    names_to = "role_cluster",
    values_to = "ip_share"
  ) %>%
  separate(
    role_cluster,
    into = c("role", "cluster", "ip", "share"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    season = factor(season),
    role = dplyr::recode(
      role,
      "starter" = "Starter",
      "reliever" = "Reliever"
    ),
    cluster = str_to_upper(cluster)
  )

# 누적 막대그래프
ggplot(
  miami_by_season_long,
  aes(
    x = season,
    y = ip_share,
    fill = cluster
  )
) +
  geom_col(width = 0.75) +
  facet_wrap(~ role, ncol = 1) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "Miami Marlins Pitcher Cluster Composition by Season",
    subtitle = "Based on inning share by pitcher role and cluster",
    x = "Season",
    y = "Inning Share",
    fill = "Cluster"
  ) +
  theme_minimal()
#말린스 시즌별 승률
team_record_model %>%
  filter(season == 2022, team == "MIA") %>%
  mutate(win_pct_percent = win_pct * 100) %>%
  select(season, team, games, w, l, win_pct, win_pct_percent)
#클러스터 비율 비교---- 
## 클러스터 + 팀성과----
team_cluster_perf <- team_cluster_wide %>%
  filter(season >= 2022) %>%
  left_join(
    team_record_model %>% filter(season >= 2022),
    by = c("season", "team")
  ) %>%
  mutate(
    win_500_group = if_else(win_pct >= 0.5, "WinPct_500_plus", "Below_500"),
    is_miami = if_else(team == "MIA", 1, 0)
  )
team_cluster_perf %>%
  summarise(
    n = n(),
    missing_win_pct = sum(is.na(win_pct))
  )

team_cluster_perf %>%
  count(season, win_500_group)
##비교분석----
cluster_cols <- team_cluster_perf %>%
  select(contains("_ip_share")) %>%
  names()

win500_cluster_profile <- team_cluster_perf %>%
  filter(win_pct >= 0.5) %>%
  summarise(
    across(
      all_of(cluster_cols),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(group = "WinPct_500_plus") %>%
  relocate(group)

miami_cluster_profile <- team_cluster_perf %>%
  filter(team == "MIA") %>%
  summarise(
    across(
      all_of(cluster_cols),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(group = "MIA") %>%
  relocate(group)

compare_win500_mia <- bind_rows(
  win500_cluster_profile,
  miami_cluster_profile
)

compare_win500_mia
#부족or 과잉
cluster_gap_mia_vs_win500 <- compare_win500_mia %>%
  pivot_longer(
    cols = all_of(cluster_cols),
    names_to = "role_cluster",
    values_to = "ip_share"
  ) %>%
  pivot_wider(
    names_from = group,
    values_from = ip_share
  ) %>%
  mutate(
    gap_mia_minus_win500 = MIA - WinPct_500_plus,
    gap_pct_point = gap_mia_minus_win500 * 100
  ) %>%
  arrange(gap_pct_point)

View(cluster_gap_mia_vs_win500)

##그래프----
library(tidyverse)
library(scales)

cluster_cols <- team_cluster_perf %>%
  select(contains("_ip_share")) %>%
  names()

win500_cluster_profile <- team_cluster_perf %>%
  filter(win_pct >= 0.5) %>%
  summarise(
    across(
      all_of(cluster_cols),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(group = ".500 이상 팀") %>%
  relocate(group)

miami_cluster_profile <- team_cluster_perf %>%
  filter(team == "MIA") %>%
  summarise(
    across(
      all_of(cluster_cols),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  mutate(group = "Miami Marlins") %>%
  relocate(group)

compare_win500_mia_long <- bind_rows(
  win500_cluster_profile,
  miami_cluster_profile
) %>%
  pivot_longer(
    cols = all_of(cluster_cols),
    names_to = "role_cluster",
    values_to = "ip_share"
  )

ggplot(
  compare_win500_mia_long,
  aes(
    x = role_cluster,
    y = ip_share,
    fill = group
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = ".500 이상 팀 vs Miami Marlins 투수 클러스터 구성비율",
    x = "투수 역할-클러스터",
    y = "이닝 비중",
    fill = "비교 집단"
  ) +
  theme_minimal()
#csv 저장----
write_csv(starter_cluster_result, "starter_cluster_result.csv")
write_csv(reliever_cluster_result, "reliever_cluster_result.csv")
write_csv(pitcher_cluster_all, "pitcher_cluster_all.csv")
write_csv(team_cluster_wide, "team_cluster_wide.csv")
write_csv(team_record_model, "team_record_model.csv")
write_csv(team_model_data, "team_model_data.csv")
write_csv(team_cluster_perf, "team_cluster_perf.csv")
write_csv(starter_cluster_summary, "starter_cluster_summary.csv")
write_csv(reliever_cluster_summary, "reliever_cluster_summary.csv")
#저장----
saveRDS(
  list(
    starter_cluster_result = starter_cluster_result,
    reliever_cluster_result = reliever_cluster_result,
    pitcher_cluster_all = pitcher_cluster_all,
    team_cluster_wide = team_cluster_wide,
    team_record_model = team_record_model,
    team_model_data = team_model_data,
    team_cluster_perf = team_cluster_perf,
    starter_cluster_summary = starter_cluster_summary,
    reliever_cluster_summary = reliever_cluster_summary
  ),
  file = "mlb_pitcher_cluster_analysis_objects.rds"
)
#불러오기
saved_objects <- readRDS("mlb_pitcher_cluster_analysis_objects.rds")

starter_cluster_result <- saved_objects$starter_cluster_result
reliever_cluster_result <- saved_objects$reliever_cluster_result
pitcher_cluster_all <- saved_objects$pitcher_cluster_all
team_cluster_wide <- saved_objects$team_cluster_wide
team_record_model <- saved_objects$team_record_model
team_model_data <- saved_objects$team_model_data
team_cluster_perf <- saved_objects$team_cluster_perf
starter_cluster_summary <- saved_objects$starter_cluster_summary
reliever_cluster_summary <- saved_objects$reliever_cluster_summary

#SALARY ----
library(tidyverse)
library(janitor)
library(broom)
library(scales)

## 연봉 데이터 파일 불러오기 ----
salary_files <- list.files(
  pattern = "^mlb_pitcher_salaries_20[0-9]{2}.*\\.csv$"
)

salary_files

## 이름 정리 함수 ----
clean_salary_name <- function(x) {
  x %>%
    str_remove_all("[*#]") %>%
    str_squish()
}

## 연도별 salary 파일 읽기 함수 ----
read_salary_one_year <- function(file) {
  
  year_from_file <- str_extract(file, "20[0-9]{2}") %>% as.integer()
  
  df <- read_csv(file, show_col_types = FALSE) %>%
    clean_names()
  
  # 2025 파일처럼 season 컬럼이 없는 경우 파일명에서 season 생성
  if (!"season" %in% names(df)) {
    df <- df %>%
      mutate(season = year_from_file)
  }
  
  df %>%
    mutate(
      season = as.integer(season),
      salary = as.numeric(salary),
      player_salary_name = clean_salary_name(player_name),
      team_salary = if_else(team == "ATH", "OAK", team)
    ) %>%
    select(
      season,
      player_salary_name,
      salary_team = team_salary,
      salary_position = position,
      salary
    )
}

salary_all_raw <- map_dfr(salary_files, read_salary_one_year)

glimpse(salary_all_raw)

## 선수-시즌으로 변환----
salary_player_year <- salary_all_raw %>%
  group_by(season, player_salary_name) %>%
  summarise(
    salary = max(salary, na.rm = TRUE),
    salary_team = paste(sort(unique(salary_team)), collapse = "/"),
    salary_position = paste(sort(unique(salary_position)), collapse = "/"),
    .groups = "drop"
  ) %>%
  mutate(
    salary_million = salary / 1000000
  )

salary_player_year %>%
  count(season)

## pitcher_cluster_all에 join용 이름 만들기 ----
pitcher_cluster_salary <- pitcher_cluster_all %>%
  mutate(
    player_join_name = case_when(
      "player_clean" %in% names(.) ~ player_clean,
      "player" %in% names(.) ~ player,
      "player_name" %in% names(.) ~ player_name,
      TRUE ~ NA_character_
    ),
    player_join_name = clean_salary_name(player_join_name),
    team = if_else(team == "ATH", "OAK", team)
  ) %>%
  left_join(
    salary_player_year,
    by = c(
      "season" = "season",
      "player_join_name" = "player_salary_name"
    )
  ) %>%
  mutate(
    war_per_1m = war / salary_million,
    salary_missing = is.na(salary)
  )

## salary 매칭 확인 ----
pitcher_cluster_salary %>%
  summarise(
    n_total = n(),
    n_salary_matched = sum(!is.na(salary)),
    n_salary_missing = sum(is.na(salary)),
    match_rate = mean(!is.na(salary))
  )

#cluster별 연봉 summary----
cluster_salary_summary <- pitcher_cluster_salary %>%
  filter(!is.na(salary)) %>%
  group_by(role, cluster) %>%
  summarise(
    n = n(),
    salary_mean = mean(salary, na.rm = TRUE),
    salary_median = median(salary, na.rm = TRUE),
    salary_sd = sd(salary, na.rm = TRUE),
    salary_q1 = quantile(salary, 0.25, na.rm = TRUE),
    salary_q3 = quantile(salary, 0.75, na.rm = TRUE),
    salary_min = min(salary, na.rm = TRUE),
    salary_max = max(salary, na.rm = TRUE),
    
    war_mean = mean(war, na.rm = TRUE),
    fip_mean = mean(fip, na.rm = TRUE),
    whip_mean = mean(whip, na.rm = TRUE),
    ip_mean = mean(ip, na.rm = TRUE),
    
    war_per_1m_mean = mean(war_per_1m, na.rm = TRUE),
    war_per_1m_median = median(war_per_1m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(role, cluster)

View(cluster_salary_summary)

#100만달러 기준
cluster_salary_summary_million <- cluster_salary_summary %>%
  mutate(
    across(
      c(
        salary_mean,
        salary_median,
        salary_sd,
        salary_q1,
        salary_q3,
        salary_min,
        salary_max
      ),
      ~ .x / 1000000,
      .names = "{.col}_m"
    )
  ) %>%
  select(
    role,
    cluster,
    n,
    salary_mean_m,
    salary_median_m,
    salary_q1_m,
    salary_q3_m,
    salary_min_m,
    salary_max_m,
    war_mean,
    fip_mean,
    whip_mean,
    ip_mean,
    war_per_1m_mean,
    war_per_1m_median
  )

cluster_salary_summary_million

#선발 salary 분포----
ggplot(
  pitcher_cluster_salary %>%
    filter(role == "starter", !is.na(salary)),
  aes(x = cluster, y = salary_million)
) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "Starter Pitcher Salary Distribution by Cluster",
    x = "Starter Cluster",
    y = "Salary"
  ) +
  theme_minimal()
#불펜 salary 분포----
ggplot(
  pitcher_cluster_salary %>%
    filter(role == "reliever", !is.na(salary)),
  aes(x = cluster, y = salary_million)
) +
  geom_boxplot(outlier.alpha = 0.4) +
  scale_y_continuous(labels = dollar_format(suffix = "M")) +
  labs(
    title = "Reliever Pitcher Salary Distribution by Cluster",
    x = "Reliever Cluster",
    y = "Salary"
  ) +
  theme_minimal()

# war/$1M 분포
ggplot(
  pitcher_cluster_salary %>%
    filter(!is.na(war_per_1m), is.finite(war_per_1m)),
  aes(x = cluster, y = war_per_1m)
) +
  geom_boxplot(outlier.alpha = 0.4) +
  facet_wrap(~ role, scales = "free_y") +
  labs(
    title = "WAR per $1M by Pitcher Cluster",
    x = "Cluster",
    y = "WAR per $1M"
  ) +
  theme_minimal()

# 2025시즌 클러스터별 연봉 분포----
cluster_salary_2025_summary <- pitcher_cluster_salary %>%
  filter(season == 2025, !is.na(salary)) %>%
  group_by(role, cluster) %>%
  summarise(
    n = n(),
    salary_mean = mean(salary, na.rm = TRUE),
    salary_median = median(salary, na.rm = TRUE),
    salary_q1 = quantile(salary, 0.25, na.rm = TRUE),
    salary_q3 = quantile(salary, 0.75, na.rm = TRUE),
    war_mean = mean(war, na.rm = TRUE),
    fip_mean = mean(fip, na.rm = TRUE),
    whip_mean = mean(whip, na.rm = TRUE),
    ip_mean = mean(ip, na.rm = TRUE),
    war_per_1m_mean = mean(war_per_1m, na.rm = TRUE),
    war_per_1m_median = median(war_per_1m, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    salary_mean_m = salary_mean / 1000000,
    salary_median_m = salary_median / 1000000,
    salary_q1_m = salary_q1 / 1000000,
    salary_q3_m = salary_q3 / 1000000
  ) %>%
  arrange(role, cluster)

cluster_salary_2025_summary

# 회귀 결과 정리 ----
reg_priority <- bind_rows(
  tidy(model_ra_pg) %>%
    mutate(model = "runs_allowed_per_game"),
  
  tidy(model_run_diff) %>%
    mutate(model = "run_diff_per_game"),
  
  tidy(model_win_pct) %>%
    mutate(model = "win_pct")
) %>%
  filter(str_detect(term, "_ip_share")) %>%
  mutate(
    role = str_extract(term, "starter|reliever"),
    cluster = str_extract(term, "c[0-9]") %>%
      str_remove("c") %>%
      factor(),
    
    good_direction = case_when(
      model == "runs_allowed_per_game" & estimate < 0 ~ 1,
      model == "run_diff_per_game" & estimate > 0 ~ 1,
      model == "win_pct" & estimate > 0 ~ 1,
      TRUE ~ 0
    ),
    
    significant_10 = if_else(p.value < 0.10, 1, 0),
    significant_05 = if_else(p.value < 0.05, 1, 0)
  )

reg_priority

#회귀 우선순위 점수----
cluster_reg_score <- reg_priority %>%
  group_by(role, cluster) %>%
  summarise(
    reg_good_direction_count = sum(good_direction, na.rm = TRUE),
    reg_significant_10_count = sum(significant_10, na.rm = TRUE),
    reg_significant_05_count = sum(significant_05, na.rm = TRUE),
    
    ra_pg_estimate = estimate[model == "runs_allowed_per_game"],
    ra_pg_pvalue = p.value[model == "runs_allowed_per_game"],
    
    run_diff_estimate = estimate[model == "run_diff_per_game"],
    run_diff_pvalue = p.value[model == "run_diff_per_game"],
    
    win_pct_estimate = estimate[model == "win_pct"],
    win_pct_pvalue = p.value[model == "win_pct"],
    .groups = "drop"
  )

cluster_reg_score

# 마이애미 부족 클러스터 정리 ----
mia_need_cluster <- cluster_gap_mia_vs_win500 %>%
  mutate(
    role = str_extract(role_cluster, "starter|reliever"),
    cluster = str_extract(role_cluster, "c[0-9]") %>%
      str_remove("c") %>%
      factor(),
    
    need_type = case_when(
      gap_pct_point < 0 ~ "MIA 부족",
      gap_pct_point > 0 ~ "MIA 과잉",
      TRUE ~ "비슷함"
    ),
    
    shortage_score = if_else(gap_pct_point < 0, abs(gap_pct_point), 0)
  ) %>%
  left_join(
    cluster_reg_score,
    by = c("role", "cluster")
  ) %>%
  mutate(
    # 저성과 클러스터는 추천 제외
    cluster_quality = case_when(
      role == "starter" & cluster == "1" ~ "elite",
      role == "starter" & cluster == "2" ~ "good",
      role == "starter" & cluster == "4" ~ "middle",
      role == "starter" & cluster == "3" ~ "bad",
      
      role == "reliever" & cluster == "2" ~ "elite",
      role == "reliever" & cluster == "3" ~ "good",
      role == "reliever" & cluster == "1" ~ "risky_middle",
      role == "reliever" & cluster == "4" ~ "bad",
      TRUE ~ "check"
    ),
    
    recommend_cluster = case_when(
      need_type == "MIA 부족" &
        cluster_quality != "bad" &
        !is.na(ra_pg_estimate) &
        ra_pg_estimate < 0 &
        ra_pg_pvalue < 0.10 ~ "최우선 보강",
      
      need_type == "MIA 부족" &
        cluster_quality != "bad" ~ "보강 후보",
      
      TRUE ~ "비추천 또는 낮은 우선순위"
    )
  ) %>%
  arrange(desc(shortage_score))

mia_need_cluster

#최종 추천 대상 클러스터----
target_clusters <- mia_need_cluster %>%
  filter(recommend_cluster %in% c("최우선 보강", "보강 후보")) %>%
  select(
    role,
    cluster,
    role_cluster,
    gap_pct_point,
    shortage_score,
    cluster_quality,
    recommend_cluster,
    ra_pg_estimate,
    ra_pg_pvalue,
    run_diff_estimate,
    run_diff_pvalue,
    win_pct_estimate,
    win_pct_pvalue
  )

target_clusters

# 2025 후보군 만들기 ----
## 최근 3년 성과----
# 2023~2025 가중 평균 사용 
recent_player_form <- pitcher_cluster_salary %>%
  filter(
    season %in% 2023:2025,
    !is.na(player_join_name),
    !is.na(role)
  ) %>%
  mutate(
    season_weight = case_when(
      season == 2025 ~ 0.5,
      season == 2024 ~ 0.3,
      season == 2023 ~ 0.2,
      TRUE ~ 0
    ),
    
    # FIP, WHIP 같은 비율형 지표는 이닝 가중
    ip_weight = if_else(!is.na(ip), ip, 0),
    weighted_ip = season_weight * ip_weight
  ) %>%
  group_by(player_join_name, role) %>%
  summarise(
    recent_n_seasons = n_distinct(season),
    
    recent_war_weighted = sum(war * season_weight, na.rm = TRUE) /
      sum(season_weight[!is.na(war)], na.rm = TRUE),
    
    recent_fip_weighted = sum(fip * weighted_ip, na.rm = TRUE) /
      sum(weighted_ip[!is.na(fip)], na.rm = TRUE),
    
    recent_whip_weighted = sum(whip * weighted_ip, na.rm = TRUE) /
      sum(weighted_ip[!is.na(whip)], na.rm = TRUE),
    
    recent_ip_total = sum(ip, na.rm = TRUE),
    recent_ip_weighted = sum(ip * season_weight, na.rm = TRUE),
    
    recent_war_sum = sum(war, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    recent_war_weighted = if_else(
      is.nan(recent_war_weighted),
      NA_real_,
      recent_war_weighted
    ),
    recent_fip_weighted = if_else(
      is.nan(recent_fip_weighted),
      NA_real_,
      recent_fip_weighted
    ),
    recent_whip_weighted = if_else(
      is.nan(recent_whip_weighted),
      NA_real_,
      recent_whip_weighted
    )
  )

recent_player_form %>%
  summarise(
    n_players = n(),
    n_recent_war_missing = sum(is.na(recent_war_weighted)),
    n_recent_fip_missing = sum(is.na(recent_fip_weighted)),
    n_recent_whip_missing = sum(is.na(recent_whip_weighted))
  )


## 보강 대상 클러스터 선정----

target_clusters_b <- target_clusters %>%
  mutate(
    cluster_priority = case_when(
      recommend_cluster == "최우선 보강" ~ 2,
      recommend_cluster == "보강 후보" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  arrange(
    desc(cluster_priority),
    desc(shortage_score)
  )
target_clusters_b

##2025후보군----
candidate_2025_b <- pitcher_cluster_salary %>%
  filter(
    season == 2025,
    team != "MIA",
    !is.na(salary),
    !is.na(war),
    !is.na(fip),
    !is.na(whip),
    !is.na(ip)
  ) %>%
  mutate(
    cluster = factor(cluster)
  ) %>%
  inner_join(
    target_clusters_b,
    by = c("role", "cluster")
  ) %>%
  left_join(
    recent_player_form,
    by = c("player_join_name", "role")
  ) %>%
  mutate(
    salary_million = salary / 1000000,
    
    # 2025 WAR/$1M
    war_per_1m_2025 = war / salary_million,
    
    # 최근 가중 WAR 기준 WAR/$1M
    recent_war_per_1m = recent_war_weighted / salary_million
  )


# 후보군 확인
candidate_2025_b %>%
  count(role, cluster, cluster_quality, recommend_cluster)

# 클러스터별 benchmark 만들기----
# median, Q1, Q3 사용
cluster_benchmark_2025_b <- candidate_2025_b %>%
  group_by(role, cluster) %>%
  summarise(
    n_cluster_candidates = n(),
    
    # salary 분위수
    salary_q1 = quantile(salary, 0.25, na.rm = TRUE),
    salary_median = median(salary, na.rm = TRUE),
    salary_q3 = quantile(salary, 0.75, na.rm = TRUE),
    
    # 2025 성과 분위수
    war_2025_median = median(war, na.rm = TRUE),
    war_2025_q3 = quantile(war, 0.75, na.rm = TRUE),
    
    fip_2025_median = median(fip, na.rm = TRUE),
    fip_2025_q1 = quantile(fip, 0.25, na.rm = TRUE),
    
    whip_2025_median = median(whip, na.rm = TRUE),
    whip_2025_q1 = quantile(whip, 0.25, na.rm = TRUE),
    
    ip_2025_median = median(ip, na.rm = TRUE),
    ip_2025_q3 = quantile(ip, 0.75, na.rm = TRUE),
    
    # 최근 3년 성과 분위수
    recent_war_median = median(recent_war_weighted, na.rm = TRUE),
    recent_war_q3 = quantile(recent_war_weighted, 0.75, na.rm = TRUE),
    
    recent_fip_median = median(recent_fip_weighted, na.rm = TRUE),
    recent_fip_q1 = quantile(recent_fip_weighted, 0.25, na.rm = TRUE),
    
    recent_whip_median = median(recent_whip_weighted, na.rm = TRUE),
    recent_whip_q1 = quantile(recent_whip_weighted, 0.25, na.rm = TRUE),
    
    recent_ip_median = median(recent_ip_total, na.rm = TRUE),
    recent_ip_q3 = quantile(recent_ip_total, 0.75, na.rm = TRUE),
    
    # WAR/$1M은 음수 WAR 선수까지 benchmark에 넣으면 기준이 왜곡될 수 있음
    recent_war_per_1m_median = median(
      recent_war_per_1m[recent_war_weighted > 0],
      na.rm = TRUE
    ),
    recent_war_per_1m_q3 = quantile(
      recent_war_per_1m[recent_war_weighted > 0],
      0.75,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


cluster_benchmark_2025_b

# 선수 후보 평가용 데이터 만들기----
candidate_2025_eval_b <- candidate_2025_b %>%
  left_join(
    cluster_benchmark_2025_b,
    by = c("role", "cluster")
  ) %>%
  mutate(
    # Performance score
    #    - 2025 한 시즌 성과 + 최근 3년 가중 성과를 함께 반영
    #    - WAR은 높을수록 좋음
    #    - FIP, WHIP는 낮을수록 좋음
    
    perf_2025_score =
      case_when(
        war >= war_2025_q3 ~ 2,
        war >= war_2025_median ~ 1,
        TRUE ~ 0
      ) +
      case_when(
        fip <= fip_2025_q1 ~ 2,
        fip <= fip_2025_median ~ 1,
        TRUE ~ 0
      ) +
      case_when(
        whip <= whip_2025_q1 ~ 2,
        whip <= whip_2025_median ~ 1,
        TRUE ~ 0
      ),
    
    perf_recent_score =
      case_when(
        recent_war_weighted >= recent_war_q3 ~ 2,
        recent_war_weighted >= recent_war_median ~ 1,
        TRUE ~ 0
      ) +
      case_when(
        recent_fip_weighted <= recent_fip_q1 ~ 2,
        recent_fip_weighted <= recent_fip_median ~ 1,
        TRUE ~ 0
      ) +
      case_when(
        recent_whip_weighted <= recent_whip_q1 ~ 2,
        recent_whip_weighted <= recent_whip_median ~ 1,
        TRUE ~ 0
      ),
    
    # 최근 성과를 더 신뢰하되, 2025 현재 성과도 반영
    performance_score = 0.4 * perf_2025_score + 0.6 * perf_recent_score,
    
    # Cost score
    #    - 평균 대신 salary Q1, median 기준
    #    - WAR/$1M은 최근 3년 가중 WAR 기준 사용
    salary_score = case_when(
      salary <= salary_q1 ~ 3,
      salary <= salary_median ~ 2,
      salary <= salary_q3 ~ 1,
      TRUE ~ 0
    ),
    
    value_score = case_when(
      recent_war_weighted > 0 &
        recent_war_per_1m >= recent_war_per_1m_q3 ~ 2,
      recent_war_weighted > 0 &
        recent_war_per_1m >= recent_war_per_1m_median ~ 1,
      TRUE ~ 0
    ),
    
    cost_score = salary_score + value_score,
    
    # Reliability score
    #    - 2025 이닝 + 최근 3년 이닝 안정성
    reliability_2025_score = case_when(
      ip >= ip_2025_q3 ~ 2,
      ip >= ip_2025_median ~ 1,
      TRUE ~ 0
    ),
    
    reliability_recent_score = case_when(
      recent_ip_total >= recent_ip_q3 ~ 2,
      recent_ip_total >= recent_ip_median ~ 1,
      TRUE ~ 0
    ),
    
    reliability_score = 0.4 * reliability_2025_score +
      0.6 * reliability_recent_score,
    
    # 최종 점수
    #    - 회귀 결과는 클러스터 선정 단계에서 이미 반영
    #    - 선수 평가 단계에서는 Need / Performance / Cost / Reliability만 사용
   
    total_score =
      performance_score +
      cost_score +
      reliability_score
  )

# 추천 후보 필터링----
#    - 저성과 클러스터 제외
#    - WAR이 음수인 선수 제외
#    - 최근 가중 WAR이 음수인 선수 제외

candidate_2025_scored_b <- candidate_2025_eval_b %>%
  filter(
    cluster_quality != "bad",
    war > 0,
    recent_war_weighted > 0
  )


# 선발 추천 후보----

starter_recommend_2025_b <- candidate_2025_scored_b %>%
  filter(
    role == "starter",
    ip >= 30,
    recent_ip_total >= 60
  ) %>%
  arrange(
    desc(total_score),
    desc(performance_score),
    desc(cost_score),
    desc(reliability_score),
    fip,
    whip,
    salary
  ) %>%
  select(
    season,
    player = player_join_name,
    team,
    role,
    cluster,
    cluster_quality,
    recommend_cluster,
    gap_pct_point,
    
    salary,
    salary_million,
    
    war,
    recent_war_weighted,
    recent_war_sum,
    recent_n_seasons,
    
    war_per_1m_2025,
    recent_war_per_1m,
    
    fip,
    recent_fip_weighted,
    whip,
    recent_whip_weighted,
    ip,
    recent_ip_total,
    
    k_percent,
    bb_percent,
    xobp,
    xslg,
    
    total_score,
    performance_score,
    cost_score,
    reliability_score,
    salary_score,
    value_score
  ) %>%
  slice_head(n = 5)


starter_recommend_2025_b

# 7. 불펜 추천 후보

reliever_recommend_2025_b <- candidate_2025_scored_b %>%
  filter(
    role == "reliever",
    ip >= 10,
    recent_ip_total >= 25
  ) %>%
  arrange(
    desc(total_score),
    desc(performance_score),
    desc(cost_score),
    desc(reliability_score),
    fip,
    whip,
    salary
  ) %>%
  select(
    season,
    player = player_join_name,
    team,
    role,
    cluster,
    cluster_quality,
    recommend_cluster,
    gap_pct_point,
    
    salary,
    salary_million,
    
    war,
    recent_war_weighted,
    recent_war_sum,
    recent_n_seasons,
    
    war_per_1m_2025,
    recent_war_per_1m,
    
    fip,
    recent_fip_weighted,
    whip,
    recent_whip_weighted,
    ip,
    recent_ip_total,
    
    k_percent,
    bb_percent,
    xobp,
    xslg,
    
    total_score,
    performance_score,
    cost_score,
    reliability_score,
    salary_score,
    value_score
  ) %>%
  slice_head(n = 5)


reliever_recommend_2025_b

# 최종 추천 후보 합치기----

final_pitcher_recommend_2025_b <- bind_rows(
  starter_recommend_2025_b,
  reliever_recommend_2025_b
) %>%
  arrange(
    role,
    desc(total_score),
    desc(performance_score),
    desc(cost_score)
  )


View(final_pitcher_recommend_2025_b)

# 저장----

write_csv(target_clusters_b, "target_clusters_b.csv")
write_csv(candidate_2025_scored_b, "candidate_2025_scored_b.csv")
write_csv(final_pitcher_recommend_2025_b, "final_pitcher_recommend_2025_b.csv")

target_clusters_b = read_csv('target_clusters_b.csv')
candidate_2025_scored_b = read_csv('candidate_2025_scored_b.csv')
final_pitcher_recommend_2025_b = read_csv('final_pitcher_recommend_2025_b.csv')

#한계----
#연봉 기준, 점수화 차이
#실제 FA인지, 트레이드가 가능한지
#계약 기간이 남았는지
#구단이 팔 의사가 있는지
#부상 이력
#나이와 향후 하락 가능성
#40인 로스터 상황
#트레이드 대가