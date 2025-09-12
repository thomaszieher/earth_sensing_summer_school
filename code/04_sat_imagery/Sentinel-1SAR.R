# install.packages(c("tidyverse", "lubridate"))

library(tidyverse)
library(lubridate)

# 1) 读入数据（把字符串 "NA" 识别为缺失）
# 假设文件名为 data.csv，第一行是表头######DESCENDING#######ASCENDING
df <- readr::read_csv("E:/Summerschool_test_S1/point1.csv", na = c("NA", "", "NaN"))

# 若你的文件其实是空格/Tab 分隔的（不是逗号），用下面这行替代：
# df <- readr::read_delim("data.csv", delim = "\t|\\s+", na = c("NA", "", "NaN"), col_names = TRUE, trim_ws = TRUE)

# 2) 解析日期列（原始格式像 2017/1/1）
df <- df %>%
  mutate(
    date = as.Date(date, format = "%Y/%m/%d")
    # 也可用：date = lubridate::ymd(date)
  ) %>%
  arrange(date)

# 3) 选取需要作图的数值列并转成长表
value_cols <- c("rvi_mean", "vh_mean", "vv_mean", "ratio_mean")

df_long <- df %>%
  pivot_longer(
    cols = all_of(value_cols),
    names_to = "variable",
    values_to = "value"
  )

# 4) 画图：时间为横轴，其他参数为纵轴；按变量分面（不同量纲更好看）
p <- ggplot(df_long, aes(x = date, y = value)) +
  geom_line(na.rm = TRUE) +
  geom_point(size = 1, na.rm = TRUE) +
  facet_grid(variable ~ station, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(x = "日期", y = "数值", title = "各参数时间序列（按站点与变量分面）") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(p)

# 可选：保存图片
# ggsave("timeseries_facet.png", p, width = 10, height = 8, dpi = 300)


library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(MASS)
library(rlang)
library(ggplot2)

K_max <- 1
y_col <- "value"       # 你的 df_long 就是 date / station / variable / value
y <- rlang::sym(y_col)

make_harm <- function(df, K, w){
  if (K <= 0) return(df)
  harm <- map_dfc(1:K, ~tibble(!!paste0("cos", .x) := cos(.x*w*df$t),
                               !!paste0("sin", .x) := sin(.x*w*df$t)))
  bind_cols(df, harm)
}

fit_one_series <- function(dat, K_max = 1, y_col = "value"){
  y <- rlang::sym(y_col)
  
  dat <- dat %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::filter(!is.na(date), !is.na(!!y)) %>%
    dplyr::arrange(date)
  
  if (nrow(dat) < 3) {
    return(list(pred = tibble(date = dat$date, fit = NA_real_), K = 0))
  }
  
  dat <- dat %>% dplyr::mutate(t = as.numeric(date - min(date)))
  w <- 2*pi/365.25
  
  fit_with_K <- function(K){
    dfk <- make_harm(dat, K, w)
    rhs <- c("t", if (K>0) c(paste0("cos",1:K), paste0("sin",1:K)))
    fml <- as.formula(paste(y_col, "~", paste(rhs, collapse = " + ")))
    mod <- try(MASS::rlm(fml, data = dfk, psi = MASS::psi.huber), silent = TRUE)
    if (inherits(mod, "try-error")) mod <- try(stats::lm(fml, data = dfk), silent = TRUE)
    if (inherits(mod, "try-error")) return(NULL)
    
    newd <- tibble(date = seq(min(dat$date), max(dat$date), by = "1 day")) %>%
      dplyr::mutate(t = as.numeric(date - min(dat$date))) %>%
      make_harm(K, w)
    newd$fit <- as.numeric(predict(mod, newdata = newd))
    list(pred = newd, K = K)
  }
  
  res <- fit_with_K(K_max)
  if (is.null(res) && K_max > 1) res <- fit_with_K(K_max - 1)
  if (is.null(res)) res <- fit_with_K(0)
  res
}

# —— 分组拟合（方式一：group_modify）——
fits <- df_long %>%
  dplyr::group_by(station, variable) %>%
  dplyr::group_modify(~{
    res <- fit_one_series(.x, K_max = K_max, y_col = y_col)
    res$pred %>% dplyr::select(date, fit)
  }) %>%
  dplyr::ungroup()

# 若你的 dplyr 版本对 group_modify 仍不友好，用方式二（nest/unnest）：
# fits <- df_long %>%
#   dplyr::group_by(station, variable) %>%
#   tidyr::nest() %>%
#   dplyr::mutate(pred = purrr::map(data, ~ fit_one_series(.x, K_max = K_max, y_col = y_col)$pred)) %>%
#   dplyr::select(-data) %>%
#   tidyr::unnest(pred) %>%
#   dplyr::ungroup()

# —— 叠加绘图 —— 
ggplot(df_long, aes(date, !!y)) +
  geom_point(size = 1, alpha = 0.6, na.rm = TRUE) +
  geom_line(data = fits, aes(y = fit), linewidth = 1) +
  facet_grid(variable ~ station, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(x = "日期", y = "数值", title = "各参数时间序列与稳健谐波拟合") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())


# 原始观测：只画折线（去掉点）
obs_line <- df_long %>% dplyr::filter(!is.na(value))

p_fit <- ggplot() +
  # 先画原始观测的灰色折线（NA 会自动断开）
  geom_line(data = obs_line, aes(date, value),
            color = "grey60", linewidth = 0.6, na.rm = TRUE) +
  # 再画谐波拟合的黑色直线
  geom_line(data = fits, aes(date, fit),
            color = "black", linewidth = 1) +
  facet_grid(variable ~ station, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(x = "日期", y = "数值",
       title = "各参数时间序列（灰线：原始；黑线：谐波拟合）") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

print(p_fit)

# rbeast异常突变检测
library(Rbeast)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(ggplot2)

# —— 工具：把 BEAST 组件提取为 data.frame(date, value) —— 
# —— 工具：提取拐点（兼容 cp=索引 或 cp=绝对时间） —— 
# ---------- 更稳的组件/拐点提取 ----------
extract_comp <- function(comp, default_time_num) {
  if (is.null(comp) || is.null(comp$Y)) {
    return(tibble(date = as.Date(numeric(0), origin = "1970-01-01"),
                  value = numeric(0)))
  }
  y <- as.numeric(comp$Y) %>% as.vector()
  tnum <- if (!is.null(comp$time)) as.numeric(comp$time) else default_time_num
  tnum <- as.vector(tnum)
  keep_time <- !is.na(tnum)
  tnum <- tnum[keep_time]
  n <- min(length(y), length(tnum))
  tibble(
    date  = as.Date(tnum[seq_len(n)], origin = "1970-01-01"),
    value = y[seq_len(n)]
  )
}

extract_cp <- function(component, ser_dates) {
  if (is.null(component) || is.null(component$cp)) {
    return(tibble(date = as.Date(character()), prob = numeric(0)))
  }
  cp_raw  <- as.numeric(component$cp) %>% as.vector()
  cp_prob <- component$cpPr
  cp_prob <- if (!is.null(cp_prob)) as.numeric(cp_prob) %>% as.vector() else rep(NA_real_, length(cp_raw))
  
  # 去 NA，同步裁剪概率
  keep <- !is.na(cp_raw)
  if (!any(keep)) return(tibble(date = as.Date(character()), prob = numeric(0)))
  cp_raw  <- cp_raw[keep]
  cp_prob <- cp_prob[seq_len(length(cp_raw))]
  
  is_integer_like <- all(abs(cp_raw - round(cp_raw)) < 1e-6)
  in_range <- length(cp_raw) > 0 &&
    min(cp_raw) >= 1 &&
    max(cp_raw) <= length(ser_dates)
  
  if (is_integer_like && in_range) {
    idx <- pmax(1, pmin(length(ser_dates), round(cp_raw)))
    tibble(date = ser_dates[idx], prob = cp_prob)
  } else {
    tibble(date = as.Date(cp_raw, origin = "1970-01-01"), prob = cp_prob)
  }
}

# ---------- 更稳的 BEAST 封装（显式指定 deltat） ----------
run_beast_one <- function(ser,
                          period = 365.25,
                          sorder = 1,
                          tcp = c(0, 3),
                          scp = c(0, 3),
                          deltat_user = NULL) {
  
  ser <- ser %>%
    dplyr::filter(!is.na(.data$date), !is.na(.data$value)) %>%
    dplyr::arrange(.data$date) %>%
    # 同一天多条观测，先平均，避免“重复时间”干扰
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = mean(.data$value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(.data$date)
  
  if (nrow(ser) < 5) {
    return(list(
      pred = tibble::tibble(date = ser$date, yhat = NA_real_, trend = NA_real_, seas = NA_real_),
      cps  = tibble::tibble(date = as.Date(character()), prob = numeric(0), type = character())
    ))
  }
  
  tnum <- as.numeric(ser$date)
  # 估计等间隔步长（若未手动指定）
  if (is.null(deltat_user)) {
    dts <- diff(unique(tnum))
    dt_est <- suppressWarnings(as.integer(round(stats::median(dts, na.rm = TRUE))))
    if (is.na(dt_est) || dt_est < 1) dt_est <- 1
  } else {
    dt_est <- as.integer(deltat_user)
  }
  
  # 先尝试 deltat=，若该版本不支持则用 metadata$deltaTime
  fit <- tryCatch(
    Rbeast::beast.irreg(
      y          = ser$value,
      time       = tnum,
      season     = "harmonic",
      period     = period,
      sorder     = sorder,
      tcp.minmax = tcp,
      scp.minmax = scp,
      outlier    = TRUE,
      deltat     = dt_est
    ),
    error = function(e) NULL,
    warning = function(w) invokeRestart("muffleWarning")
  )
  if (is.null(fit)) {
    fit <- tryCatch(
      Rbeast::beast.irreg(
        y          = ser$value,
        time       = tnum,
        season     = "harmonic",
        period     = period,
        sorder     = sorder,
        tcp.minmax = tcp,
        scp.minmax = scp,
        outlier    = TRUE,
        metadata   = list(deltaTime = dt_est)
      ),
      error = function(e) NULL,
      warning = function(w) invokeRestart("muffleWarning")
    )
  }
  if (is.null(fit)) {
    return(list(
      pred = tibble::tibble(date = ser$date, yhat = NA_real_, trend = NA_real_, seas = NA_real_),
      cps  = tibble::tibble(date = as.Date(character()), prob = numeric(0), type = character())
    ))
  }
  
  default_time_num <- tnum
  trend_df <- extract_comp(fit$trend,  default_time_num) %>% dplyr::rename(trend = value)
  seas_df  <- extract_comp(fit$season, default_time_num) %>% dplyr::rename(seas  = value)
  
  pred_df <- dplyr::full_join(trend_df, seas_df, by = "date") %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
      trend = as.numeric(trend),
      seas  = as.numeric(seas),
      yhat  = tidyr::replace_na(trend, 0) + tidyr::replace_na(seas, 0)
    ) %>%
    dplyr::filter(!is.na(yhat))
  
  cp_trend  <- extract_cp(fit$trend,  ser$date) %>% dplyr::mutate(type = "trend")
  cp_season <- extract_cp(fit$season, ser$date) %>% dplyr::mutate(type = "season")
  cp_df <- dplyr::bind_rows(cp_trend, cp_season)
  
  list(pred = pred_df, cps = cp_df)
}

# ========== 批量运行 ==========
beast_res <- df_long %>%
  dplyr::group_by(station, variable) %>%
  tidyr::nest() %>%
  dplyr::mutate(res = purrr::map(.data$data, run_beast_one)) %>%
  dplyr::ungroup()

# 拟合曲线（黑线）
preds <- beast_res %>%
  dplyr::transmute(station, variable, pred = purrr::map(.data$res, "pred")) %>%
  tidyr::unnest(pred)

# 拐点（竖线）
cps <- beast_res %>%
  dplyr::transmute(station, variable, cp = purrr::map(.data$res, "cps")) %>%
  tidyr::unnest(cp)

# 原始观测（灰线；不画点）
obs_line <- df_long %>% dplyr::filter(!is.na(.data$value))

# ========== 叠加绘图 ==========
p <- ggplot() +
  geom_line(data = obs_line, aes(date, value),
            color = "grey60", linewidth = 0.6, na.rm = TRUE) +
  geom_line(data = preds, aes(date, yhat),
            color = "black", linewidth = 1, na.rm = TRUE) +
  geom_vline(data = cps,
             aes(xintercept = as.numeric(date), color = type, alpha = prob),
             linewidth = 0.7, show.legend = TRUE) +
  scale_color_manual(values = c(trend = "red3", season = "royalblue4"),
                     name = "拐点类型") +
  scale_alpha_continuous(range = c(0.3, 0.9), na.value = 0.6,
                         name = "后验概率") +
  facet_grid(variable ~ station, scales = "free_y") +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(x = "日期", y = "数值",
       title = "Rbeast 异常与突变检测：灰=原始，黑=拟合，竖线=拐点") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

# ======== 仅新增：官方多面板诊断图函数（不改任何参数） ========
plot_beast_panel <- function(df_long, station_sel, variable_sel,
                             vars = c("y","s","scp","sorder","t","tcp","torder","error"),
                             main = NULL, ...) {
  stopifnot(exists("run_beast_one"))
  
  # 读取你当前 run_beast_one 的默认参数，确保一致
  fml <- formals(run_beast_one)
  period      <- eval(fml$period)
  sorder      <- eval(fml$sorder)
  tcp         <- eval(fml$tcp)
  scp         <- eval(fml$scp)
  deltat_user <- eval(fml$deltat_user)
  
  # 取该面板的序列，保持与主流程相同的预处理
  ser <- df_long %>%
    dplyr::filter(.data$station == station_sel,
                  .data$variable == variable_sel) %>%
    dplyr::filter(!is.na(.data$date), !is.na(.data$value)) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
  
  if (nrow(ser) < 5) stop("该序列点数过少，无法生成诊断图。")
  
  tnum <- as.numeric(ser$date)
  if (is.null(deltat_user)) {
    dts <- diff(unique(tnum))
    dt_est <- suppressWarnings(as.integer(round(stats::median(dts, na.rm = TRUE))))
    if (is.na(dt_est) || dt_est < 1) dt_est <- 1
  } else {
    dt_est <- as.integer(deltat_user)
  }
  
  fit <- tryCatch(
    Rbeast::beast.irreg(
      y = ser$value, time = tnum,
      season = "harmonic", period = period, sorder = sorder,
      tcp.minmax = tcp, scp.minmax = scp,
      outlier = TRUE, deltat = dt_est
    ),
    error = function(e) NULL,
    warning = function(w) invokeRestart("muffleWarning")
  )
  if (is.null(fit)) {
    fit <- tryCatch(
      Rbeast::beast.irreg(
        y = ser$value, time = tnum,
        season = "harmonic", period = period, sorder = sorder,
        tcp.minmax = tcp, scp.minmax = scp,
        outlier = TRUE, metadata = list(deltaTime = dt_est)
      ),
      error = function(e) NULL,
      warning = function(w) invokeRestart("muffleWarning")
    )
  }
  if (is.null(fit)) stop("BEAST 拟合失败，无法绘制诊断图。")
  
  if (is.null(main)) main <- paste(station_sel, variable_sel)
  plot(fit, vars = vars, main = main, ...)
}

# ======== 使用示例：为一个面板画官方诊断图 ========
# 挑一个你要看的组合（可修改）
plot_beast_panel(df_long, station_sel = "point1", variable_sel = "rvi_mean",ncpStat="max")