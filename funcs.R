get_mlb_data_cols <- function(df) {
  select(Date:Team, Final, Open:...23) %>%
    set_names(
      "date",
      "rot",
      "vh",
      "team",
      "final",
      "ml_open",
      "ml_close",
      "rl",
      "rl_odds",
      "ou_open",
      "ou_open_odds",
      "ou_close",
      "ou_close_odds",
      "seas"
    )
}


fix_halves <- function(num) {
  str_replace_all(num, "½", ".5")
}

# File functions
get_seas_from_file <- function(path = "data",
                               f_type = ".xlsx",
                               pat = "\\d{4}") {
  # extract season from filename
  dir(
    path = path,
    pattern = f_type
  ) %>%
    str_extract_all(pat) %>%
    unlist() %>%
    sort()
}

# get_seas_from_file()
get_files <- function(path = "data",
                      f_type = ".xlsx",
                      pat = "\\d{4}-\\d{2}") {
  # get vector of file names
  dir(
    path = path,
    pattern = f_type,
    full.names = TRUE
  ) %>%
    sort()
}

# Date function
get_month <- function(col, chars) {
  ifelse(chars == 4,
    substr(col, 1, 2),
    substr(col, 1, 1)
  )
}
get_day <- function(col, chars) {
  ifelse(chars == 2,
    substr(col, 3, 4),
    substr(col, 2, 3)
  )
}

generate_date <- function(df) {
  game_date <- df %>%
    dplyr::transmute(
      n_chars = nchar(date),
      month = get_month(date, n_chars) %>% as.numeric(),
      day = get_day(date, nchar(month)) %>% as.numeric(),
      seas
    ) %>%
    dplyr::transmute(date = paste0(seas, "-", month, "-", day) %>% as.Date())
  df %>%
    dplyr::select(-date) %>%
    dplyr::bind_cols(game_date)
}

gen_gid <- function(df) {
  #  is_data.frame(df)
  n_games <- nrow(df) / 2
  df$gid <- c(c(1:n_games), c(1:n_games)) %>% sort()
  df
}

mlb_select_cols <- function(df) {
  if (ncol(df) == 23) {
    df <- df %>%
      select(Date:Team, Final, Open:...23) %>%
      set_names(
        "date",
        "rot",
        "vh",
        "team",
        "final",
        "ml_open",
        "ml_close",
        "rl",
        "rl_odds",
        "ou_open",
        "ou_open_odds",
        "ou_close",
        "ou_close_odds"
      )
    return(df)
  }
  df <- df %>%
    select(Date:Team, Final, Open:...21) %>%
    mutate(
      rl = NA,
      rl_odds = NA
    ) %>%
    set_names(
      "date",
      "rot",
      "vh",
      "team",
      "final",
      "ml_open",
      "ml_close",
      "ou_open",
      "ou_open_odds",
      "ou_close",
      "ou_close_odds",
      "rl",
      "rl_odds"
    )
  df
}

convert_odds_numeric <- function(df) {
  df %>%
    mutate(
      ml_open = ifelse(tolower(ml_open) == "nl", NA, ml_open) %>%
        as.numeric(),
      ml_close = ifelse(tolower(ml_close) == "nl", NA, ml_close) %>%
        as.numeric(),
      rl = as.numeric(fix_halves(rl)),
      rl_odds = as.numeric(rl_odds),
      ou_open = as.numeric(fix_halves(ou_open)),
      ou_open_odds = as.numeric(ou_open_odds),
      ou_close = as.numeric(fix_halves(ou_close)),
      ou_close_odds = as.numeric(ou_close_odds)
    )
}

fix_neutral_games <- function(df) {
  is_even <- function(n) {
    !(n %% 2)
  }
  neutral_gid <- df %>%
    filter(vh == "N") %>%
    pull(gid) %>%
    unique()
  # even rotation are Home / odd rotation are away
  df[is_even(df$rot), ]$vh <- "H"
  df[!is_even(df$rot), ]$vh <- "V"
  df %>%
    mutate(neutral = gid %in% neutral_gid %>% as.numeric())
}






col_select_rename <- function(df, keep_cols = NULL, prefix) {
  df %>%
    select(one_of(keep_cols)) %>%
    set_names(paste0(prefix, names(.) %>% tolower()))
}


odds_transform <- function(df) {
  v <- df %>% filter(vh == "V")
  h <- df %>% filter(vh == "H")
  h_odds <- h %>%
    select(
      h_ml_open = ml_open,
      h_ml_close = ml_close,
      h_rl = rl,
      h_rl_odds = rl_odds,
      ou_open,
      ou_open_und = ou_open_odds,
      ou_close,
      ou_close_und = ou_close_odds
    )

  v_odds <- v %>% select(
    v_ml_open = ml_open,
    v_ml_close = ml_close,
    v_rl = rl,
    v_rl_odds = rl_odds,
    ou_open_ovr = ou_open_odds,
    ou_close_ovr = ou_close_odds
  )

  bind_cols(h_odds, v_odds)
}

team_transform <- function(df) {
  team_cols <- c(c("rot", "team", "final"))
  gm_cols <- c("gid", "date", "neutral")

  v_info <- df %>%
    filter(vh == "V") %>%
    select(-neutral) %>%
    col_select_rename(team_cols, "v_")

  h_info <- df %>%
    filter(vh == "H") %>%
    col_select_rename(team_cols, "h_")
  gm_info <- df %>%
    filter(vh == "H") %>%
    col_select_rename(gm_cols, "")

  gm_info %>%
    bind_cols(h_info) %>%
    bind_cols(v_info)
}

repair_step <- function(df) {
  checkr::check_data(df)
  mlb_select_cols(df) %>%
    gen_gid() %>%
    convert_odds_numeric() %>%
    fix_neutral_games()
}
transform_step <- function(df) {
  checkr::check_data(df)
  df %>%
    team_transform() %>%
    bind_cols(odds_transform(df))
}
odds_pipeline <- function(files, seasons) {
  checkr::check_character(files)
  checkr::check_character(seasons)
  df_list <- map2(
    files,
    seasons,
    ~ read_excel(.x) %>%
      repair_step() %>%
      transform_step() %>%
      mutate(seas = .y) %>%
      generate_date()
  )
  map(df_list, checkr::check_data)
  df_list %>%
    bind_rows()
}

mlb_polish_dataset <- function(df) {
  df <- df %>%
    select(
      gid,
      seas,
      date,
      v_rot,
      v_team,
      v_final,
      h_rot,
      h_team,
      h_final,
      starts_with("v_"),
      starts_with("h_"),
      ou_open,
      ou_open_ovr,
      ou_open_und,
      ou_close,
      ou_close_ovr,
      ou_close_und,
      neutral
    )

  df <- df %>%
    mutate(
      ou_open = ifelse(ou_open > 16, NA, ou_open),
      ou_close = ifelse(ou_close > 16, NA, ou_close),
      ou_open_ovr = ifelse(is.na(ou_open), NA, ou_open_ovr),
      ou_open_und = ifelse(is.na(ou_open), NA, ou_open_und),
      ou_close_ovr = ifelse(is.na(ou_close), NA, ou_open_ovr),
      ou_close_und = ifelse(is.na(ou_close), NA, ou_open_und)
    )

  df[df$h_team == "LOS", "h_team"] <- "LAD"
  df[df$v_team == "LOS", "v_team"] <- "LAD"
  df$gid <- NULL
  df
}
