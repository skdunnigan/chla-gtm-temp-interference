
# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'data.xlsx')) %>%
       janitor::clean_names() %>%
       dplyr::mutate(dilution_percent = forcats::as_factor(dilution_percent))

chla_c <- 24 # enter starting concentration of chlorophyll in collection sample
temp_r <- 20 # enter reference temperature

# 02 create dilution plot ----------------------------------------------------

chla_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " RFU"))

dat %>%
  ggplot(mapping = aes(x = temp_c, y = chla_rfu, color = dilution_percent)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = 'bottom', show.legend = FALSE) +
  scale_color_discrete(name = "Dilution Factor") +
  xlim(0, 40) +
  theme_bw() +
  theme(legend.position = "right") +
  labs(y = chla_RFU_title,
       x = 'Temperature ('~degree*C*')')


# 03 calculate linear reg for each dil ---------------------------------------

## 03a run linear regression model ----
## on all dilution_percent groups and clean output with `broom::tidy()`
lm_out <- dat %>%
          dplyr::group_by(dilution_percent) %>%
          do(broom::tidy(lm(chla_rfu ~ temp_c, data = .)))

## can get additional diagnostics with `broom::glance()` function
dat %>%
  group_by(dilution_percent) %>%
  summarise(broom::glance(lm(chla_rfu ~ temp_c, data = .)))

## 03b pull out slopes and intercepts ----
## for each group and merge into new file
m <- lm_out %>%
      dplyr::filter(term == '(Intercept)') %>%
      dplyr::rename(m = estimate)

b <- lm_out %>%
      dplyr::filter(term != '(Intercept)') %>%
      dplyr::select(dilution_percent, estimate) %>%
      dplyr::rename(b = estimate)

lm_new <- dplyr::left_join(m, b, by = "dilution_percent") # create new dataframe

## add the lm results to the original data frame (but only m and b)
dat_lm <- dplyr::left_join(dat, lm_new, by = "dilution_percent") %>%
          dplyr::select(1:4, 6, 10, 7:9, -5)

rm(m, b, lm_new, lm_out)


# 04 calculate p for each dilution group ----------------------------------
dat_lm <- dat_lm %>%
          dplyr::mutate(p = m/chla_c)

p_avg <- mean(dat_lm$p, na.rm = TRUE)


# 05 calculate corrected chlorophyll values -------------------------------

chla_c <- function(x, y){
  y/(1 + p_avg(x - temp_r))
}

dat_lm %>%
  mutate(chla_r = chla_c(temp_c, chla_rfu))
