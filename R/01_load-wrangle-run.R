# great resource for interpreting lm model outputs: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R


# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'data.xlsx')) %>%
       janitor::clean_names() %>%
       dplyr::mutate(dilution_percent = forcats::as_factor(dilution_percent))

chla_c <- 321 # enter starting concentration of chlorophyll in collection sample
Tr <- 20 # enter reference temperature

# 02 create dilution plot ----------------------------------------------------

## 02.1 create plotting function, call it 'lin_plot()' ----
lin_plot <- function(temp, y) {

chla_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " RFU"))

  dat %>%
    ggplot(mapping = aes(x = temp_c, y = x, color = dilution_percent)) +
    geom_point(size = 3) +
    stat_smooth(method = "lm", se = FALSE) +
    # ggpubr::stat_regline_equation(label.y.npc = 'bottom', show.legend = FALSE, size = 5) +
    scale_color_discrete(name = "Dilution Factor") +
    xlim(0, 40) +
    theme_bw() +
    theme(legend.position = "right",
          axis.title = element_text(size = 12),
          text = element_text(size = 12)) +
    labs(y = chla_RFU_title,
         x = 'Temperature ('~degree*C*')')

}

## 02.2 plot of lm Chla_rfu ~ Temperature ----
a <- lin_plot(dat$chla_rfu)
a

# 03 assess for slope:intercept constant across all concentrations ---------------------------------------

## linear regression model on all dilution_percent groups and clean output with `broom::tidy()`
## additional diagnostics with `broom::glance()` function
lm_out <- dat %>%
          dplyr::group_by(dilution_percent) %>%
          do(broom::tidy(lm(chla_rfu ~ temp_c, data = .)))

  diag <- dat %>%
          group_by(dilution_percent) %>%
          do(broom::glance(lm(chla_rfu ~ temp_c, data = .)))

##check out model outputs
lm_out
diag

## examine slope:intercept
m <- lm_out %>%
      dplyr::filter(term == 'temp_c') %>%
      dplyr::rename(m = estimate) %>%
      dplyr::select(-term)

b <- lm_out %>%
      dplyr::filter(term == '(Intercept)') %>%
      dplyr::select(dilution_percent, estimate) %>%
      dplyr::rename(b = estimate)

## examine slope:intercept ratios for each dilution: are they constant?
ratios <- dplyr::left_join(m, b, by = "dilution_percent") %>%
          dplyr::mutate(ratio = m/b)

# remove unnecessary df
rm(m, b, lm_out, diag)


# 04 adjust for reference temp at 20C -------------------------------------

dat2 <- dat %>%
        dplyr::mutate(delta_T = temp_c - Tr)

dat2 %>%
  ggplot(mapping = aes(x = delta_T, y = chla_rfu, color = dilution_percent)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_discrete(name = "Dilution Factor") +
  theme_bw() +
  theme(legend.position = "right",
        axis.title = element_text(size = 12),
        text = element_text(size = 12)) +
  labs(y = chla_RFU_title,
       x = expression(paste(Delta, 'Temperature ', "(", degree, "C)")),
       caption = expression(paste('Reference temperature: ', 20, degree, "C")))


# 05 calculate p ----------------------------------------------------------

lm_out <- dat2 %>%
          dplyr::group_by(dilution_percent) %>%
          do(broom::tidy(lm(chla_rfu ~ delta_T, data = .)))

diag <- dat2 %>%
        group_by(dilution_percent) %>%
        do(broom::glance(lm(chla_rfu ~ delta_T, data = .)))

##check out model outputs
lm_out
diag

## pull out slope and intercept
m <- lm_out %>%
  dplyr::filter(term == 'delta_T') %>%
  dplyr::rename(m = estimate) %>%
  dplyr::select(-term)

b <- lm_out %>%
  dplyr::filter(term == '(Intercept)') %>%
  dplyr::select(dilution_percent, estimate) %>%
  dplyr::rename(b = estimate)

## calculate p for each dilution
p_all <- dplyr::left_join(m, b, by = "dilution_percent") %>%
         dplyr::right_join(dat2, by = "dilution_percent") %>%
         dplyr::mutate(p = m/b)

# get average temperature coefficient for each concentration at adjusted temperature
p <- mean(p_all$p, na.rm = TRUE)
p_sd <- sd(p_all$p, na.rm = TRUE)

p_exp <- paste0(round(p, digits = 3), "??", round(p_sd, digits = 3))



# 06 apply adjustment to chla values --------------------------------------

chla_c <- function(x, y){
  y / (1 + p*(x - Tr))
}

dat_corr <- dat2 %>%
  mutate(chla_r = chla_c(temp_c, chla_rfu))

# 07 create plot with corrected chlorophyll data --------------------------

b <- dat_corr %>%
  ggplot(mapping = aes(x = temp_c, y = chla_r, color = dilution_percent)) +
  geom_point(size = 3) +
  stat_smooth(method = "lm", se = FALSE) +
  ggpubr::stat_regline_equation(label.y.npc = 'bottom', show.legend = FALSE) +
  scale_color_discrete(name = "Dilution Factor") +
  xlim(0, 40) +
  theme_bw() +
  theme(legend.position = "right",
        axis.title = element_text(size = 12)) +
  labs(y = chla_RFU_title,
       x = 'Temperature ('~degree*C*')')


# 08 goodness of fit ------------------------------------------------------


