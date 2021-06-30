# great resource for interpreting lm model outputs: https://feliperego.github.io/blog/2015/10/23/Interpreting-Model-Output-In-R


# 00 load-packages -----------------------------------------------------------
# uncomment below if you need to load packages to run this code
source('R/00_load-packages.R')


# 01 load-data ---------------------------------------------------------------

dat <- readxl::read_xlsx(here::here('data', 'data.xlsx')) %>%
       janitor::clean_names() %>%
       dplyr::mutate(dilution_percent = forcats::as_factor(dilution_percent))

Tr <- 20 # enter reference temperature

# 02 create initial dilution plot ----------------------------------------------------

chla_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L"))
chla_RFU_title <- expression(paste("Chlorophyll ", italic("a "), mu*"g/L", " RFU"))

plot.a <- dat %>%
            ggplot(mapping = aes(x = temp_c, y = chla_rfu)) +
            stat_smooth(method = "lm", aes(color = dilution_percent), se = FALSE) +
            geom_point(size = 3) +
            scale_color_discrete(name = "Dilution Factor") +
            theme_bw() +
            theme(legend.position = "right",
                  axis.title = element_text(size = 12),
                  text = element_text(size = 12)) +
            labs(y = chla_RFU_title,
                 x = 'Temperature ('~degree*C*')')
plot.a

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

# derive p ----------------------------------------------------------------

plot.b <- ratios %>%
            ggplot(aes(x = b, y = m)) +
            geom_point(aes(color = dilution_percent), size = 3) +
            stat_smooth(method = "lm", se = FALSE, color = "black") +
            scale_color_discrete(name = "Dilution Factor") +
            theme_bw() +
            theme(legend.position = "right",
                  axis.title = element_text(size = 12),
                  text = element_text(size = 12)) +
            labs(x = "Intercept",
                 y = "Slope")
plot.b

## calculate slope for slope ( = y) vs intercept ( = x) of the relationship

p.lm <- ratios %>% lm(m ~ b, data = .)
p.lm.origin <- ratios %>% lm(m ~ 0 + b, data = .)

p.lm.tidy <- broom::tidy(p.lm)

p.lm.origin <- broom::tidy(p.lm.origin)


## get p

p <- p.lm.tidy[[2,2]]
p.origin <- p.lm.origin[[1,2]]


# 06 apply adjustment to chla values --------------------------------------

# Equation 1 from Watras et al. 2011:

Fr <- function(x, y){
  y / (1 + p*(x - Tr))
}

Fr_origin <- function(x, y){
  y / (1 + p.origin*(x - Tr))
}

dat_corr <- dat %>%
            mutate(chla_r = Fr(temp_c, chla_rfu),
                   chla_r_origin = Fr_origin(temp_c, chla_rfu))


# 07 create plot with corrected chlorophyll data --------------------------

plot.c <- dat_corr %>%
            ggplot(mapping = aes(x = temp_c, y = chla_r)) +
            stat_smooth(method = "lm", aes(color = dilution_percent), se = FALSE) +
            geom_point(size = 3) +
            scale_color_discrete(name = "Dilution Factor") +
            theme_bw() +
            theme(legend.position = "right",
                  axis.title = element_text(size = 12)) +
            labs(y = chla_RFU_title,
                 x = 'Temperature ('~degree*C*')')
plot.c

plot.d <- dat_corr %>%
            ggplot(mapping = aes(x = temp_c, y = chla_r_origin)) +
            stat_smooth(method = "lm", aes(color = dilution_percent), se = FALSE) +
            geom_point(size = 3) +
            scale_color_discrete(name = "Dilution Factor") +
            theme_bw() +
            theme(legend.position = "right",
                  axis.title = element_text(size = 12)) +
            labs(y = chla_RFU_title,
                 x = 'Temperature ('~degree*C*')')
plot.d


# 08 goodness of fit ------------------------------------------------------


