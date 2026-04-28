# This script runs a quantitative comparison of the G-B relationships 
# from a compilation of studies from the literature (Walker et al. 2021) and creates Figure S9.
# This script calculates the relative changes in biomass (dB/B) per unit 
# of relative change in growth (dG/G) from the Swiss forest data and creates Figure S3.

# load packages
library(dplyr)
library(lme4) 
library(lmerTest) 
library(ggplot2)
library(viridis)
library(here)
# Relative change biomass (plantC) vs. NPP ####

# From Walker et al. 2021 ####
table2_walker <- readr::read_csv(paste0(here::here(), "/data-raw/table2_walker.csv")) |> 
  rename(X95CI_beta = `95CI_beta`)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)
table2_walker_B <- table2_walker %>% 
  filter(biomeE_var=="plantC") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         SD_beta = ifelse(is.na(SD_beta), mean(SD_beta/beta,na.rm=T), SD_beta))
         
table2_walker_G <- table2_walker %>% 
  filter(biomeE_var=="NPP") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))

# Bootstrap with loop ...
out <- data.frame()

for (n in 1:1e5){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = n, biomass = B_sample, growth = G_sample, ratio = B_sample / G_sample) |> 
    bind_rows(out)
}

# ... or function
sample_walker <- function(id, table2_walker_G, table2_walker_B){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = id, biomass = B_sample, growth = G_sample, ratio = B_sample / G_sample)
  
  return(out)
}

out <- purrr::map_dfr(
  as.list(seq(1e5)),
  ~sample_walker(., table2_walker_G, table2_walker_B)
)

#write.csv(out, paste0(here::here(), "/data/out_bootstrap.csv"))

out <- read.csv(paste0(here::here(), "/data/out_bootstrap.csv"))
gg1 <- out |> 
  ggplot(aes(growth, biomass)) +
  geom_hex() +
  khroma::scale_fill_batlowW( reverse = TRUE) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[NPP]), y = expression(beta[Cveg]),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 12),legend.title = element_text(size = 12),
                     plot.title = element_text(size = 12),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.12, .78),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(breaks=seq(-5,5,5)) + 
  scale_y_continuous(breaks=seq(0,4,2)) 
gg1

out |> 
  ggplot(aes(growth, ..density..)) +
  geom_density() +
  theme_classic()

out |> 
  ggplot(aes(biomass, ..density..)) +
  geom_density() +
  theme_classic()

gg2 <- out |> 
  ggplot(aes(ratio, ..density..)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Cveg]/beta[NPP]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     plot.title = element_text(size = 12)) + 
  scale_x_continuous(limits= c(-0.3, 5), breaks=seq(0,5,1)) + 
  scale_y_continuous(breaks=c(0,0.6,0.3)) 
gg2

# Figure S9 ####
library(patchwork)
# gg1 + gg2 + plot_layout(ncol = 2) +
#   plot_annotation(
#     tag_levels = list(c("e", "f")),
#     tag_suffix = ")"
#   ) &
#   theme(plot.tag = element_text(size = 12))
# #cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
# ggsave(paste0(here::here(), "/fig/fig_Cveg_NPP.png"), width = 8, height = 4)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                 NPP to GPP                               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Relative change biomass (plantC) vs. NPP ####

# From Walker et al. 2021 ####
table2_walker <- openxlsx::read.xlsx(paste0(here::here(), "/data-raw/table2_walker.xlsx")) |> 
  rename(X95CI_beta = `95CI_beta`)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)
table2_walker_B <- table2_walker %>% 
  filter(biomeE_var=="GPP",
         !is.na(beta)) %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(SD_beta/beta,na.rm=T), SD_beta))

table2_walker_G <- table2_walker %>% 
  filter(biomeE_var=="NPP") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))
sample_walker <- function(id, table2_walker_G, table2_walker_B){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = id, GPP = B_sample, NPP = G_sample, ratio = B_sample / G_sample)
  
  return(out)
}
out <- purrr::map_dfr(
  as.list(seq(1e5)),
  ~sample_walker(., table2_walker_G, table2_walker_B)
)

#write.csv(out, paste0(here::here(), "/data/out_bootstrap_NPPGPP.csv"))


out <- read.csv(paste0(here::here(), "/data/out_bootstrap_NPPGPP.csv"))
gg3 <- out |> 
  ggplot(aes(GPP, NPP)) +
  geom_hex(bins=60) +
  khroma::scale_fill_batlowW( reverse = TRUE) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[GPP]), y = expression(beta[NPP]),title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     axis.title.y=element_text(angle=0, vjust = 0.5),
                     legend.text = element_text(size = 12),legend.title = element_text(size = 12),
                     plot.title = element_text(size = 12),
                     legend.key = element_rect(fill = NA, color = NA),
                     legend.position = c(.12, .78),
                     legend.direction="vertical",
                     legend.margin = margin(.2, .2, .2, .2),
                     legend.key.size = unit(.5, 'cm'),
                     #legend.box.background = element_rect(color="black",size=0.2),
                     legend.box.margin = margin(1, 1, 1, 1)) + 
  scale_x_continuous(breaks=seq(-5,5,5)) + 
  scale_y_continuous(breaks=seq(0,4,2)) 
gg3

out |> 
  ggplot(aes(NPP, ..density..)) +
  geom_density() +
  theme_classic()

out |> 
  ggplot(aes(GPP, ..density..)) +
  geom_density() +
  theme_classic()

out<-out |> 
  mutate(ratio_invert = 1/ratio)

gg4 <- out |> 
  ggplot(aes(ratio_invert, ..density..)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[NPP]/beta[GPP]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     plot.title = element_text(size = 12)) + 
  scale_x_continuous(limits= c(-0.3, 5), breaks=seq(0,5,1)) + 
  scale_y_continuous(breaks=c(0,0.6,0.3)) 
gg4

# # Figure S9 ####
# library(patchwork)
# gg1 + gg2 + plot_layout(ncol = 2) + 
#   plot_annotation(tag_levels = 'a', tag_suffix = ")")& 
#   theme(plot.tag = element_text(size = 12)) 
# #cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
# ggsave(paste0(here::here(), "/fig/fig_GPP_NPP.png"), width = 8, height = 4)
# 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Cveg to Cveg_belowground                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# From Walker et al. 2021 ####
table2_walker <- openxlsx::read.xlsx(paste0(here::here(), "/data-raw/table2_walker.xlsx")) |> 
  rename(X95CI_beta = `95CI_beta`)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)
table2_walker_B <- table2_walker %>% 
  filter(biomeE_var=="belowground",
         !is.na(beta)) %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))

table2_walker_G <- table2_walker %>% 
  filter(biomeE_var=="plantC") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))
sample_walker <- function(id, table2_walker_G, table2_walker_B){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = id, Croot = B_sample, Cveg = G_sample, ratio = B_sample / G_sample)
  
  return(out)
}
out <- purrr::map_dfr(
  as.list(seq(1e5)),
  ~sample_walker(., table2_walker_G, table2_walker_B)
)

#write.csv(out, paste0(here::here(), "/data/out_bootstrap_Cveg_Croot.csv"))


out <- read.csv(paste0(here::here(), "/data/out_bootstrap_Cveg_Croot.csv"))
gg5 <- out |> 
  ggplot(aes(x=Cveg, y=Croot)) +
  geom_hex(bins=60) +
  khroma::scale_fill_batlowW( reverse = TRUE) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Cveg]), y = expression(beta[Croot]), title=NULL) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(angle=0, vjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(.12, .78),
    legend.direction = "vertical",
    legend.margin = margin(.2, .2, .2, .2),
    legend.key.size = unit(.5, 'cm'),
    legend.box.margin = margin(1, 1, 1, 1)
  ) + 
  scale_x_continuous(breaks = seq(-5, 5, 5)) + 
  scale_y_continuous(breaks = seq(0, 4, 2), limits = c(-2, 4))
gg5

out |> 
  ggplot(aes(Cveg, ..density..)) +
  geom_density() +
  theme_classic()

out |> 
  ggplot(aes(Croot, ..density..)) +
  geom_density() +
  theme_classic()

out2<-out |> 
  mutate(ratio2=Cveg/Croot)

gg6 <- out2 |> 
  ggplot(aes(ratio2, ..density..)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Cveg]/beta[Croot]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     plot.title = element_text(size = 12)) + 
  scale_x_continuous(limits= c(-0.3, 5), breaks=seq(0,5,1)) + 
  scale_y_continuous(breaks=c(0,0.6,0.3)) 
gg6

# Figure S9 ####
library(patchwork)
gg1 + gg2+ gg3 + gg4 + gg5 + gg6  + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")")& 
  theme(plot.tag = element_text(size = 12)) 
#cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
ggsave(paste0(here::here(), "/fig/Over_all_walker_fig.png"), width = 8, height = 11)



### Codes below are not used anymore






##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------------ XX---------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~







##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Cveg to Cveg_aboveground                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# From Walker et al. 2021 ####
table2_walker <- openxlsx::read.xlsx(paste0(here::here(), "/data-raw/table2_walker.xlsx")) |> 
  rename(X95CI_beta = `95CI_beta`)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)
table2_walker_B <- table2_walker %>% 
  filter(biomeE_var=="aboveground",
         !is.na(beta)) %>%
  mutate(SD_beta = X95CI_beta/1.96,
         SD_beta = ifelse(is.na(SD_beta), mean(SD_beta/beta,na.rm=T), SD_beta))

table2_walker_G <- table2_walker %>% 
  filter(biomeE_var=="plantC") %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))
sample_walker <- function(id, table2_walker_G, table2_walker_B){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = id, Cwood = B_sample, Cveg = G_sample, ratio = B_sample / G_sample)
  
  return(out)
}
out <- purrr::map_dfr(
  as.list(seq(1e5)),
  ~sample_walker(., table2_walker_G, table2_walker_B)
)

write.csv(out, paste0(here::here(), "/data/out_bootstrap_Cveg_Cwood.csv"))


out <- read.csv(paste0(here::here(), "/data/out_bootstrap_Cveg_Cwood.csv"))
gg1 <- out |> 
  ggplot(aes(Cveg, Cwood)) +
  geom_hex(bins=60) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("lavenderblush3", "navy", "red", "yellow"))(5)) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Cveg]), y = expression(beta[Cwood]), title=NULL) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(angle=0, vjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(.12, .78),
    legend.direction = "vertical",
    legend.margin = margin(.2, .2, .2, .2),
    legend.key.size = unit(.5, 'cm'),
    legend.box.margin = margin(1, 1, 1, 1)
  ) + 
  scale_x_continuous(breaks = seq(-5, 5, 5)) + 
  scale_y_continuous(breaks = seq(0, 4, 2), limits = c(-2, 4))
gg1

out |> 
  ggplot(aes(Cveg, ..density..)) +
  geom_density() +
  theme_classic()

out |> 
  ggplot(aes(Cwood, ..density..)) +
  geom_density() +
  theme_classic()
out2<-out |> 
  mutate(ratio2=Cveg/Cwood)
gg2 <- out2 |> 
  ggplot(aes(ratio2, ..density..)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Cveg]/beta[Cwood]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     plot.title = element_text(size = 12)) + 
  scale_x_continuous(limits= c(-0.3, 5), breaks=seq(0,5,1)) + 
  scale_y_continuous(breaks=c(0,0.6,0.3)) 
gg2

# Figure S9 ####
library(patchwork)
gg1 + gg2 + plot_layout(ncol = 2) + 
  plot_annotation(tag_levels = 'a', tag_suffix = ")")& 
  theme(plot.tag = element_text(size = 12)) 
#cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
ggsave(paste0(here::here(), "/fig/fig_Cveg_Cwood.png"), width = 8, height = 4)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                           Cveg to Cveg_aboveground                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# From Walker et al. 2021 ####
table2_walker <- openxlsx::read.xlsx(paste0(here::here(), "/data-raw/table2_walker_Updated.xlsx")) |> 
  rename(X95CI_beta = `95CI_beta`)

# Need to convert 95% CI to SD by dividing by 1.96
# Replace the NAs values in 95% CI for the coefficient of variation (SD/mean)
table2_walker_B <- table2_walker %>% 
  filter(biomeE_var=="aboveground",
         !is.na(beta)) %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(SD_beta/beta,na.rm=T), SD_beta))

table2_walker_G <- table2_walker %>% 
  filter(biomeE_var=="belowground",
         !is.na(beta)) %>%
  mutate(SD_beta = X95CI_beta/1.96,
         CV=SD_beta/beta, CV = ifelse(CV>=0, CV, NA),
         SD_beta = ifelse(is.na(SD_beta), mean(CV,na.rm=T), SD_beta))
sample_walker <- function(id, table2_walker_G, table2_walker_B){
  
  i <- sample(dim(table2_walker_B)[1], 1)
  j <- sample(dim(table2_walker_G)[1], 1)
  
  B_sample <- rnorm(1, table2_walker_B$beta[i], table2_walker_B$SD_beta[i])
  G_sample <- rnorm(1, table2_walker_G$beta[j], table2_walker_G$SD_beta[j])
  
  out <- tibble(id = id, Cwood = B_sample, CRoot = G_sample, ratio = B_sample / G_sample)
  
  return(out)
}
out <- purrr::map_dfr(
  as.list(seq(1e5)),
  ~sample_walker(., table2_walker_G, table2_walker_B)
)

write.csv(out, paste0(here::here(), "/data/out_bootstrap_Croot_Cwood.csv"))


out <- read.csv(paste0(here::here(), "/data/out_bootstrap_Croot_Cwood.csv"))
gg1 <- out |> 
  ggplot(aes(CRoot, Cwood)) +
  geom_hex(bins=60) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("lavenderblush3", "navy", "red", "yellow"))(5)) +
  geom_abline(intercept=0, slope=1, linetype="dotted") +
  geom_hline(yintercept = 0, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Croot]), y = expression(beta[Cwood]), title=NULL) + 
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.title.y = element_text(angle=0, vjust = 0.5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 12),
    legend.key = element_rect(fill = NA, color = NA),
    legend.position = c(.12, .78),
    legend.direction = "vertical",
    legend.margin = margin(.2, .2, .2, .2),
    legend.key.size = unit(.5, 'cm'),
    legend.box.margin = margin(1, 1, 1, 1)
  ) + 
  scale_x_continuous(breaks = seq(-5, 5, 5)) + 
  scale_y_continuous(breaks = seq(0, 4, 2), limits = c(-2, 4))
gg1

out |> 
  ggplot(aes(CRoot, ..density..)) +
  geom_density() +
  theme_classic()

out |> 
  ggplot(aes(Cwood, ..density..)) +
  geom_density() +
  theme_classic()

gg2 <- out |> 
  ggplot(aes(ratio, ..density..)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype="dotted") +
  geom_vline(xintercept = 0, linetype="dotted") +
  labs(x = expression(beta[Cwood]/beta[Croot]), y = "Density",title=NULL) + 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     axis.text = element_text(size = 12),axis.title = element_text(size = 12),
                     plot.title = element_text(size = 12)) + 
  scale_x_continuous(limits= c(-0.3, 5), breaks=seq(0,5,1)) + 
  scale_y_continuous(breaks=c(0,0.6,0.3)) 
gg2

# Figure S9 ####
library(patchwork)
gg1 + gg2 + plot_layout(ncol = 2) +
  plot_annotation(
    tag_levels = list(c("c", "d")),
    tag_suffix = ")"
  ) &
  theme(plot.tag = element_text(size = 12))
#cowplot::plot_grid(gg1, gg2, rel_widths = c(1, 0.8), labels = c("a", "b"))
ggsave(paste0(here::here(), "/fig/fig_Croot_Cwood.png"), width = 8, height = 4)


