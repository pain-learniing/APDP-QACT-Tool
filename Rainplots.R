##########################################################################################
## Script for Plotting distributions of individual-level fitted parameters 
## as raincloud plots
## For supplement figures: S18, S19
## Swati wrote this file for plotting rain plot for balloon task control vs pain group
###########################################################################################
library("tidyverse")
library("ggplot2")
library("cowplot")
library("readr")
library("patchwork")
library("gridExtra")
source("https://raw.githubusercontent.com/datavizpyr/data/master/half_flat_violinplot.R")

# plotting variables
cols = c("C" = "#0C7BDC","P" = "#FFC20A")
hr = 7.5/1.75
wr = 7

## Model 3arm bandit ----------------

# read individual parameter estimate data
#rl_path = "C:/Users/Swati Rajwal/Downloads/EXPERIMENT 2 (MIA'S ONLINE GAME) ANALYSIS PIPELINE/_2_modelling/prefit_output/mod1_RL/"
#m1_control_path = dir(rl_path,full.names = F,pattern = paste('ind.*.','control_mod1_RL','.*',sep=''))
#m1_pain_path = dir(rl_path,full.names = F,pattern = paste('ind.*.','pain_mod1_RL','.*',sep=''))

#m1C = read.csv(paste(rl_path, m1_control_path, sep=''))
f_m1c = file.choose()  ## mydata_fit_ind_est.csv for control group
m1C = read.csv(f_m1c)
m1C['Group'] = rep('Controls', dim(m1C)[1])

f_m1p = file.choose() ## mydata_fit_ind_est.csv for pain group
m1P = read.csv(f_m1p)
m1P['Group'] = rep('Pain', dim(m1P)[1])

mod1data = rbind(m1C, m1P)

# mu_Arew parameter plot
mu_Arew_plot <- ggplot(mod1data, aes(x = Group, y = Arew_mean, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = Arew_mean),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = Arew_mean, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('mu_Arew') + theme(axis.title.x = element_blank())

# mu_Apun
mu_Apun_plot <- ggplot(mod1data, aes(x = Group, y = Apun_mean, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = Apun_mean),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = Apun_mean, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('mu_Apun') + theme(axis.title.x = element_blank())


#mu_R
mu_R_plot <- ggplot(mod1data, aes(x = Group, y = R_mean, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = R_mean),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = R_mean, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('mu_R') + theme(axis.title.x = element_blank())

#mu_P
mu_P_plot <- ggplot(mod1data, aes(x = Group, y = P_mean, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = P_mean),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = P_mean, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('mu_P') + theme(axis.title.x = element_blank())

#mu_xi
mu_xi_plot <- ggplot(mod1data, aes(x = Group, y = xi_mean, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = xi_mean),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = xi_mean, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('mu_xi') + theme(axis.title.x = element_blank())

#mu_d
mu_d_plot <- ggplot(mod1data, aes(x = Group, y = d_mean, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = d_mean),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = d_mean, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('mu_d') + theme(axis.title.x = element_blank())


# arrange plots
mod1_rainclouds = mu_Arew_plot+mu_Apun_plot+mu_R_plot+mu_P_plot+ mu_xi_plot+mu_d_plot+ plot_layout(guides = "collect") & theme(legend.position = 'bottom') 
mod1_rainclouds
## save figure
save_dir = '../_Figures/'
ggplot2::ggsave(paste(save_dir,'IndParamsRL_S18.eps', sep=''), plot=mod1_rainclouds,device=cairo_ps,dpi=300,width=400*wr,height=450*hr,units="px")
dev.off()

