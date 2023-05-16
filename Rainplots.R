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

## Model 2: KF ----------------

# read data
#kf_path = '../_2_modelling/prefit_output/mod2_KF/'

#m2_control_path = dir(kf_path,full.names = F,pattern = paste('ind.*.','control','.*',sep=''))
#m2_pain_path = dir(kf_path,full.names = F,pattern = paste('ind.*.','pain','.*',sep=''))
#m2C = read.csv(paste(kf_path,m2_control_path, sep=''))

m2C = read.csv(file.choose())  #read mod2_KF ind_control.csv file
m2C['Group'] = rep('Controls', dim(m2C)[1])
#m2P = read.csv(paste(kf_path, m2_pain_path, sep = '')) 
m2P = read.csv(file.choose())   #read mod2_KF ind_pain.csv file
m2P['Group'] = rep('Back Pain', dim(m2P)[1])

mod2data = rbind(m2C, m2P)

# Volatility
m2_v_plot <- ggplot(mod2data, aes(x = Group, y = drift, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = drift),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = drift, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('v') + theme(axis.title.x = element_blank())

# Stochasticity
m2_s_plot <- ggplot(mod2data, aes(x = Group, y = eps, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = eps),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = eps),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_fill_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('s')+ theme(axis.title.x = element_blank())

# Xi
m2_xi_plot <- ggplot(mod2data, aes(x = Group, y = xi, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = xi),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = xi, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab('\U03BE')+theme(axis.title.x = element_blank())

# W0
m2_w0_plot <- ggplot(mod2data, aes(x = Group, y = w0, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = w0),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = w0, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_fill_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab(bquote(w^0)) + theme(axis.title.x = element_blank())

# E0
m2_E0_plot <- ggplot(mod2data, aes(x = Group, y = E0, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = E0),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = E0, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_colour_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  scale_fill_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab(bquote(E^0)) + theme(axis.title.x = element_blank())

# C
m2_C_plot <- ggplot(mod2data, aes(x = Group, y = cs, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5) +
  geom_point(aes(x = Group, y = cs),
             position = position_jitter(width = .05), size = 2, shape = 20)  +
  geom_boxplot(aes(x = Group, y = cs, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  theme(text=element_text(family="Arial", size = 20)) + 
  scale_fill_manual(values = c("Back Pain" = "#FFC20A", "Controls" = "#0C7BDC")) +
  ylab("C")+ theme(axis.title.x = element_blank())

mod2_rainclouds = m2_v_plot + m2_s_plot + m2_xi_plot +
  m2_w0_plot + m2_E0_plot + m2_C_plot +
  plot_layout(guides = "collect", ncol = 2) & theme(legend.position = 'bottom') 
# arrange plots
mod2_rainclouds   
## save figure
save_dir = '../_Figures/'
ggplot2::ggsave(paste(save_dir, 'IndParamsKF_S19.eps', sep=''), plot=mod2_rainclouds ,device=cairo_ps,dpi=300,width=450*wr,height=750*hr,units="px")
dev.off()

