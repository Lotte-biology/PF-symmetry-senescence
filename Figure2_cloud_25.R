
print("I started running the script")
install.packages("tidyverse")
install.packages("cowplot")
install.packages("mgcv")
print("I installed packages")

library(tidyverse)
library(cowplot)
library(mgcv)
print("Libraries installed")

# read data (last generation pop)
ddead<-read.table("dead_ABC.txt")
names(ddead)<-c("t","age","dam0","res0","dead")
ddead<- ddead %>% filter(t>4975)

my_del_col <- "#d92567"
my_font_size <- 16
damage_fill <- "#d96125"
## fig2A
dpf_res <- ddead %>% group_by(age) %>% summarise(y = median(res0),
                                                 ymin = quantile(res0,0.2),
                                                 ymax = quantile(res0,0.8))
dpf_res <- dpf_res %>% filter(age <= 22)

fig2A <- ggplot(dpf_res,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = "darkgreen") +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color = "darkgreen") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,22.5),
                     breaks = seq(0,22,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,5.5),
                     breaks = seq(0,5.5,1)) +
  labs(x = "Age", y = "Resources") +
  background_grid(major = "xy", minor = "y")

fig2A
print("Done figure 2A")

## fig2B

dpf_dam <- ddead %>% group_by(age) %>% summarise(y = median(dam0),
                                                 ymin = quantile(dam0,0.2),
                                                 ymax = quantile(dam0,0.8))
dpf_dam <- dpf_dam %>% filter(age <= 22)

fig2B <- ggplot(dpf_dam,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = damage_fill) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color = damage_fill) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,22.5),
                     breaks = seq(0,2,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,4.5),
                     breaks = seq(0,4.5,1)) +
  labs(x = "Age", y = "Damage") +
  background_grid(major = "xy", minor = "y")

fig2B
print("Done figure 2B")

## fig2C: mortality vs. age
dpf_mort <- ddead %>% group_by(age) %>% summarise(y = mean(dead))

dpf_mort <- dpf_mort %>% filter(age <= 22)    

dpf_gam <- ddead %>% filter(age <=22)
print("Starting gam")

mpf1 <- gam(dead ~ s(age), family = binomial, data = dpf_gam) 
print("Finished gam")

gam.check(mpf1)
dpf.pred <- data.frame(age=seq(1,22,0.1))
fittedpf <- predict(mpf1,newdata = dpf.pred, type = "response", se.fit = T)
dpf.pred <- dpf.pred %>% mutate(y = fittedpf$fit,
                                ymin = y-1.96*fittedpf$se.fit,
                                ymax = y+1.96*fittedpf$se.fit)
fig2C <- ggplot(dpf_mort,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = my_del_col) +
  geom_line(data = dpf.pred, color = my_del_col, size = 1.3) +
   geom_ribbon(data = dpf.pred, aes(ymin=ymin,ymax=ymax),
              alpha = 0.3) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,20.5),
                     breaks = seq(0,20,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.02),
                     breaks = seq(0,0.02,0.01)) +
  labs(x = "Age", y = "Mortality") +
  background_grid(major = "xy", minor = "y")
fig2C

rm(dpf_gam)

# read data (lat generation pop)
ddead<-read.table("dead_DEF.txt")
names(ddead)<-c("t","age","dam0","res0","dead")
ddead<- ddead %>% filter(t>4975)

## fig2D
dDEF_res <- ddead %>% group_by(age) %>% summarise(y = median(res0),
                                                 ymin = quantile(res0,0.2),
                                                 ymax = quantile(res0,0.8))
dDEF_res <- dDEF_res %>% filter(age <= 18)

fig2D <- ggplot(dDEF_res,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = "darkgreen") +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color = "darkgreen") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,14.5),
                     breaks = seq(0,14,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,4),
                     breaks = seq(0,4,1)) +
  labs(x = "Age", y = "Resources") +
  background_grid(major = "xy", minor = "y")

fig2D

## fig2E

dDEF_dam <- ddead %>% group_by(age) %>% summarise(y = median(dam0),
                                                 ymin = quantile(dam0,0.2),
                                                 ymax = quantile(dam0,0.8))
dDEF_dam <- dDEF_dam %>% filter(age <= 15)

fig2E <- ggplot(dDEF_dam,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = damage_fill) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color = damage_fill) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,14.5),
                     breaks = seq(0,14,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.25),
                     breaks = seq(0,1.25,0.5)) +
  labs(x = "Age", y = "Damage") +
  background_grid(major = "xy", minor = "y")

fig2E

## fig2F: mortality vs. age
dDEF_mort <- ddead %>% group_by(age) %>% summarise(y = mean(dead))

dDEF_mort <- dDEF_mort %>% filter(age <= 10)    

dDEF_gam <- ddead %>% filter(age <=10)
print("Starting gam 2")
#dDEF_gam <-dDEF_gam%>% filter(t >=4500)
rm(ddead)
mDEF1 <- gam(dead ~ s(age), family = binomial, data = dDEF_gam) 
print("Finished gam 2")
gam.check(mDEF1)
dDEF.pred <- data.frame(age=seq(1,10,0.1))
fittedDEF <- predict(mDEF1,newdata = dDEF.pred, type = "response", se.fit = T)
dDEF.pred <- dDEF.pred %>% mutate(y = fittedDEF$fit,
                                  ymin = y-1.96*fittedDEF$se.fit,
                                  ymax = y+1.96*fittedDEF$se.fit)
fig2F <- ggplot(dDEF_mort,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = my_del_col) +
  geom_line(data = dDEF.pred, color = my_del_col, size = 1.3) +
  geom_ribbon(data = dDEF.pred, aes(ymin=ymin,ymax=ymax),
              alpha = 0.3) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,14.5),
                     breaks = seq(0,14,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.003),
                     breaks = seq(0,0.003,0.001)) +
  labs(x = "Age", y = "Mortality") +
  background_grid(major = "xy", minor = "y")
fig2F

rm(dDEF_gam)

# read data (last generation pop)
ddead<-read.table("dead_GHI.txt")
names(ddead)<-c("t","age","dam0","res0","dead")
ddead<- ddead %>% filter(t>4975)

## fig2G
dGHI_res <- ddead %>% group_by(age) %>% summarise(y = median(res0),
                                                 ymin = quantile(res0,0.2),
                                                 ymax = quantile(res0,0.8))
dGHI_res <- dGHI_res %>% filter(age <= 18)

fig2G <- ggplot(dGHI_res,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = "darkgreen") +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color =  "darkgreen") +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,18.5),
                     breaks = seq(0,18,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,3),
                     breaks = seq(0,3,1)) +
  labs(x = "Age", y = "Resources") +
  background_grid(major = "xy", minor = "y")

fig2G

## fig2H

dGHI_dam <- ddead %>% group_by(age) %>% summarise(y = median(dam0),
                                                  ymin = quantile(dam0,0.2),
                                                  ymax = quantile(dam0,0.8))
dGHI_dam <- dGHI_dam %>% filter(age <= 18)

fig2H <- ggplot(dGHI_dam,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = damage_fill) +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),
                width=0,
                color = damage_fill) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,18.5),
                     breaks = seq(0,15,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,1.5),
                     breaks = seq(0,1.5,0.5)) +
  labs(x = "Age", y = "Damage") +
  background_grid(major = "xy", minor = "y")

fig2H

## fig2I: mortality vs. age
dGHI_mort <- ddead %>% group_by(age) %>% summarise(y = mean(dead))

dGHI_mort <- dGHI_mort %>% filter(age <= 10)    

dGHI_gam <- ddead %>% filter(age <=10)
print("Starting gam 3")

mGHI1 <- gam(dead ~ s(age), family = binomial, data = dGHI_gam) 
print("Finished gam 3")

gam.check(mGHI1)
dGHI.pred <- data.frame(age=seq(1,10,0.1))
fittedGHI <- predict(mGHI1,newdata = dGHI.pred, type = "response", se.fit = T)
dGHI.pred <- dGHI.pred %>% mutate(y = fittedGHI$fit,
                                  ymin = y-1.96*fittedGHI$se.fit,
                                  ymax = y+1.96*fittedGHI$se.fit)
fig2I <- ggplot(dGHI_mort,aes(age,y)) + theme_cowplot(my_font_size) +
  geom_point(size = 3.5, color = my_del_col) +
  geom_line(data = dGHI.pred, color = my_del_col, size = 1.3) +
  geom_ribbon(data = dGHI.pred, aes(ymin=ymin,ymax=ymax),
              alpha = 0.3) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(0,18.5),
                     breaks = seq(0,18,5)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.01),
                     breaks = seq(0,0.01,0.005)) +
  labs(x = "Age", y = "Mortality") +
  background_grid(major = "xy", minor = "y")
fig2I

rm(dGHI_gam)
rm(ddead)
save.image(file = "Figure2_gams_cloud_25points.RData")

plot_grid(fig2A, fig2B, fig2C, fig2D,  fig2E,fig2F, fig2G, fig2H,fig2I, 
          labels = c('A','B','C','D','E','F','G','H','I'), 
          label_size = 18,
          ncol = 3)
