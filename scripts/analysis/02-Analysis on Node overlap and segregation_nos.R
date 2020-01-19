####################################
#Node overlap and segregation - nos#
####################################

library(tidyverse)
library(nos)
library(ggpubr)
library(ggridges)


  #1. Import  final matrices
vir_ISmatrix_sp_std <- read.csv("data/vir_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mari_ISmatrix_sp_std <- read.csv("data/mari_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
nca_ISmatrix_sp_std <- read.csv("data/nca_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
haw_ISmatrix_sp_std <- read.csv("data/haw_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
mad_ISmatrix_sp_std <- read.csv("data/mad_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)
jap_ISmatrix_sp_std <- read.csv("data/jap_ISmatrix_sp_std_grp6.csv", sep=",", row.names = 1) %>%
  as.matrix(.)

#1.1. 0.25 threshold matrices
vir_0.25 <- vir_ISmatrix_sp_std * (vir_ISmatrix_sp_std > 0.25)
mari_0.25 <- mari_ISmatrix_sp_std * (mari_ISmatrix_sp_std > 0.25)
nca_0.25 <- nca_ISmatrix_sp_std * (nca_ISmatrix_sp_std > 0.25)
haw_0.25 <- haw_ISmatrix_sp_std * (haw_ISmatrix_sp_std > 0.25)
mad_0.25 <- mad_ISmatrix_sp_std * (mad_ISmatrix_sp_std > 0.25)
jap_0.25 <- jap_ISmatrix_sp_std * (jap_ISmatrix_sp_std > 0.25)


#1.2. 0.5 threshold matrices
vir_0.5 <- vir_ISmatrix_sp_std * (vir_ISmatrix_sp_std > 0.5)
mari_0.5 <- mari_ISmatrix_sp_std * (mari_ISmatrix_sp_std > 0.5)
nca_0.5 <- nca_ISmatrix_sp_std * (nca_ISmatrix_sp_std > 0.5)
haw_0.5 <- haw_ISmatrix_sp_std * (haw_ISmatrix_sp_std > 0.5)
mad_0.5 <- mad_ISmatrix_sp_std * (mad_ISmatrix_sp_std > 0.5)
jap_0.5 <- jap_ISmatrix_sp_std * (jap_ISmatrix_sp_std > 0.5)

  #2. Get the NOS measures for each food web
#West Indies
vir_NOS <- freqMat_2_edge(vir_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(vir_NOS)
vir_NOS_0.25 <- freqMat_2_edge(vir_0.25, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
vir_NOS_0.5 <- freqMat_2_edge(vir_0.5, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)

vir_NOS_full <- data_frame("value" = vir_NOS$ov_in,
           "nos" = "in") %>%
  rbind(data_frame("value" = vir_NOS$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0") %>%
rbind(data_frame("value" = vir_NOS_0.25$ov_in, #adding 0.25 data
                   "nos" = "in") %>%
  rbind(data_frame("value" = vir_NOS_0.25$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0.25")) %>%
rbind(data_frame("value" = vir_NOS_0.5$ov_in, #adding 0.5 data
                   "nos" = "in") %>%
  rbind(data_frame("value" = vir_NOS_0.5$ov_out,
                           "nos" = "out")) %>%
  mutate(threshold = "0.5")) %>%
  mutate(site = "vir")

#Marshall Islands
mari_NOS <- freqMat_2_edge(mari_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(mari_NOS)
mari_NOS_0.25 <- freqMat_2_edge(mari_0.25, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
mari_NOS_0.5 <- freqMat_2_edge(mari_0.5, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)

mari_NOS_full <- data_frame("value" = mari_NOS$ov_in,
                           "nos" = "in") %>%
  rbind(data_frame("value" = mari_NOS$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0") %>%
  rbind(data_frame("value" = mari_NOS_0.25$ov_in, #adding 0.25 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = mari_NOS_0.25$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.25")) %>%
  rbind(data_frame("value" = mari_NOS_0.5$ov_in, #adding 0.5 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = mari_NOS_0.5$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.5")) %>%
  mutate(site = "mari")

#Hawaii
haw_NOS <- freqMat_2_edge(haw_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(haw_NOS)
haw_NOS_0.25 <- freqMat_2_edge(haw_0.25, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
haw_NOS_0.5 <- freqMat_2_edge(haw_0.5, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)

haw_NOS_full <- data_frame("value" = haw_NOS$ov_in,
                           "nos" = "in") %>%
  rbind(data_frame("value" = haw_NOS$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0") %>%
  rbind(data_frame("value" = haw_NOS_0.25$ov_in, #adding 0.25 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = haw_NOS_0.25$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.25")) %>%
  rbind(data_frame("value" = haw_NOS_0.5$ov_in, #adding 0.5 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = haw_NOS_0.5$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.5")) %>%
  mutate(site = "haw")

#Madagascar
mad_NOS <- freqMat_2_edge(mad_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(mad_NOS)
mad_NOS_0.25 <- freqMat_2_edge(mad_0.25, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
mad_NOS_0.5 <- freqMat_2_edge(mad_0.5, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)

mad_NOS_full <- data_frame("value" = mad_NOS$ov_in,
                           "nos" = "in") %>%
  rbind(data_frame("value" = mad_NOS$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0") %>%
  rbind(data_frame("value" = mad_NOS_0.25$ov_in, #adding 0.25 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = mad_NOS_0.25$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.25")) %>%
  rbind(data_frame("value" = mad_NOS_0.5$ov_in, #adding 0.5 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = mad_NOS_0.5$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.5")) %>%
  mutate(site = "mad")

#New Caledonia
nca_NOS <- freqMat_2_edge(nca_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(nca_NOS)
nca_NOS_0.25 <- freqMat_2_edge(nca_0.25, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
nca_NOS_0.5 <- freqMat_2_edge(nca_0.5, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)

nca_NOS_full <- data_frame("value" = nca_NOS$ov_in,
                           "nos" = "in") %>%
  rbind(data_frame("value" = nca_NOS$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0") %>%
  rbind(data_frame("value" = nca_NOS_0.25$ov_in, #adding 0.25 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = nca_NOS_0.25$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.25")) %>%
  rbind(data_frame("value" = nca_NOS_0.5$ov_in, #adding 0.5 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = nca_NOS_0.5$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.5")) %>%
  mutate(site = "nca")

#Japan
jap_NOS <- freqMat_2_edge(jap_ISmatrix_sp_std, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
summary(jap_NOS)
jap_NOS_0.25 <- freqMat_2_edge(jap_0.25, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)
jap_NOS_0.5 <- freqMat_2_edge(jap_0.5, bip = TRUE, sp_nam = TRUE) %>%
  NOSM_bip(., perc = 1, sl = 0)

jap_NOS_full <- data_frame("value" = jap_NOS$ov_in,
                           "nos" = "in") %>%
  rbind(data_frame("value" = jap_NOS$ov_out,
                   "nos" = "out")) %>%
  mutate(threshold = "0") %>%
  rbind(data_frame("value" = jap_NOS_0.25$ov_in, #adding 0.25 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = jap_NOS_0.25$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.25")) %>%
  rbind(data_frame("value" = jap_NOS_0.5$ov_in, #adding 0.5 data
                   "nos" = "in") %>%
          rbind(data_frame("value" = jap_NOS_0.5$ov_out,
                           "nos" = "out")) %>%
          mutate(threshold = "0.5")) %>%
  mutate(site = "jap")


  #3. Plots 
#Density plots

#West Indies
virNOS_p <- ggplot(data = subset(vir_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4","black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "West Indies",
       y = "", 
       x = "NOS") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2)) +
  guides(color = F, fill =F) 
 

#Hawaii
hawNOS_p <- ggplot(data = subset(haw_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4","black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "Hawaii",
       y = "Density", 
       x = "") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2))+
        #plot.subtitle = element_text(size = 10, colour = "red", hjust = 0.5, face="bold")) +
  guides(color = F, fill =F) 

#Madagascar
madNOS_p <- ggplot(data = subset(mad_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4", "black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "Madagascar",
       y = "", 
       x = "") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2)) +
  guides(color = F, fill =F) 

#Marshall Islands
mariNOS_p <- ggplot(data = subset(mari_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4", "black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "Marshall Islands",
       y = "Density", 
       x = "") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2)) +
  guides(color = F, fill =F) 

#New Caledonia
ncaNOS_p <- ggplot(data = subset(nca_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(values = c("#67e6dc","darkslategray4", "black")) + #c("grey70", "grey35","black"))
  scale_color_manual(values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "New Caledonia",
       y = "", 
       x = "") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2)) +
  guides(color = F, fill =F) 

#Japan
japNOS_p <- ggplot(data = subset(jap_NOS_full, nos %in% "in"), mapping = aes(x = value, fill = threshold, color = threshold)) +
  geom_density(alpha = 0.1, size = 0.3, aes(y = ..density..)) +
  scale_fill_manual(name = "Interaction strength threshold", values = c("#67e6dc","darkslategray4", "black")) + #c("grey70", "grey35","black"))
  scale_color_manual(name = "Interaction strength threshold", values = c("#67e6dc","darkslategray4","black")) + #c("gold", "coral","darkorchid4")) #67e6dc
  scale_x_continuous(expand = c(0,0.05)) +
  labs(title = "Okinawa",
       y = "Density", 
       x = "NOS") +
  theme_pubr() +
  theme(plot.title = element_text(size = 6, colour = "black", face = "bold", hjust = 0.5),
        axis.text = element_text(size=6, colour = "black"),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.title = element_text(size=7),
        axis.line = element_line(colour = "black", size=0.2),
        legend.text = element_text(colour="black", size=6),
        legend.title = element_text(colour="black", size=6),
        legend.key.size = unit(3,"mm"),
        legend.title.align = 1.5)
 
#Combined plots

NOS_in.plot <- ggpubr::ggarrange(hawNOS_p, madNOS_p, mariNOS_p, ncaNOS_p, japNOS_p, virNOS_p,
                  ncol = 2, nrow = 3,
                  common.legend = TRUE, legend="bottom") 


ggsave("output/figures/fig.4 - NOS_in plot.pdf", plot = NOS_in.plot, width = 8.7, height = 12, units = "cm", dpi = 300)

