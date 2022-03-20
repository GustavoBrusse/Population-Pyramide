library(ggplot2)
require(gridExtra)
setwd("C:\\Users\\Gustavo\\Desktop\\DOUTORADO\\TESE\\ROTINAS")
ages_lon<-read.table("piramide_popgroup.txt",header = TRUE, sep='')
ages_lon2<-read.table("piramide_profamy.txt",header = TRUE, sep='')

ages_lon$base <-  0
ages_pyr <- ages_lon
ages_pyr$pct[ages_pyr$group == "MALE"] <- -ages_lon$pct[ages_lon$group == "MALE"]

ages_lon2$base <-  0
ages_pyr2 <- ages_lon2
ages_pyr2$pct[ages_pyr2$group == "MALE"] <- -ages_lon2$pct[ages_lon$group == "MALE"]

#### 2050 - POPGROUP

p <- ggplot(data = subset(ages_pyr, year == 2050),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
                 "#009E73", "#F0E442", "#D55E00", "#CC79A7")

p_pyr2050_popgroup <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  scale_fill_manual(values = bly_palette) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "HRM: 2050",
       fill = "Group") +
    coord_flip()

#### 2050 - POPFAMY

p <- ggplot(data = subset(ages_pyr2, year == 2050),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
                 "#009E73", "#F0E442", "#D55E00", "#CC79A7")

p_pyr2050_profamy <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  scale_fill_manual(values = bly_palette) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "ECCM: 2050",
       fill = "Group") +
  coord_flip()

#### 2030 - POPGROUP

p <- ggplot(data = subset(ages_pyr, year == 2030),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
                 "#009E73", "#F0E442", "#D55E00", "#CC79A7")

p_pyr2030_popgroup <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  scale_fill_manual(values = bly_palette) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "HRM: 2030",
       fill = "Group") +
   coord_flip()

#### 2030 - POPFAMY

p <- ggplot(data = subset(ages_pyr2, year == 2030),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

bly_palette <- c("#E69F00", "#0072B2", "#000000", "#56B4E9",
                 "#009E73", "#F0E442", "#D55E00", "#CC79A7")

p_pyr2030_profamy <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  scale_fill_manual(values = bly_palette) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "ECCM: 2030",
       fill = "Group") +
  coord_flip()
##### 2010 - POPGROUP

p <- ggplot(data = subset(ages_pyr, year == 2010),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

p_pyr2010_popgroup <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
  scale_x_continuous(breaks = seq(0, 90, 10)) +
  scale_fill_manual(values = bly_palette) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "HRM: 2010",
       fill = "Group") +
  coord_flip()

##### 2010 - ProFamy

p <- ggplot(data = subset(ages_pyr2, year == 2010),
            mapping = aes(x = age, ymin = base,
                          ymax = pct, fill = group))

p_pyr2010_profamy <- p + geom_ribbon(alpha = 0.5) +
  scale_y_continuous(labels = abs, limits = max(ages_lon$pct, na.rm = TRUE) * c(-1,1)) +
  scale_x_continuous(breaks = seq(10, 80, 10)) +
  scale_fill_manual(values = bly_palette) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "Age", y = "Percent of Population",
       title = "ECCM: 2010",
       fill = "Group") +
  coord_flip()

grid.arrange(p_pyr2010_popgroup, p_pyr2010_profamy, p_pyr2030_popgroup, p_pyr2030_profamy,p_pyr2050_popgroup, p_pyr2050_profamy, ncol=2)

