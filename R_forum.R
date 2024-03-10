library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(pastecs)
library(ggpubr)


# read data
forum <- read_excel("data/forumhadriani.xlsx")
# directory for export
dir.create("export", showWarnings = FALSE)


ggplot(forum, aes(x=FellingAge)) + geom_histogram(binwidth = 20, color="darkblue", fill="lightblue") +
  xlab("Tree age (years)") + ylab("Frequency")
ggsave("export/forum_treeage_histogram.png", width = 12, height = 8)

felling_age_hist <- ggplot(forum, aes(x=FellingAge)) + geom_histogram(binwidth = 20, color="darkblue", fill="lightblue") +
  xlab("Tree age (years)") + ylab("Frequency") + facet_grid(~Provenance)
ggsave("export/forum_treeage_region_histogram.png", felling_age_hist, width = 12, height = 8)

ggplot(forum, aes(x=FellingAge)) + geom_density()

#ggplot(forum, aes(x=FellingAge)) + geom_boxplot()

stat.desc(forum$FellingAge)

prov_freq <- as.data.frame(table(forum$Provenance))
colnames(prov_freq) <-c("English", "freq")
colourCount = length(prov_freq[,1])
getPalette = colorRampPalette(brewer.pal(colourCount, "Set1"))


provenance_pie <- ggplot(prov_freq, aes(x="",y=freq,fill=English)) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") + 
  theme(axis.text.x=element_blank()) + theme_void() + theme(strip.text = element_text(size = 5)) +
  scale_fill_manual(values = getPalette(colourCount), labels = paste0(prov_freq$English, " (", prov_freq$freq, ")")) +
  guides(fill=guide_legend(title="Context"))  
#geom_text(aes(label = freq), position = position_stack(vjust = 0.5),size=3)
ggsave("export/forum_provenance_pie.png", provenance_pie, dpi = 300, width = 8, height = 6) 


felling_dates <- ggplot(forum, aes(y=FellingAge,x=FellingDate, color=Provenance)) + geom_point() +
  scale_colour_manual(values = getPalette(3)) +
  xlab("Felling year") + ylab ("Felling Age") 
ggsave("export/forum_felling_dates_age_scatter.png", felling_dates, width = 12, height = 8)  

ggplot(forum, aes(x=FellingDate)) + geom_histogram(binwidth = 25, color="darkblue", fill="lightblue") +
  xlab("Felling year") + ylab ("Count")
ggsave("export/forum_felling_histogram.png", width = 12, height = 8)  

ggarrange(felling_dates,                                                 
          ggarrange(felling_age_hist, provenance_pie, ncol = 2, labels = c("B", "C")), 
          nrow = 2, labels = "A") 
ggsave("export/forum_combined.png", dpi = 300, width = 12, height = 12)  

