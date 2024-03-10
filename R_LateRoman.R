library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(pastecs)
library(ggpubr)


# read data
lateroman <- read_excel("data/late_roman.xlsx")
# directory for export
dir.create("export", showWarnings = FALSE)

felling_age_hist <- ggplot(lateroman, aes(x=FellingAge)) + geom_histogram(binwidth = 10, color="darkblue", fill="lightblue") +
  xlab("Tree age (years)") + ylab("Frequency")
ggsave("export/lateroman_treeage_histogram.png", felling_age_hist, dpi = 300, width = 12, height = 8) 
ggplot(lateroman, aes(x=FellingAge)) + geom_histogram(binwidth = 10, color="darkblue", fill="lightblue") +
  xlab("Baumalter (Jahren)") + ylab("Häufigkeit")
ggsave("export/lateroman_baumalter_histogram.png", dpi = 300, width = 12, height = 8)

ggplot(lateroman, aes(x=FellingAge)) + geom_density()

#ggplot(lateroman, aes(x=FellingAge)) + geom_boxplot()

stat.desc(lateroman$FellingAge)

struct_freq <- as.data.frame(table(lateroman$Structure))
colnames(struct_freq) <-c("English", "freq")
colourCount = length(struct_freq[,1])
getPalette = colorRampPalette(brewer.pal(colourCount, "Set1"))

context_pie <- ggplot(struct_freq, aes(x="",y=freq,fill=English)) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") + 
  theme(axis.text.x=element_blank()) + theme_void() + theme(strip.text = element_text(size = 5)) +
  scale_fill_manual(values = getPalette(colourCount), labels = paste0(struct_freq$English, " (", struct_freq$freq, ")")) +
  guides(fill=guide_legend(title="Context"))  
#geom_text(aes(label = freq), position = position_stack(vjust = 0.5),size=3)
ggsave("export/lateroman_context_pie.png", context_pie, dpi = 300, width = 8, height = 6) 


felling_dates <- ggplot(lateroman, aes(y=FellingAge,x=FellingDate)) + geom_point(aes(color=Structure, shape=Place), size =2) +
  scale_colour_manual(values = getPalette(colourCount)) +
  xlab("Felling year") + ylab ("Felling Age") #+ geom_smooth(method = "lm") 
ggsave("export/lateroman_felling_dates_age_scatter.png", felling_dates, dpi = 300, width = 12, height = 8)  

ggplot(lateroman, aes(x=FellingDate)) + geom_histogram(binwidth = 5, color="darkblue", fill="lightblue") +
  xlab("Felling year") + ylab ("Count")
ggsave("export/lateroman_felling_histogram.png", dpi = 300, width = 12, height = 8) 

ggarrange(felling_dates,                                                 
          ggarrange(felling_age_hist, context_pie, ncol = 2, labels = c("B", "C")), 
          nrow = 2, labels = "A") 
ggsave("export/lateroman_combined.png", dpi = 300, width = 12, height = 12)  

