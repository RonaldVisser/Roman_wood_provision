library(readxl)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(pastecs)
library(ggpubr)

# read data
nl_west <- read_excel("data/NL_West.xlsx")
struct <- read_excel("data/Structuur_EN_DE.xlsx")
# directory for export
dir.create("export", showWarnings = FALSE)

felling_age_hist <- ggplot(nl_west, aes(x=FellingAge)) + geom_histogram(binwidth = 20, color="darkblue", fill="lightblue") +
  xlab("Tree age (years)") + ylab("Frequency")
ggsave("export/nl_west_treeage_histogram.png",felling_age_hist, width = 12, height = 8)
ggplot(nl_west, aes(x=FellingAge)) + geom_histogram(binwidth = 20, color="darkblue", fill="lightblue") +
  xlab("Baumalter (Jahren)") + ylab("Häufigkeit")
ggsave("export/nl_west_baumalter_histogram.png", width = 12, height = 8)

ggplot(nl_west, aes(x=FellingAge)) + geom_density()
#ggplot(nl_west, aes(x=Leeftijd)) + geom_boxplot()

boxplot(nl_west$FellingAge)

stat.desc(nl_west$FellingAge)

struct_freq <- as.data.frame(table(nl_west$Structure))
colnames(struct_freq) <-c("English", "freq")
struct_freq <- merge(struct_freq,struct,by="English")
colourCount = length(struct_freq[,1])
getPalette = colorRampPalette(brewer.pal(colourCount, "Paired"))
ggplot(struct_freq, aes(x="",y=freq,fill=Deutsch)) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") + 
  theme(axis.text.x=element_blank()) + theme_void() + theme(strip.text = element_text(size = 5)) +
  scale_fill_manual(values = getPalette(colourCount), labels = paste0(struct_freq$Deutsch, " (", struct_freq$freq, ")")) +
  guides(fill=guide_legend(title="Kontext"))  
  #geom_text(aes(label = freq), position = position_stack(vjust = 0.5),size=3)
ggsave("export/nl_west_kontext_pie.png", width = 8, height = 8)  

context_pie <- ggplot(struct_freq, aes(x="",y=freq,fill=English)) +
  geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y") + 
  theme(axis.text.x=element_blank()) + theme_void() + theme(strip.text = element_text(size = 5)) +
  scale_fill_manual(values = getPalette(colourCount), labels = paste0(struct_freq$English, " (", struct_freq$freq, ")")) +
  guides(fill=guide_legend(title="Context"))  
#geom_text(aes(label = freq), position = position_stack(vjust = 0.5),size=3)
ggsave("export/nl_west_context_pie.png", context_pie, width = 8, height = 8)  

ggplot(nl_west, aes(x=FellingDate)) + geom_histogram(binwidth = 25, color="darkblue", fill="lightblue") + 
  xlab("Felling Date") + ylab("Count")
ggsave("export/nl_west_felling_histogram.png", width = 12, height = 8)  

felling_dates <- ggplot(nl_west, aes(x=FellingDate, y=FellingAge, colour=Structure)) + geom_point() +
  scale_colour_manual(values = getPalette(colourCount)) +
  xlab("Felling year") + ylab ("Felling Age") 
ggsave("export/nl_west_felling_dates_age_scatter.png", felling_dates, width = 12, height = 8)  


ggarrange(felling_dates,                                                 
          ggarrange(felling_age_hist, context_pie, ncol = 2, labels = c("B", "C")), 
          nrow = 2, labels = "A") 
ggsave("export/nl_west_combined.png", dpi = 300, width = 12, height = 12)  
