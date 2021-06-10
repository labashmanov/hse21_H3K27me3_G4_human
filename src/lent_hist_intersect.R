source('lib.R')

###
NAME <- 'H3K27me_HepG2.intersect_with_G4.bed'
OUT_DIR <- 'Results/'
library(ggplot2)
###Histogram of lengths

bed_df <- read.delim(paste0(NAME), as.is = TRUE, header = FALSE)
colnames(bed_df) <- c('chrom', 'start', 'end', 'name', 'score')
bed_df$len <- bed_df$end - bed_df$start

ggplot(bed_df) +
  aes(x = len) +
  geom_histogram() +
  ggtitle(NAME, subtitle = sprintf('Number of peaks = %s', nrow(bed_df))) +
  theme_bw()
ggsave(paste0('filter_peaks.', NAME, '.filtered.hist.pdf'), path = OUT_DIR)