source('lib.R')

###
NAME <- 'H3K27me3_HepG2.ENCFF042EDV.h19.bed'
NAME <- 'H3K27me3_HepG2.ENCFF609FME.h19.bed'
OUT_DIR <- 'Results/'

###

bed_df <- read.delim(paste0(NAME), as.is = TRUE, header = FALSE)
colnames(bed_df) <- c('chrom', 'start', 'end', 'name', 'score')
bed_df$len <- bed_df$end - bed_df$start

bed_df <- bed_df %>%
  arrange(-len) %>%
  filter(len < 2000)
print(bed_df)

ggplot(bed_df) +
  aes(x = len) +
  geom_histogram() +
  ggtitle(NAME, subtitle = sprintf('Number of peaks = %s', nrow(bed_df))) +
  theme_bw()
ggsave(paste0('2000filter_peaks.', NAME, '.filtered.hist.pdf'), path = OUT_DIR)

bed_df %>%
  select(-len) %>%
  write.table(file=paste0(NAME ,'.filtered.bed'),
              col.names = FALSE, row.names = FALSE, sep = '\t', quote = FALSE)
