library(ChIPpeakAnno)
library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(org.Hs.eg.db)
#library(TxDb.Mmusculus.UCSC.mm10.knownGene)
#library(org.Mm.eg.db)

###


peaks <- toGRanges(paste0('H3K27me_HepG2.intersect_with_G4.bed'), format="BED")
n
peaks[1:2]

annoData <- toGRanges(TxDb.Hsapiens.UCSC.hg19.knownGene)
annoData[1:2]


anno <- annotatePeakInBatch(peaks, AnnotationData=annoData, 
                            output="overlapping", 
                            FeatureLocForDistance="TSS",
                            bindingRegion=c(-2000, 300))
data.frame(anno) %>% head()

anno$symbol <- xget(anno$feature, org.Hs.egSYMBOL)
data.frame(anno) %>% head()

anno_df <- data.frame(anno)
write.table(anno_df, file=paste0('H3K27me_HepG2.intersect_with_G4.bed.genes.txt'),
            col.names = TRUE, row.names = FALSE, sep = '\t', quote = FALSE)

uniq_genes_df <- unique(anno_df['symbol'])
write.table(uniq_genes_df, file=paste0('H3K27me_HepG2.intersect_with_G4.bed.genes_uniq.txt'),
            col.names = FALSE, row.names = FALSE, sep = '\t', quote = FALSE)