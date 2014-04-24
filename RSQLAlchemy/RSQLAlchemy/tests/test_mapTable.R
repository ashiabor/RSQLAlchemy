##TEST RSQLAlchemy

#setup
library("RSQLAlchemy")
library("RSQLite")

#TEST mapTable
testMapTable <- function() {
  
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
  
  SNPTEST <- snp(snp_id='SNPTEST', chr="1", all_A="T", all_B="C")
  gotResult <- SNPTEST
  
  checkEquals(gotResult$snp_id, 'SNPTEST')
  checkEquals(gotResult$chr, "1")
  checkEquals(gotResult$all_A, "T")
  checkEquals(gotResult$all_B, "C")
}

testMapTableAutoUpdate <- function() {
  
  snp<-mapTable(tableName="snp",
                columns='snp_id="character",
                        chr="character",
                        all_A="character",
                        all_B="character"');
  
  SNPTEST <- snp(snp_id='SNPTEST', chr="1", all_A="T", all_B="C")
  SNPTEST$snp_id<-"SNPCHANGE"
  gotResult <- SNPTEST
  
  checkEquals(gotResult$snp_id, 'SNPCHANGE')
  checkEquals(gotResult$chr, "1")
  checkEquals(gotResult$all_A, "T")
  checkEquals(gotResult$all_B, "C")
  
}

