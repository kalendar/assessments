# This R script will determine the difficulty level for reading items using Flesch Readability Ease
# metric. These difficulty estimates will inform DAACS' computer adaptive testing algorithm.

library(koRpus)

tree.tagger.dir <- '/Applications/tree-tagger/bin/tree-tagger-english'
passages.dir <- 'reading/passages/'

if(!file.exists(tree.tagger.dir)) {
	stop('Tree Tagger not found. See http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/')
}

set.kRp.env(TT.cmd = tree.tagger.dir, lang='en')
passages <- list.files(passages.dir, pattern = '.txt')

# Mapping of Flesch Readibilty Ease index to item difficulty category
difficulties <- data.frame(rbind(
	c('5', 'LOW'),
	c('6', 'LOW'),
	c('7', 'LOW'),
	c('8-9', 'MEDIUM'),
	c('>= 10 (high school)', 'HARD'),
	c('>= 13 (college)', 'HARD')
), stringsAsFactors = FALSE)
names(difficulties) <- c('Grade', 'Difficulty')
difficulties

readinglevels <- data.frame()
for(i in passages) {
	tag <- tokenize(paste0(passages.dir, i))
	f <- flesch(tag)
	readinglevels <- rbind(readinglevels, data.frame(
		Passage = i,
		RE = f@Flesch$RE,
		Grade = f@Flesch$grade
	))
}
readinglevels


readinglevels <- merge(readinglevels, difficulties, by = 'Grade', all.x = TRUE)
View(readinglevels[order(readinglevels$Passage),])

table(readinglevels$Grade, useNA='ifany')
table(readinglevels$Difficulty)

write.csv(readinglevels,
		  file = paste0(passages.dir, 'reading/ReadingDifficulty.csv'),
		  row.names = FALSE)
