library(gdata)
library(tools)
library(rjson)
library(jsonlite)


##### Reading

domains <- c(ID = 'Ideas',
			 IN = 'Inference',
			 LA = 'Language',
			 PU = 'Purpose',
			 ST = 'Structure')

items <- read_excel('reading/ReadingItems.xlsx', sheet = 1)
items <- as.data.frame(items)

items$Domain <- as.character(factor(items$DomainID,
									levels = names(domains),
									labels = unname(domains)))
items$Month <- as.integer(factor(items$Month, #levels = c(1, 6, 8),
								 levels = c('January', 'June', 'August')))
table(items$Month)

for(i in 1:nrow(items)) {
	item <- list()
	item$source <- paste0('NY-', items[i,]$Year, '-', 
						  formatC(items[i,]$Month, width=2, flag='0'), '-',
						  formatC(items[i,]$ItemNum, width=2, flag='0'))
	item$domain <- items[i,]$Domain
	item$difficulty <- items[i,]$DifficultyLevel
	item$stem <- items[i,]$Stem
	item$A <- items[i,]$A
	item$B <- items[i,]$B
	item$C <- items[i,]$C
	item$D <- items[i,]$D
	item$answer <- items[i,]$Answer
	# item$feedback <- paste0(scan(paste0('mathematics/items/', items[i,]$Filename), 
	# 					  what = character(), sep = '\n', blank.lines.skip = FALSE, quiet = TRUE), 
	# 						collapse = '\n')
	item$feedbackfile <- items[i,]$Filename
	json.out <- jsonlite::toJSON(item, pretty = TRUE, auto_unbox = TRUE)
	cat(json.out, file = paste0('reading/items/', item$source, '.json'))
}

##### Mathematics

items <- read.xls('mathematics/source/MathItems.xlsx', sheet=1, stringsAsFactors=FALSE)

items[items$DifficultyLevel == '', ]$DifficultyLevel <- NA
items[items$Domain == '', ]$Domain <- NA
items$DifficultyLevel <- toupper(items$DifficultyLevel)
items$Domain <- tolower(gsub(' ', '_', items$Domain))
items$Filename <- paste0(items$State, '-', items$Year, '-',
						 formatC(items$Month, width=2, flag='0'), '-',
						 formatC(items$ItemNum, width=2, flag='0'), '.md')
items <- items[!is.na(items$Domain),]

for(i in 1:nrow(items)) {
	item <- list()
	item$source <- paste0(items[i,]$State, '-', items[i,]$Year, '-', 
						  formatC(items[i,]$Month, width=2, flag='0'), '-',
						  formatC(items[i,]$ItemNum, width=2, flag='0'))
	item$domain <- items[i,]$Domain
	item$difficulty <- items[i,]$DifficultyLevel
	item$stem <- items[i,]$Stem
	item$A <- items[i,]$A
	item$B <- items[i,]$B
	item$C <- items[i,]$C
	item$D <- items[i,]$D
	item$answer <- items[i,]$Answer
	# item$feedback <- paste0(scan(paste0('mathematics/items/', items[i,]$Filename), 
	# 					  what = character(), sep = '\n', blank.lines.skip = FALSE, quiet = TRUE), 
	# 						collapse = '\n')
	item$feedbackfile <- items[i,]$Filename
	json.out <- jsonlite::toJSON(item, pretty = TRUE, auto_unbox = TRUE)
	cat(json.out, file = paste0('mathematics/items/', item$source, '.json'))
}

file.edit(paste0('mathematics/items/', item$source, '.json'))

tmp <- fromJSON(paste0('mathematics/items/', item$source, '.json'))

