if(!file.exists('build.R')) {
	stop('Working directory not set correctly. Set the working directory to the location of 
		 of this R script.')
}

source('build-srl.R')
source('build-writing.R')
source('build-reading.R')
source('build-mathematics.R')

file.edit('build/SRL.json')
file.edit('build/Writing.json')
file.edit('build/Reading.json')
file.edit('build/Mathematics.json')
