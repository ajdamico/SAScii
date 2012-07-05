read.SAScii <- 
function( fn , sas_ri , beginline = 1 , buffersize = 50 , zipped = F , n = -1 , intervals.to.print = 1000 , lrecl = NULL){
#read.SAScii uses a smaller buffersize than the usual FWF default, to handle larger datasets



	x <- parse.SAScii( sas_ri , beginline , lrecl )
	
	#only the width field should include negatives
	y <- subset( x , !is.na( varname ) )
	
	
	#if the ASCII file is stored in an archive, unpack it to a temporary file and run that through read.fwf instead.
	if ( zipped ){
		#create a temporary file and a temporary directory..
		tf <- tempfile() ; td <- tempdir()
		#download the CPS repwgts zipped file
		download.file( fn , tf , mode = "wb" )
		#unzip the file's contents and store the file name within the temporary directory
		fn <- unzip( tf , exdir = td , overwrite = T )
	}
	
	#initiate both full and partial data frames
	SASfile.partial <- SASfile <- data.frame()
	
	#start the counter
	i <- 1

	#run this read-in loop until there are no more records to read in
	while( i == 1 | nrow( SASfile.partial ) > 0 ){

		curStart <- intervals.to.print * ( i - 1 )
		
		#if not all the data records should be read in..
		if ( n != -1 ) {
			#read in the minimum of..
			lines.to.read <- 
				min( 
					#the number of records to be read in, minus the number of records already read in..
					n - nrow( SASfile ) , 
					#and the number of records in the next interval
					intervals.to.print
					)
		} else {
			lines.to.read <- intervals.to.print
		}
		
		#input actual SAS data text-delimited file to read in
		SASfile.partial <- read.fwf( 
								fn , 
								x$width , 
								col.names=y$varname , 
								comment.char = '' , 
								colClasses = ifelse( y$char , 'character' , 'numeric' ) , 
								buffersize = buffersize , 
								n = lines.to.read ,
								skip = ( curStart )
							)

		SASfile <- rbind( SASfile , SASfile.partial )
		
		i <- i + 1
			
		cat( "  current progress: read.fwf has read in" , prettyNum( nrow( SASfile ) , big.mark = "," ) , "records" , "\r" )

	}
							
	#divide by the divisor whenever necessary
	for ( l in 1:nrow(y) ){
	
		#are there any periods in the column already?
		no_decimal_points <- ( sum( grepl( "." , SASfile[ , l ] , fixed = T ) ) == 0 )
		
		if ( (y[ l , "divisor" ] != 1) & !(y[ l , "char" ]) & no_decimal_points ) SASfile[ , l ] <- SASfile[ , l ] * as.numeric( y[ l , "divisor" ] )
		
		#if ( y[ l , "char" ] ) SASfile[ , l ] <- str_trim( SASfile[ , l ] )
	}

	SASfile
}

