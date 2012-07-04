read.SAScii <- 
function( fn , sas_ri , beginline = 1 , buffersize = 50 , zipped = F , n = -1 ){
#read.SAScii uses a smaller buffersize than the usual FWF default, to handle larger datasets



	x <- parse.SAScii( sas_ri , beginline )
	
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
		
	#input actual SAS data text-delimited file to read in
	SASfile <- read.fwf( 
							fn , 
							x$width , 
							col.names=y$varname , 
							comment.char = '' , 
							colClasses = ifelse( y$char , 'character' , 'numeric' ) , 
							buffersize = buffersize , 
							n = n
						)

	#divide by the divisor whenever necessary
	for ( l in 1:nrow(y) ){
	
		#are there any periods in the column already?
		no_decimal_points <- ( sum( grepl( "." , SASfile[ , l ] , fixed = T ) ) == 0 )
		
		if ( (y[ l , "divisor" ] != 1) & !(y[ l , "char" ]) & no_decimal_points ) SASfile[ , l ] <- SASfile[ , l ] * as.numeric( y[ l , "divisor" ] )
		
		if ( y[ l , "char" ] ) SASfile[ , l ] <- str_trim( SASfile[ , l ] )
	}

	SASfile
}

