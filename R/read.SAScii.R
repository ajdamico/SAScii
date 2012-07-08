#code from
#http://r.789695.n4.nabble.com/Capturing-warnings-with-capture-output-td912468.html
#thanks to Gabor Grothendieck

withWarnings <- function(expr) {
     wHandler <- function(w) {
      cat(w$message, "\n")
      invokeRestart("muffleWarning")
     }
     withCallingHandlers(expr, warning = wHandler)
} 


read.SAScii <- 
function( fn , sas_ri , beginline = 1 , buffersize = 50 , zipped = F , n = -1 , intervals.to.print = 1000 , lrecl = NULL){
#read.SAScii uses a smaller buffersize than the usual FWF default, to handle larger datasets

	x <- parse.SAScii( sas_ri , beginline , lrecl )
	
	#only the width field should include negatives
	y <- x[ !is.na( x[ , 'varname' ] ) , ]
	
	
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
		
		# read all columns in as character fields
		# (some will be converted to numeric later)
		SASfile.partial <- read.fwf( 
								fn , 
								x$width , 
								col.names=y$varname , 
								comment.char = '' , 
								colClasses = 'character' , 
								buffersize = buffersize , 
								n = lines.to.read ,
								skip = ( curStart )
							)

		SASfile <- rbind( SASfile , SASfile.partial )
		
		i <- i + 1
			
		cat( "  current progress: read.fwf has read in" , prettyNum( nrow( SASfile ) , big.mark = "," ) , "records" , "\r" )

	}
	
	# loop through all columns to:
		# convert to numeric where necessary
		# divide by the divisor whenever necessary
	for ( l in 1:nrow(y) ){
	
		# if the SAS input script says the current column should be numeric
		# convert it!
		if ( !y[ l , 'char' ] ){
		
			#handle NAs introduced by coercion warnings by..
			#capturing them
			op <-
				capture.output( 
					withWarnings(
						SASfile[ , l ] <- as.numeric( SASfile[ , l ] ) 
					)
				)
		
			#and then printing this NOTE to the console
			if( identical( op , "NAs introduced by coercion " ) ){
			
				problem.line <-
					readLines( sas_ri )[ grep( y[ l , 'varname' ] , readLines( sas_ri ) ) ]
				
				potential.fix <-
					gsub( 
						y[ l , 'varname' ] , 
						paste( 
							y[ l , 'varname' ] ,
							"$" 
						) ,
						problem.line
					)
				
				writeLines( 
					paste( 
						"NOTE: column" , 
						y[ l , 'varname' ] , 
						"either contains missings or character strings.\n" ,
						"if this column is numeric and contains only numbers and missings, no action is required.\n" ,
						"if this column actually contains character strings, then\n" ,
						"consider modifying the SAS input syntax file stored at\n" ,
						sas_ri , "\n" ,
						"to include a dollar sign ($) in the text below.\n\n" , 
						"here's a guess: try changing\n" ,
						problem.line ,
						"\n  to\n" ,
						potential.fix ,
						"\n\n"
					)
				)
			}
		}
	
		#are there any periods in the column already?
		no_decimal_points <- ( sum( grepl( "." , SASfile[ , l ] , fixed = T ) ) == 0 )
		
		if ( (y[ l , "divisor" ] != 1) & !(y[ l , "char" ]) & no_decimal_points ) SASfile[ , l ] <- SASfile[ , l ] * as.numeric( y[ l , "divisor" ] )
		
	}

	SASfile
}

