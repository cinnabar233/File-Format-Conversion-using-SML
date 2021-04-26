fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
	let 
		exception emptyInputFile 

		exception UnevenFields of string

		val is = TextIO.openIn infilename

		val os = TextIO.openOut outfilename

		fun dfa( character , state , is , os , delim1 , delim2 , isempty, line , total , count) = 
			if state = 0 then    (* state = 0 represents the state in which the preceding character was not a '\' *)

				if character = NONE
				(* End of File is reached *)

				 then 
				 	if isempty then raise emptyInputFile  (*uncaught exception is raised *)

				 	else if  not(total = count) then  raise UnevenFields  ("Expected: " ^ Int.toString(total) ^ " fields, Present: " ^ Int.toString(count) ^ " fields on Line " ^ Int.toString(line+1))
				 	(* if in the last line total number of character does not match the one in the first line*)

				 	else (  TextIO.closeIn is ;  TextIO.closeOut os )

				else if valOf(character) = #"\\"
					    (* state transition occurs with no output to the file *)
						 then dfa(TextIO.input1 is , 1, is,os,delim1 , delim2 , false, line, total , count)

				else if valOf(character) = delim1 
					
					then( TextIO.output1(os,delim2); 

							if line = 0 then dfa(TextIO.input1 is , 0 , is,os,delim1 , delim2 , false, line , total+1, count+1)
							(* for every delimiter the count of number fo fields increases *)

							else dfa(TextIO.input1 is , 0 , is,os,delim1 , delim2 , false, line , total, count+1)
						)

				else if valOf(character) = delim2 

					then(

							TextIO.output1(os, #"\\");  TextIO.output1(os,delim2);
						

							dfa(TextIO.input1 is , 0 , is, os, delim1 , delim2 ,false,line, total , count )
						)

				else if valOf(character) = #"\n" 

					then if not(line=0) andalso not( total = count ) 
						
						then raise UnevenFields ("Expected: " ^ Int.toString(total) ^ " fields, Present: " ^ Int.toString(count) ^ " fields on Line " ^ Int.toString(line+1))
							(* at the end of line count and total are matched and proper exception is raised*)

					else (TextIO.output1(os, #"\n");  dfa(TextIO.input1 is , 0 , is, os, delim1 , delim2 , false, line+1 , total , 1 ))
						(* the count is initialized to 1 and line number is incremented *)

				else (TextIO.output1(os,valOf(character)); dfa(TextIO.input1 is , 0 , is, os ,  delim1 , delim2 , false, line , total , count))
							
			else

				if character = NONE then 

					if not(total = count) then  raise UnevenFields  ("Expected: " ^ Int.toString(total) ^ " fields, Present: " ^ Int.toString(count) ^ " fields on Line " ^ Int.toString(line+1))

					else  (TextIO.output1(os, #"\\") ; TextIO.closeIn is ; TextIO.closeOut os) 

				else if valOf(character) = delim1 

						then (TextIO.output1(os,delim1); dfa(TextIO.input1 is , 0 , is , os ,  delim1 , delim2 , false, line, total , count))

				else ( TextIO.output1(os, #"\\") ; dfa(character,0, is , os , delim1,delim2 , false, line, total , count) )


	in 

			dfa(TextIO.input1 is,0, is , os , delim1, delim2 ,true, 0 ,  1 , 1)

			handle UnevenFields msg => print(msg ^ "\n")
		
end;







fun csv2tsv(infilename , outfilename) = 
	convertDelimiters(infilename , #"," , outfilename , #"\t")

fun tsv2csv( infilename , outfilename) = 
	convertDelimiters(infilename , #"\t" , outfilename , #",")




fun convertNewlines( infilename , newline1 , outfilename , newline2 ) = 

	let 
		exception emptyInputFile

		val is = TextIO.openIn infilename

		val os = TextIO.openOut outfilename


		fun f( s , is , os , newline1 , newline2 ) = 

			if size(s) < size(newline1) then

				if TextIO.lookahead(is) = NONE then ( TextIO.output(os,s) ; TextIO.closeOut os)

				else f( s ^ str(valOf(TextIO.input1(is)))  , is , os , newline1 , newline2 )

			else 
				if s = newline1 
					then ( TextIO.output(os,newline2) ; 

						if TextIO.lookahead(is) = NONE then TextIO.closeOut os 

						else f( str( valOf(TextIO.input1(is)))  , is , os , newline1 , newline2 )
						)
				else 
					(
						TextIO.output(os, substring(s,0,1)) ; 

						f( substring(s,1,size(s)-1) , is , os , newline1 , newline2)
					)
	in
		if TextIO.lookahead(is) = NONE then raise emptyInputFile
		else f("" , is , os , newline1 , newline2)
end;
			

fun unix2dos(infilename,outfilename) = 
		convertNewlines(infilename , "\n" , outfilename , "\r\n")

fun dos2unix(infilename,outfilename) = 
		convertNewlines(infilename , "\r\n" , outfilename , "\n")




