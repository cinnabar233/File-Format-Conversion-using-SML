(*Author: Himanshu Gaurav Singh 2019CS10358*)
(*convDelim encloses every field in double quotes*)

fun convertDelimiters(infilename, delim1 , outfilename, delim2) = 
	let 
		open TextIO

		exception emptyInputFile

		exception UnevenFields of int*int*int

		exception InvalidRecord(*checks whether the field is invalid*)

		exception LastRecordDoesNotEndWithEOL

		val is = openIn infilename

		val os = openOut outfilename 

		fun f(character,flag,line,total,count) = 


			 if flag = 0 then (*flag = 0  denotes that we are scanning a field that is originally not enclosed within double quotes  *)

				if character = NONE then raise LastRecordDoesNotEndWithEOL 

				(*when the character is delimiter or a newline , the character is printed with double quoutes on both sides of it, but not when the newline is
					followed by  EOF*)

				else if valOf(character) = delim1 then 

				(output(os,"\"") ; output(os, str(delim2)) ; output(os , "\"") ;  

					if(line=0) then f(input1(is) , 0, line,total+1,count+1) else f(input1(is),0,line,total,count+1))  

				else if valOf(character) = #"\"" then  f(input1(is) ,1,line,total,count)   

				else if valOf(character) = #"\n" then 

					if not (total = count) then raise UnevenFields(line,total,count)

					else (  output(os,"\"") ;output(os,"\n") ; 

						if (lookahead(is) = NONE) then  (closeIn is ; closeOut os)  

						else ( output(os,"\""); f(input1(is),0,line+1,total,1) ) 

						)

				else (output1(os, valOf(character)) ; f( input1(is) , 0 ,line , total , count ))


			else  (*flag = 0  denotes that we are scanning a field that is originally  enclosed within double quotes  *)

				if character = NONE then raise InvalidRecord  

				else if valOf(character) = #"\"" then


					if(lookahead(is) = NONE orelse valOf(lookahead(is)) = #"\n" orelse valOf(lookahead(is)) = delim1) then f(input1(is), 0 , line , total , count)

					else if valOf(lookahead(is)) = #"\"" then(output(os,"\"\""); input1(is) ; f(input1(is) , 1 , line , total , count ))

					else raise InvalidRecord

				else ( output1(os, valOf(character)) ; f(input1(is) , 1 , line , total , count))

	in 
		if lookahead(is) = NONE then raise emptyInputFile else (output(os,"\"") ; f(input1(is) ,0,0,1,1))

		handle UnevenFields (line,total,count) => print("Expected: " ^ Int.toString(total) ^ " fields, Present: " ^ Int.toString(count) ^ " fields on Line " ^ Int.toString(line+1)^"\n") 


		
end;


fun csv2tsv(infilename,outfilename) = convertDelimiters(infilename,#",",outfilename,#"\t")
fun tsv2csv(infilename,outfilename) = convertDelimiters(infilename,#"\t",outfilename,#",")




