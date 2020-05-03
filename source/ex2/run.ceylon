//unique labels for eq lt gt
import ceylon.file { ... }

class VMParseException(String message) extends ParseException(message) {}

shared void run() {
	value userPath="D:/ceylon/ex1/testfiles/StackArithmetic/StackTest";
	value path = parsePath(userPath);
	if (is Directory dir = path.resource) {
		{File*} vmFiles = dir.files("*.vm");
		
		if (vmFiles.empty) {print("No files to convert");}
		
		for (file in vmFiles) {
			value outFilePath = file.path.siblingPath(getAsmFilename(file.path));
			if (is File|Nil loc = outFilePath.resource){
				File outFile = createFileIfNil(loc);
				try (fileAppender = outFile.Appender()){
					variable Integer lineNumber = 0;
					forEachLine(file, (String line) {
						lineNumber++;
						{String+} tokensStream = line.split();
						try {
							value convertedCommand = convertToAsm(tokensStream, lineNumber);
							if (is String convertedCommand){
								if (convertedCommand != "") {
									convertedCommand.replace("filename", file.name.split('.'.equals).first);
									fileAppender.writeLine(convertedCommand);
								}
							}
						}
						catch (VMParseException e){
							throw Exception("Illegal command in line ``lineNumber``, file ``file.name``\n" + e.message);
						}						
					});
				}
			}
			else {
				throw Exception("Output file error: ``outFilePath`` is not valid file name");
			}
		}
	}
	else {
		throw Exception("File system error: ``path`` is not valid directory");
	}
}

String getAsmFilename(Path path){
	value stringPath=path.string;
	value filename=stringPath[stringPath.lastIndexOf("\\")+1..stringPath.lastIndexOf(".")-1];
	return filename+".asm";
}

String|VMParseException convertToAsm({String+} tokensStream, Integer lineNumber){

	String? firstToken = tokensStream.first;
	if (is Null firstToken) {return "";}
//	print(firstToken);
//	print(firstToken=="//");
//	if (firstToken == "//") {return "";}
	
	
	switch(firstToken)
	case("//" | ""){
		return "";
	}
	
	case("push"|"pop") {
		checkArgumentsAmount(tokensStream, 3); 
		try {
			value thirdArgument=tokensStream.rest.rest.first;
			if (is Null thirdArgument) {throw VMParseException("third argument error");}
			value i = Integer.parse(thirdArgument);
			value segment=(tokensStream.rest).first;
			
			if (is Integer i) {
				if (i<0) {throw VMParseException("negative third argument");}
				return (if (firstToken=="push") then convertPush(segment, i) else convertPop(segment, i));
			}
			else {throw VMParseException("illegal third argument");}
		
		}
		catch (ParseException e){
			//return Null; //throws VMParseException third argument
			throw VMParseException("illegal third argument");
		}		
	}
	
	case("add" | "sub" | "and" | "or"){
		checkArgumentsAmount(tokensStream, 1);
		variable String result = 
								"@SP		//set SP on first argument 
								 M=M-1
								 M=M-1
								 A=M		//D=first argument
								 D=M
								 A=A+1 	//set A contains SP of second argument, it is M
								 ";
		switch(firstToken)
		case("add") {result += "D=D+M\n";}
		case("sub") {result += "D=D-M\n";}
		case("and") {result += "D=D&M\n";}
		case("or")  {result += "D=D|M\n";}
		else {}
		result += 
					"A=A-1  //store result on first argument place
					 M=D
					 D=A+1	//restore SP point on second argument place
					 @SP
					 M=D
					 ";
		return result;			
	}
	
	case("neg" | "not") {
		checkArgumentsAmount(tokensStream, 1);
		variable String result=
"@SP
				A=M-1
				";
		result += (if (firstToken=="neg") then "M=-M\n" else "M=!M\n");
		return result;
	}
	
	case("eq" | "lt" | "gt") {
		checkArgumentsAmount(tokensStream, 1);
		variable String result="";
		result =		
"@SP
					M=M-1
					M=M-1
					A=M
					D=M
					A=A+1
					D=D-M
					@J_COMPARSION_``lineNumber``
					";
					
		if (firstToken == "eq") {result +=	"D;JEQ\n";}
		if (firstToken == "lt") {result +=	"D;JLT\n";}
		if (firstToken == "gt") {result +=	"D;JGT\n";}
				
		result +=	
"@0
					D=A
					@COMPARSION_BRANCHING_END_``lineNumber``
					1;JMP
					(J_COMPARSION_``lineNumber``)
					@0
					A=A-1
					D=A
					(COMPARSION_BRANCHING_END_``lineNumber``)
					@SP
					A=M
					M=D
					@SP
					M=M+1
					";
			
		return result;
	}
	
	case("goto") {
		checkArgumentsAmount(tokensStream, 2);
		String? label = tokensStream.rest.first;
		if (is Null label) {throw VMParseException("illegal label");}

		return 
"@``label``
				1; JMP
				";
	}
	
	case("if-goto") {
		checkArgumentsAmount(tokensStream, 2);
		String? label = tokensStream.rest.first;
		if (is Null label) {throw VMParseException("illegal label");}

		return 
"@SP
				M=M-1
				A=M
				D=M
				@``label``
				D;JNE
				";
	}
	
	case("label") {
		checkArgumentsAmount(tokensStream, 2);
		String? label = tokensStream.rest.first;
		if (is Null label) {throw VMParseException("illegal label");}
		
		return "(``label``)\n";
	}
		
		
	
	else {
		throw VMParseException("unknown command");
	}
}

String|VMParseException convertPush(String? segment, Integer i){
	if (is Null segment) {throw VMParseException("wrong segment");}
	switch(segment)
	case("constant"){
		value result = 
					"@``i``		//D=i
					 D=A
					 @SP		//*SP=D
					 A=M
					 M=D
					 @SP		//SP++
					 M=M+1
					 ";
		return result;
	}
	
	case("local" | "argument" | "this" | "that" | "temp") {
		variable String result = "";
		switch(segment)
		case("local") {result +="@LCL\n";}
		case("argument") {result +="@ARG\n";}
		case("this") {result +="@THIS\n";}
		case("that") {result +="@THAT\n";}
		case("temp") {
			if (i>8) {throw VMParseException("wrong argument range");}
			result +="@5\n";
		}
		else{}
		result += (if (segment == "temp") then "D=A\n" else "D=M\n");
		result +=
"@``i``
						A=D+A
						D=M
						@SP
						A=M
						M=D
						@SP
						M=M+1
						";
		return result;
	}       
	
	case("static"){
		if (i>240) {throw VMParseException("wrong argument range");}
		return 
"@filename.``i``
				D=M
				@SP
				A=M
				M=D
				@SP
				M=M+1
				";
	}
	
	case("pointer") {
		if (i>1) {throw VMParseException("wrong argument range");}
		variable String result ="";
		result += (if (i==0) then "@THIS\n" else "@THAT\n");
		result += 
"D=M
					@SP
					A=M
					M=D
					@SP
					M=M+1
					";
		return result;
	}				
	
	else {throw VMParseException("wrong segment");}
}

String|VMParseException convertPop(String? segment, Integer i){
	if (is Null segment) {throw VMParseException("wrong segment");}
	switch(segment)
	case("local" | "argument" | "this" | "that" | "temp"){
		
		variable String result = "";
		switch(segment)
		case("local") {result +="@LCL\n";}
		case("argument") {result +="@ARG\n";}
		case("this") {result +="@THIS\n";}
		case("that") {result +="@THAT\n";}
		case("temp") {
			if (i>8) {throw VMParseException("wrong argument range");}
			result +="@5\n";
		}
		else{}
		result += (if (segment == "temp") then "D=A\n" else "D=M\n");
		result +=
"@``i``
				D=D+A
				@R13 //13 = (LCL+i)
				M=D
				@SP 	//D=*(SP--)
				M=M-1
				A=M
				D=M
				@R13
				A=M
				M=D
				";	
		return result;
	}
	
	case("static"){
		if (i>240) {throw VMParseException("wrong argument range");}
		return 
"@SP
				M=M-1
				A=M
				D=M
				@filename.``i``
				M=D
				";
	}
	
	case("pointer") {
		if (i>1) {throw VMParseException("wrong argument range");}
		variable String result =
"@SP
								M=M-1
								A=M
								D=M
								";
		result += (if (i==0) then "@THIS\n" else "@THAT\n");
		result += 
"M=D
					";
		return result;
	}
	
	else {throw VMParseException("wrong segment");}
}

Boolean|VMParseException checkArgumentsAmount({String+} tokensStream, Integer amount){
	variable Boolean result = true;
	if (tokensStream.size != amount) {
		result = false;
		if (tokensStream.size > amount) {
			for(i->token in tokensStream.indexed) {
//				print("``i``: ``token``");
				if (i == amount) {
//					print(token.startsWith("//"));
					if (token.startsWith("//") || token=="\n" || token=="") {
						result = true;
					}
				}
			}
		}
	}
	if (!result) {
		throw VMParseException("wrong arguments amount");
	}
	return result;
}