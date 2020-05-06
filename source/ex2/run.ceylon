import ceylon.file { ... }
//import ceylon.collection {MutableList}

class VMParseException(String message) extends ParseException(message) {}

shared void run() {
	String? userPath=process.arguments.first;
	if (exists userPath) {
		value path = parsePath(userPath);
		variable {File*} vmFiles = {};
		variable Path outFilePath;
		value bootstrapCode = 
"@256
								D=A
								@SP
								M=D
								``convertToAsm("call Sys.init 0".split(), 0, "bootstrap")``
								";							
								
		if (is Directory dir = path.resource) {
			vmFiles = dir.files("*.vm");
			if (vmFiles.empty) {print("No files to convert");}
			outFilePath = dir.path.childPath(getAsmFilename(dir));
		}
		else if (is File file = path.resource) {
			vmFiles = {file};		
			outFilePath = file.path.siblingPath(getAsmFilename(file));
		}
		else {
			throw Exception("File system error: ``path`` is not valid file or directory");
		}

		if (is File|Nil loc = outFilePath.resource){
			File outFile = createFileIfNil(loc);
			try (fileAppender = outFile.Appender()){
			
				fileAppender.writeLine(bootstrapCode);
				
				for (file in vmFiles) {				
					variable Integer lineNumber = 0;
					value filename = file.name.split('.'.equals).first;
					forEachLine(file, (String line) {
						lineNumber++;
						{String+} tokensStream = line.split();
						try {
							value convertedCommand = convertToAsm(tokensStream, lineNumber, filename);
							if (is String convertedCommand){
								if (convertedCommand != "") {
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
		}
		else {
			throw Exception("Output file error: ``outFilePath`` is not valid file name");
		}
	}
	else {
		throw Exception("Path it missing: path must be first argument");
	}
}


String getAsmFilename(File|Directory res){
	value path = res.path;
	value stringPath=path.string;
	value filename = if (is File res) then 			stringPath[stringPath.lastIndexOf("\\")+1..stringPath.lastIndexOf(".")-1] else stringPath[stringPath.lastIndexOf("\\")+1...];

	return filename+".asm";
}

String|VMParseException convertToAsm({String+} tokensStream, Integer lineNumber, String filename){

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
				return (if (firstToken=="push") then convertPush(segment, i, filename) else convertPop(segment, i, filename));
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
					@J_COMPARSION_``lineNumber``_``filename``
					";
					
		if (firstToken == "eq") {result +=	"D;JEQ\n";}
		if (firstToken == "lt") {result +=	"D;JLT\n";}
		if (firstToken == "gt") {result +=	"D;JGT\n";}
				
		result +=	
"@0
					D=A
					@COMPARSION_BRANCHING_END_``lineNumber``_``filename``
					1;JMP
					(J_COMPARSION_``lineNumber``_``filename``)
					@0
					A=A-1
					D=A
					(COMPARSION_BRANCHING_END_``lineNumber``_``filename``)
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
	
	case("call") {
		checkArgumentsAmount(tokensStream, 3);
		value functionName = tokensStream.rest.first;
		value nArgsString = tokensStream.rest.rest.first;
		if (is Null functionName) {throw VMParseException("illegal function name");}
		if (is Null nArgsString) {throw VMParseException("illegal nArgs parameter");}
		value pseudoPushA = 
"D=A
						@SP
						A=M
						M=D
						@SP
						M=M+1";
		value pseudoPushAContent = 
"D=M
						@SP
						A=M
						M=D
						@SP
						M=M+1";
		variable String result ="";				
		try {
			value nArgs = Integer.parse(nArgsString);
			if (is Integer nArgs) {
				if (nArgs < 0) {throw VMParseException("negative nArgs parameter");}
				
				result +=
"@retAddrLabel_``functionName``_``lineNumber``_``filename`` //push retAddrLabel
						``pseudoPushA``
						@LCL
						``pseudoPushAContent``
						@ARG
						``pseudoPushAContent``
						@THIS
						``pseudoPushAContent``
						@THAT
						``pseudoPushAContent``
						@SP //ARG=SP-5-nArgs
						D=M
						@5
						D=D-A
						@``nArgs``
						D=D-A
						@ARG
						M=D
						@SP //LCL=SP
						D=M
						@LCL
						M=D
						@``functionName``
						1; JMP
						(retAddrLabel_``functionName``_``lineNumber``_``filename``)
						";
			
			}
			else {throw VMParseException("error parsing nArgs to integer");}
		}
		catch (ParseException e) {
			throw VMParseException("error parsing nArgs to integer");
		}
		return result;
	}

	case("function") {
		checkArgumentsAmount(tokensStream, 3);
//		print(tokensStream);
		value functionName = tokensStream.rest.first;
//		print(functionName);
		value nVarsString = tokensStream.rest.rest.first;
//		print(nVarsString);
		if (is Null functionName) {throw VMParseException("illegal function name");}
		if (is Null nVarsString) {throw VMParseException("illegal nVars parameter");}

		variable String result ="";				
		try {
			value nVars = Integer.parse(nVarsString);
			if (is Integer nVars) {
				if (nVars < 0) {throw VMParseException("negative nVars parameter");}
				
				result +=
"(``functionName``)
						@``nVars``
						D=A
						(initLocal_``functionName``)
						@endInit_``functionName``
						D;JEQ
						@SP
						A=M
						M=0
						@SP
						M=M+1
						D=D-1
						@initLocal_``functionName``
						1;JMP
						(endInit_``functionName``)
						";
			
			}
			else {throw VMParseException("error parsing nVars to integer");}
		}
		catch (ParseException e) {
			throw VMParseException("error parsing nVars to integer");
		}
		return result;		
	}
	
	case("return"){
		checkArgumentsAmount(tokensStream, 1);
		return 
"@LCL //endFrame (R13) = LCL
				D=M
				@R13
				M=D
				@R13 // retAddr(R14) = *(endframe -5)
				A=M-1
				A=A-1
				A=A-1
				A=A-1
				A=A-1 
				D=M
				@R14
				M=D
				@SP //*ARG = pop()
				A=M-1
				D=M
				@ARG
				A=M
				M=D
				@ARG //SP=ARG+1
				D=M
				@SP
				M=D+1
				@R13 //restore THAT
				A=M-1
				D=M
				@THAT
				M=D
				@R13 //restore THIS
				A=M-1
				A=A-1
				D=M
				@THIS
				M=D
				@R13 //restore ARG
				A=M-1
				A=A-1
				A=A-1
				D=M
				@ARG
				M=D
				@R13 //restore LCL
				A=M-1
				A=A-1
				A=A-1
				A=A-1
				D=M
				@LCL
				M=D
				@R14 //goto retAddr
				A=M
				1;JMP
				";
	}
	
	else {
		throw VMParseException("unknown command");
	}
}

String|VMParseException convertPush(String? segment, Integer i, String filename){
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
"@``filename``.``i``
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

String|VMParseException convertPop(String? segment, Integer i, String filename){
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
				@``filename``.``i``
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