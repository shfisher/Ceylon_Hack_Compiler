import ceylon.file { ... }
import ceylon.collection {LinkedList}
import ceylon.buffer {ByteBuffer}
import ceylon.buffer.charset {...}
import ceylon.io {newOpenFile}

shared void run() {
	String? userPath=process.arguments.first;
	if (exists userPath) {
		value path = parsePath(userPath);
		variable {File*} jackFiles = {};
								
		if (is Directory dir = path.resource) {
			jackFiles = dir.files("*.jack"); 
			if (jackFiles.empty) {print("No files to convert");}
		}
		else if (is File file = path.resource) {
			jackFiles = {file};		
		}
		else {
			throw Exception("File system error: ``path`` is not valid file or directory");
		}
		
		for (file in jackFiles) {
			//tokenizing
			LinkedList<Token> tokensStream = jackTokenizer(file);
			
			Path outTokensFilePath = file.path.siblingPath(file.name.split('.'.equals).first + "T2.xml");
			if (is File|Nil loc = outTokensFilePath.resource){
				File outFile = createFileIfNil(loc);
				try (fileAppender = outFile.Appender()){
					fileAppender.writeLine("<tokens>");
			//tokenizer output to .XML
					for (token in tokensStream) {
						fileAppender.writeLine(token.toXML);
					}
					fileAppender.writeLine("</tokens>");
				}
			}
			
			Path outParserFilePath = file.path.siblingPath(file.name.split('.'.equals).first + "2" + ".xml");
			Path vmFilePath = file.path.siblingPath(file.name.split('.'.equals).first + ".vm");
			if (is File|Nil locParser = outParserFilePath.resource){
				File xmlFile = createFileIfNil(locParser);
				if (is File|Nil locParserVM = vmFilePath.resource) {
					File vmFile = createFileIfNil(locParserVM);
			//parser output to .XML and .vm		
					value parser = Parse(xmlFile, vmFile, tokensStream);
					parser.parseClass();
				}
			}
		}
	}
	else {
		throw Exception("Path it missing: path must be first argument");
	}
}		


LinkedList<Token> jackTokenizer(File file) {
	value keyword = set {"class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return"};
	value symbol = set {'{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~'};

	value builder = StringBuilder();
	value list = LinkedList<Token>{};
	value reader = newOpenFile(file);
	value buf = ByteBuffer({});
	buf.resize(16384);
	buf.clear();

	reader.read(buf);
	buf.flip();
	variable String bytesStream = ascii.decode(buf);
		
	while (bytesStream.size != 0) {
	
//skip comments	
		if (getFirst(bytesStream) == '/') {
			if (getFirst(bytesStream.rest) == '/') {
				while (getFirst(bytesStream) != '\n') {
					bytesStream = bytesStream.rest;
				}
				bytesStream = bytesStream.rest;
			}
			else if (getFirst(bytesStream.rest) == '*') {
				assert (exists spanIndex = bytesStream.firstInclusion("*/"));
				bytesStream = bytesStream.spanFrom(spanIndex+2);
			}
//one backslash without * is a symbol
			else {
				list.add(Token("".pad(1,getFirst(bytesStream)), "symbol"));
				bytesStream = bytesStream.rest;
			}
		}

//string constant		
		else if (getFirst(bytesStream) == '"') {
			bytesStream = bytesStream.rest;

			while (getFirst(bytesStream) != '"') {
				builder.appendCharacter(getFirst(bytesStream));
				bytesStream = bytesStream.rest;
			}
			bytesStream = bytesStream.rest;
			list.add(Token(builder.string, "stringConstant"));
			builder.clear();
			
		}

//integer constant		
		else if (getFirst(bytesStream) in '0'..'9') {
			builder.appendCharacter(getFirst(bytesStream));
			bytesStream = bytesStream.rest;
			
			while (getFirst(bytesStream) in '0'..'9') {
				builder.appendCharacter(getFirst(bytesStream));
				bytesStream = bytesStream.rest;
			}
			list.add(Token(builder.string, "integerConstant"));
			builder.clear();
			
		}

//symbol		
		else if (getFirst(bytesStream) in symbol) {
			list.add(Token("".pad(1,getFirst(bytesStream)), "symbol"));
			bytesStream = bytesStream.rest;
		}

//identifier or keyword		
		else if (getFirst(bytesStream) in 'A'..'Z' || getFirst(bytesStream) in 'a'..'z' || getFirst(bytesStream) == '_') {
			builder.appendCharacter(getFirst(bytesStream));
			bytesStream = bytesStream.rest;
			
			while (getFirst(bytesStream) in '0'..'9' || getFirst(bytesStream) in 'A'..'Z' || getFirst(bytesStream) in 'a'..'z' || getFirst(bytesStream) == '_') {
				builder.appendCharacter(getFirst(bytesStream));
				bytesStream = bytesStream.rest;
			}
			if (builder.string in keyword) {
				list.add(Token(builder.string, "keyword"));
			}
			else {
				list.add(Token(builder.string, "identifier"));
			}
			builder.clear();
		}
		else {
			bytesStream = bytesStream.rest;
		}
	}
	return list;
}

//due to errorless input, emulates String.first with not NULL Character return
shared Character getFirst(String s) {
	assert (exists sFirst = s.first);
	return sFirst;
}

class Parse(File destinatonXML, File destinationVM, LinkedList<Token> tokens) {
	variable Token currentToken = Token("emptyValue", "emptyType");
//xml tabulator
	variable String tab = "";

	value classLevelSymbolTable = LinkedList<SymbolRecord>{};
	value subLevelSymbolTable = LinkedList<SymbolRecord>{};
	variable Integer fieldCounter = -1;
	variable Integer staticCounter = -1; 
	variable Integer argumentCounter = -1;
	variable Integer localCounter = -1;	
	variable String className = "";
	variable String currentRoutineName = "";
	variable Integer ifCounter = 0;
	variable Integer whileCounter = 0;
	
//search and return variable in symbol table, force not null return due to errorless input
	SymbolRecord getSymbolRecord(String pName) {
		value subLevelRecord = subLevelSymbolTable.find((SymbolRecord record) => record.name == pName);
		if (is SymbolRecord subLevelRecord) {
			return subLevelRecord;
		}
		assert (exists classLevelRecord = classLevelSymbolTable.find((SymbolRecord record) => record.name == pName));
		return classLevelRecord;
	}

//the same, but return may be null	
	SymbolRecord? getSymbolRecordIfExists(String pName) {
		value subLevelRecord = subLevelSymbolTable.find((SymbolRecord record) => record.name == pName);
		if (is SymbolRecord subLevelRecord) {
			return subLevelRecord;
		}
		return classLevelSymbolTable.find((SymbolRecord record) => record.name == pName);		
	}

//if token stream not empty, pop first token from stream to class-scope "currentToken" token variable, return true if succeeded
	Boolean getNextToken(){
		Token? tempToken = tokens.delete(0);
		if (is Token tempToken) {
			currentToken = Token(tempToken.val, tempToken.type);
		}
		return if (is Null tempToken) then false else true;
	}
	
	void increaseTabulation() {
		tab = tab + "  ";
	}
	
	void decreaseTabulation() {
		tab = tab[0..(tab.size-3)];
		if (tab.size == 1) {
			tab = "";
		}
	}

//put terminal value to XML (parser output)	
	void writeTerminal() {
		try (dest = destinatonXML.Appender()) {	
			dest.writeLine(tab + currentToken.toXML);
		}
	}

//the same for non-terminal
	void writeNonTerminalOpening(String name) {
		try (dest = destinatonXML.Appender()) {
			dest.writeLine(tab + "<" + name + ">");
		}
		increaseTabulation();
	}
	
	void writeNonTerminalClosing(String name) {
		decreaseTabulation();
		try (dest = destinatonXML.Appender()) {
			dest.writeLine(tab + "</" + name + ">");
		}
	}

//put line to .vm file (compiler output)	
	void writeLineVM(String line) {
		try (dest = destinationVM.Appender()) {
			dest.writeLine(line);
		}
	}		

//parser start function
//EXTRA TABULATION FOR COMPILER PART	
	shared void parseClass() {
		writeNonTerminalOpening("class");
		getNextToken();
		writeTerminal(); //class
		getNextToken();
		writeTerminal(); //className
//store class name for subroutine call
			className = currentToken.val;
		getNextToken();
		writeTerminal(); // {

		getNextToken();
		while (currentToken.val in ["static", "field"]) {
			parseClassVarDec();
			getNextToken();
		}
		while (currentToken.val in ["constructor", "function", "method"]) {
			parseSubroutineDec();
			getNextToken();
		}
		writeTerminal(); // }
		writeNonTerminalClosing("class");
	}

//class variables declaration
	void parseClassVarDec(){
		writeNonTerminalOpening("classVarDec");
		writeTerminal(); //static|field
//store static or field variable
			value kind = currentToken.val;
		getNextToken();
		writeTerminal(); //type
//variable type (integer, string, ...), can be useful in case of "class" type for subroutine call
			value type = currentToken.val;
		getNextToken();
		writeTerminal(); //varName
//symbol table main identifier
			variable String name = currentToken.val;
			if (kind == "static") {
				staticCounter++;
				classLevelSymbolTable.add(SymbolRecord(name, type, kind, staticCounter));
			}
			else {
				fieldCounter++;
				classLevelSymbolTable.add(SymbolRecord(name, type, "this", fieldCounter));
print("added clst: ``currentToken.val`` ``type`` this ``fieldCounter``");
			}
		getNextToken();
		while (currentToken.val == ",") {
			writeTerminal(); //,
			getNextToken();
			writeTerminal(); //varName
				name = currentToken.val;
				if (kind == "static") {
					staticCounter++;
					classLevelSymbolTable.add(SymbolRecord(name, type, kind, staticCounter));
				}
				else {
					fieldCounter++;
					classLevelSymbolTable.add(SymbolRecord(name, type, "this", fieldCounter));
print("added clst: ``currentToken.val`` ``type`` this ``fieldCounter``");
				}
			getNextToken();		
		}
		writeTerminal(); //;
		writeNonTerminalClosing("classVarDec");		
	}

//subroutine declaration	
	void parseSubroutineDec() {
		writeNonTerminalOpening("subroutineDec");
		writeTerminal(); //constructor|function|method
			value routineKind = currentToken.val;
//in method class object must be first argument
			if (routineKind == "method"){
				argumentCounter++;
				subLevelSymbolTable.add(SymbolRecord("this", className, "argument", argumentCounter));
			}
		getNextToken();
		writeTerminal(); //void|type
		getNextToken();
		writeTerminal(); //subroutineName
//store subrotine name for function call
			currentRoutineName = currentToken.val;
		getNextToken();
		writeTerminal(); //(
		getNextToken();
		parseParameterList();
		writeTerminal(); //)
		getNextToken();
//execute local variables parsing and counting
		parseSubroutineBody1();
			writeLineVM("function ``className``.``currentRoutineName`` ``localCounter+1``");
//constructor callee, allocate memory for local variables and return object address
			if (routineKind == "constructor") {
				writeLineVM("push constant ``fieldCounter+1``");
				writeLineVM("call Memory.alloc 1");
				writeLineVM("pop pointer 0");
			}
//in case of method, push current object address
			else if (routineKind == "method") {
				writeLineVM("push argument 0");
				writeLineVM("pop pointer 0");
			}
		parseSubroutineBody2();
		writeNonTerminalClosing("subroutineDec");

//reset sublevel counters and symbol table		
		argumentCounter = -1;
		localCounter = -1;
		subLevelSymbolTable.clear();
	}

//function arguments
	void parseParameterList() {
		writeNonTerminalOpening("parameterList");
		if (currentToken.val != ")") {
			writeTerminal(); //type
				variable String type = currentToken.val;
			getNextToken();
			writeTerminal(); //varName
				argumentCounter++;
				subLevelSymbolTable.add(SymbolRecord(currentToken.val, type, "argument", argumentCounter));				
			getNextToken();
			while (currentToken.val == ",") {
				writeTerminal(); //,
				getNextToken(); 
				writeTerminal(); //type
					type = currentToken.val;
				getNextToken();
				writeTerminal(); //varName
					argumentCounter++;
					subLevelSymbolTable.add(SymbolRecord(currentToken.val, type, "argument", argumentCounter));	
				getNextToken();	
			}
		}
		writeNonTerminalClosing("parameterList");
	}

//local variables record and count	
	void parseSubroutineBody1() {
		writeNonTerminalOpening("subroutineBody");
		writeTerminal(); //{
		getNextToken();
		while (currentToken.val == "var") {
			parseVarDec();
		}
	}

//body	
	void parseSubroutineBody2() {
		parseStatements();
		writeTerminal(); //}
		writeNonTerminalClosing("subroutineBody");
	}

//local variables	
	void parseVarDec() {
		writeNonTerminalOpening("varDec");
		writeTerminal(); //var
		getNextToken();
		writeTerminal(); //type
			value type = currentToken.val;
		getNextToken(); 
		writeTerminal(); //varName
			localCounter++;
			subLevelSymbolTable.add(SymbolRecord(currentToken.val, type, "local", localCounter));
print("added slst: ``currentToken.val`` ``type`` local ``localCounter``");
		getNextToken();
		
		while(currentToken.val == ",") {
			writeTerminal(); //,
			getNextToken();
			writeTerminal(); //varName
				localCounter++;
				subLevelSymbolTable.add(SymbolRecord(currentToken.val, type, "local", localCounter));
print("added slst: ``currentToken.val`` ``type`` local ``localCounter``");
			getNextToken();
		}
		writeTerminal(); //;
		getNextToken();
		writeNonTerminalClosing("varDec");
	}
	
//switch one of 5 statements
	void parseStatements() {
		writeNonTerminalOpening("statements");
		while (currentToken.val in ["let", "if", "while", "do", "return"]) {
			switch(currentToken.val)
			case("let") {parseLetStatement();}
			case("if") {parseIfStatement();}
			case("while") {parseWhileStatement();}
			case("do") {parseDoStatement();}
			case("return") {parseReturnStatement();}
			else {}
		}
		writeNonTerminalClosing("statements");
	}
	
	void parseLetStatement() {
			variable Boolean isArray = false;
		writeNonTerminalOpening("letStatement");
		writeTerminal(); //let
		getNextToken();
		writeTerminal(); //varName
			value symbol = getSymbolRecord(currentToken.val);
		getNextToken();
		if (currentToken.val == "[") {
//if assigned value is array element, store array name
				writeLineVM("push ``symbol.kind`` ``symbol.index``");
			writeTerminal(); //[
			getNextToken();
			parseExpression();
//then push index (in parseExpression) and calculate offset
				isArray = true;				
				writeLineVM("add");
			writeTerminal(); //]
			getNextToken();
		}
		writeTerminal(); //=
		getNextToken();
		parseExpression();
		writeTerminal(); //;
//if calculated expression is array too, it uses that 0, so if assigned value is array, we must to use that 0 safely via temp
			if (isArray) {
				writeLineVM("pop temp 0");
				writeLineVM("pop pointer 1");
				writeLineVM("push temp 0");
				writeLineVM("pop that 0");
			}
//usual assigning
			else {
				writeLineVM("pop ``symbol.kind`` ``symbol.index``");
			}
		getNextToken();
		writeNonTerminalClosing("letStatement");
	}
	
	void parseIfStatement() {
//unique label index
//must be stored, because parseIF body may consist inner ifStatements
			value storedIfCounter = ifCounter;
			ifCounter = ifCounter+1;
//print("if entered ifIndex = ``ifCounter`` stored = ``storedIfCounter``");
		writeNonTerminalOpening("ifStatement");
		writeTerminal(); //if
		getNextToken();
		writeTerminal(); //(
		getNextToken();
		parseExpression();
			writeLineVM("if-goto IF_TRUE``storedIfCounter``");
			writeLineVM("goto IF_FALSE``storedIfCounter``");
			writeLineVM("label IF_TRUE``storedIfCounter``");
		writeTerminal(); //)
		getNextToken();
		writeTerminal(); //{			
		getNextToken();
		parseStatements();
//print("if statements leave, ifIndex = ``ifCounter`` stored = ``storedIfCounter``");			
		writeTerminal(); //}			
		getNextToken();
		if (currentToken.val == "else") {
				writeLineVM("goto IF_END``storedIfCounter``");
				writeLineVM("label IF_FALSE``storedIfCounter``");
			writeTerminal(); //else
			getNextToken();
			writeTerminal(); //{
			getNextToken();
			parseStatements();
			writeTerminal(); //}
			getNextToken();
				writeLineVM("label IF_END``storedIfCounter``");
		}
			else {
				writeLineVM("label IF_FALSE``storedIfCounter``");
			}
		writeNonTerminalClosing("ifStatement");	
	}	
	
	void parseWhileStatement() {
//unique label index
			value storedWhileCounter = whileCounter;
			whileCounter = whileCounter+1;
//print("while entered whileIndex = ``whileCounter`` stored = ``storedWhileCounter``");
		writeNonTerminalOpening("whileStatement");
		writeTerminal(); //while
		getNextToken();
		writeTerminal(); //(
			writeLineVM("label WHILE_EXP``storedWhileCounter``");
		getNextToken();
		parseExpression();
		writeTerminal(); //)
			writeLineVM("not");
			writeLineVM("if-goto WHILE_END``storedWhileCounter``");
		getNextToken();
		writeTerminal(); //{
		getNextToken();
		parseStatements();
//print("while statements leave whileIndex = ``whileCounter`` stored = ``storedWhileCounter``");
		writeTerminal(); //}
			writeLineVM("goto WHILE_EXP``storedWhileCounter``");
			writeLineVM("label WHILE_END``storedWhileCounter``");
		getNextToken();
		writeNonTerminalClosing("whileStatement");
	}
	
	void parseDoStatement() {
		writeNonTerminalOpening("doStatement");
		writeTerminal(); //do
		getNextToken();
		parseSubroutineCall();
		getNextToken();
		writeTerminal(); //;
//void call must discard returned value
			writeLineVM("pop temp 0");
		getNextToken();
		writeNonTerminalClosing("doStatement");
	}
	
	void parseReturnStatement() {
		writeNonTerminalOpening("returnStatement");
		writeTerminal(); //return
		getNextToken();
		if (currentToken.val != ";") {
			parseExpression();
		}
//void return must return 0 by contract
			else {
				writeLineVM("push constant 0");
			}
			writeLineVM("return");
		writeTerminal(); //;
		getNextToken();
		writeNonTerminalClosing("returnStatement");
	}
	
	void parseExpression() {
		writeNonTerminalOpening("expression");
		parseTerm();
		getNextToken();
		while (currentToken.val in ["+", "-", "*", "/", "&", "|", ">", "<", "="]) {
			writeTerminal(); //op
//save OP for postfix (stack oriented) notation
				value op = currentToken.val;
			getNextToken();
			parseTerm();
			getNextToken();
//handle exp1 op exp2 
				switch (op)
				case ("+") {writeLineVM("add");}
				case ("-") {writeLineVM("sub");}
				case ("*") {writeLineVM("call Math.multiply 2");}
				case ("/") {writeLineVM("call Math.divide 2");}
				case ("&") {writeLineVM("and");}
				case ("|") {writeLineVM("or");}
				case (">") {writeLineVM("gt");}
				case ("<") {writeLineVM("lt");}
				case ("=") {writeLineVM("eq");}
				else {}
				
		}
		writeNonTerminalClosing("expression");
	}
	
	void parseSubroutineCall() {
//		writeNonTerminalOpening("subroutineCall");
		writeTerminal(); //subroutineName|className|varName
			variable String funcName = currentToken.val;
//nArgs store amount of caller arguments
			variable Integer nArgs = 0;
		getNextToken();
		if (currentToken.val == ".") {
			writeTerminal(); //.
//if caller is inner method (exists in symbol table), write it's object as first argument
				if (is SymbolRecord record = getSymbolRecordIfExists(funcName)) {
					writeLineVM("push ``record.kind`` ``record.index``");
					nArgs++;
					funcName = record.type;					
				}
			getNextToken();
			writeTerminal(); //subroutineName
				funcName += "." + currentToken.val;
			getNextToken();
		}
//this class method, push "this" as first argument
		else {
			funcName = className + "." + funcName;
				writeLineVM("push pointer 0");
				nArgs++;
		}
		writeTerminal(); //(
		getNextToken();
		nArgs += parseExpressionList();
		writeTerminal(); //)
			writeLineVM("call ``funcName`` ``nArgs``");
//		writeNonTerminalClosing("subroutineCall");
	}

//main expression parser	
	void parseTerm() {
		writeNonTerminalOpening("term");
		if (currentToken.val in ["-", "~"]) {
			writeTerminal(); //unaryOp
				value unaryOp = currentToken.val;
			getNextToken();
			parseTerm();
//handle "op exp" (postfix)
				if (unaryOp == "-") {
					writeLineVM("neg");
				}
				else {
					writeLineVM("not");
				}
		}
		else if (currentToken.val == "(") {
			writeTerminal(); //(
			getNextToken();
			parseExpression();
			writeTerminal(); //)
		}
		else if (currentToken.type in ["stringConstant", "integerConstant", "keyword"]) {
			writeTerminal(); //stringConstant|integerConstant|keyword
			if (currentToken.type == "stringConstant") {
//handle string constant
				writeLineVM("push constant ``(currentToken.val).size``");
				writeLineVM("call String.new 1");
				variable String stringToCopy = currentToken.val;
				while(is Character char = stringToCopy.first){
					writeLineVM("push constant ``char.integer``");
					writeLineVM("call String.appendChar 2");
					stringToCopy = stringToCopy.rest;
				}
			}
			else if (currentToken.type == "integerConstant") {
//handle push n
				writeLineVM("push constant ``currentToken.val``");
			}
//handle keyword constants
			else if (currentToken.val == "false") { 
				writeLineVM("push constant 0");
			}
			else if (currentToken.val == "null") { 
				writeLineVM("push constant 0");
			}
			else if (currentToken.val == "true") {
				writeLineVM("push constant 0");
				writeLineVM("not");			
			}
			else if (currentToken.val == "this") {
				writeLineVM("push pointer 0");			
			}
		}
		else {
//LL1 lookahead
			assert (exists secondToken = tokens[0]);
			if (secondToken.val in ["(", "."]) {
				parseSubroutineCall();
			}
			else {
				writeTerminal(); //varName
					value record = getSymbolRecord(currentToken.val);
					writeLineVM("push ``record.kind`` ``record.index``");
//in case of array, write it via that 0
				if (secondToken.val == "[") {
					getNextToken();
					writeTerminal(); //[
					getNextToken();
					parseExpression();
						writeLineVM("add");
						writeLineVM("pop pointer 1");
						writeLineVM("push that 0");
					writeTerminal(); //]
				}
			}
		}
		writeNonTerminalClosing("term");
	}

//push and count arguments in subroutine call	
	Integer parseExpressionList() {
		writeNonTerminalOpening("expressionList");
			variable Integer nArgs = 0;
		if (currentToken.val != ")") {              //non-empty expressionList
			parseExpression();
				nArgs++;
			while (currentToken.val == ",") {
				writeTerminal(); //.
				getNextToken();
				parseExpression();
					nArgs++;
			}
		}
		writeNonTerminalClosing("expressionList");
		return nArgs;
	}
}	