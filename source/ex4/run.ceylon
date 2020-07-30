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
			LinkedList<Token> tokensStream = jackTokenizer(file);
			
			Path outTokensFilePath = file.path.siblingPath(file.name.split('.'.equals).first + "T2.xml");
			if (is File|Nil loc = outTokensFilePath.resource){
				File outFile = createFileIfNil(loc);
				try (fileAppender = outFile.Appender()){
					fileAppender.writeLine("<tokens>");
					for (token in tokensStream) {
						fileAppender.writeLine(token.toXML);
					}
					fileAppender.writeLine("</tokens>");
				}
			}
			
			Path outParserFilePath = file.path.siblingPath(file.name.split('.'.equals).first + "2" + ".xml");
			if (is File|Nil locParser = outParserFilePath.resource){
				File vmFile = createFileIfNil(locParser);
				value parser = Parse(vmFile, tokensStream);
				parser.parseClass();
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
	buf.resize(4096);
	buf.clear();

	reader.read(buf);
	buf.flip();
	variable String bytesStream = ascii.decode(buf);
		
	while (bytesStream.size != 0) {
		
		if (getFirst(bytesStream) == '/') {
			if (getFirst(bytesStream.rest) == '/') {
				while (getFirst(bytesStream) != '\n') {
					bytesStream = bytesStream.rest;
				}
				bytesStream = bytesStream.rest;
			}
			else if (getFirst(bytesStream.rest) == '*') {
//				value spanIndex = bytesStream.firstInclusion("*/");
				assert (exists spanIndex = bytesStream.firstInclusion("*/"));
//				if (is Integer spanIndex) {
					bytesStream = bytesStream.spanFrom(spanIndex+2);
//				}
			}
			else {
//				builder.appendCharacter(getFirst(bytesStream));
				list.add(Token("".pad(1,getFirst(bytesStream)), "symbol"));
				bytesStream = bytesStream.rest;
//				builder.clear();
			}
		}
		
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
		
		else if (getFirst(bytesStream) in symbol) {
//			builder.appendCharacter(getFirst(bytesStream));
			list.add(Token("".pad(1,getFirst(bytesStream)), "symbol"));
			bytesStream = bytesStream.rest;
//			builder.clear();
		}
		
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

/*
shared Character getFirst(String s) {
		value c = s.first;
        if (is Null c) {
			return '@';
		}
		else {
			return c;
		}
}
*/

shared Character getFirst(String s) {
	assert (exists sFirst = s.first);
	return sFirst;
}

class Parse(File destinaton, LinkedList<Token> tokens) {
	variable Token currentToken = Token("emptyValue", "emptyType");
	variable String tab = "";
//	tree
	
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
	
	void writeTerminal() {
		try (dest = destinaton.Appender()) {	
			dest.writeLine(tab + currentToken.toXML);
		}
//		addtotree
	}

	void writeNonTerminalOpening(String name) {
		try (dest = destinaton.Appender()) {
			dest.writeLine(tab + "<" + name + ">");
		}
		increaseTabulation();
//		addtotree
	}
	
	void writeNonTerminalClosing(String name) {
		decreaseTabulation();
		try (dest = destinaton.Appender()) {
			dest.writeLine(tab + "</" + name + ">");
		}
	}
	
//must return Tree
	shared void parseClass() {
		writeNonTerminalOpening("class");
		getNextToken();
		writeTerminal(); //class
		getNextToken();
		writeTerminal(); //className
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
	
	void parseClassVarDec(){
		writeNonTerminalOpening("classVarDec");
		writeTerminal(); //static|field
		getNextToken();
		writeTerminal(); //type
		getNextToken();
		writeTerminal(); //varName
		getNextToken();
		while (currentToken.val == ",") {
			writeTerminal(); //,
			getNextToken();
			writeTerminal(); //varName
			getNextToken();		
		}
		writeTerminal(); //;
		writeNonTerminalClosing("classVarDec");		
	}
	
	void parseSubroutineDec() {
		writeNonTerminalOpening("subroutineDec");
		writeTerminal(); //constructor|function|method
		getNextToken();
		writeTerminal(); //void|type
		getNextToken();
		writeTerminal(); //subroutineName
		getNextToken();
		writeTerminal(); //(
		getNextToken();
		parseParameterList();
		writeTerminal(); //)
		getNextToken();
		parseSubroutineBody();
		writeNonTerminalClosing("subroutineDec");
	}
	
	void parseParameterList() {
		writeNonTerminalOpening("parameterList");
		if (currentToken.val != ")") {
			writeTerminal(); //type
			getNextToken();
			writeTerminal(); //varName
			getNextToken();
			while (currentToken.val == ",") {
				writeTerminal(); //,
				getNextToken(); 
				writeTerminal(); //type
				getNextToken();
				writeTerminal(); //varName
				getNextToken();	
			}
		}
		writeNonTerminalClosing("parameterList");
	}
	
	void parseSubroutineBody() {
		writeNonTerminalOpening("subroutineBody");
		writeTerminal(); //{
		getNextToken();
		while (currentToken.val == "var") {
			parseVarDec();
		}
		parseStatements();
		writeTerminal(); //}
		writeNonTerminalClosing("subroutineBody");
	}
	
	void parseVarDec() {
		writeNonTerminalOpening("varDec");
		writeTerminal(); //var
		getNextToken();
		writeTerminal(); //type
		getNextToken(); 
		writeTerminal(); //varName
		getNextToken();
		
		while(currentToken.val == ",") {
			writeTerminal(); //,
			getNextToken();
			writeTerminal(); //varName
			getNextToken();
		}
		writeTerminal(); //;
		getNextToken();
		writeNonTerminalClosing("varDec");
	}
	
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
		writeNonTerminalOpening("letStatement");
		writeTerminal(); //let
		getNextToken();
		writeTerminal(); //varName
		getNextToken();
		if (currentToken.val == "[") {
			writeTerminal(); //[
			getNextToken();
			parseExpression();
			writeTerminal(); //]
			getNextToken();
		}
		writeTerminal(); //=
		getNextToken();
		parseExpression();
		writeTerminal(); //;
		getNextToken();
		writeNonTerminalClosing("letStatement");
	}
	
	void parseIfStatement() {
		writeNonTerminalOpening("ifStatement");
		writeTerminal(); //if
		getNextToken();
		writeTerminal(); //(
		getNextToken();
		parseExpression();
		writeTerminal(); //)
		getNextToken();
		writeTerminal(); //{
		getNextToken();
		parseStatements();
		writeTerminal(); //}
		
		getNextToken();
		if (currentToken.val == "else") {
			writeTerminal(); //else
			getNextToken();
			writeTerminal(); //{
			getNextToken();
			parseStatements();
			writeTerminal(); //}
			getNextToken();
		}
		writeNonTerminalClosing("ifStatement");	
	}	
	
	void parseWhileStatement() {
		writeNonTerminalOpening("whileStatement");
		writeTerminal(); //while
		getNextToken();
		writeTerminal(); //(
		getNextToken();
		parseExpression();
		writeTerminal(); //)
		getNextToken();
		writeTerminal(); //{
		getNextToken();
		parseStatements();
		writeTerminal(); //}
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
			getNextToken();
			parseTerm();
			getNextToken();
		}
		writeNonTerminalClosing("expression");
	}
	
	void parseSubroutineCall() {
//		writeNonTerminalOpening("subroutineCall");
		writeTerminal(); //subroutineName|className|varName
		getNextToken();
		if (currentToken.val == ".") {
			writeTerminal(); //.
			getNextToken();
			writeTerminal(); //subroutineName
			getNextToken();
		}
		writeTerminal(); //(
		getNextToken();
		parseExpressionList();
		writeTerminal(); //)
//		writeNonTerminalClosing("subroutineCall");
	}
	
	void parseTerm() {
		writeNonTerminalOpening("term");
		if (currentToken.val in ["-", "~"]) {
			writeTerminal(); //unaryOp
			getNextToken();
			parseTerm();
		}
		else if (currentToken.val == "(") {
			writeTerminal(); //(
			getNextToken();
			parseExpression();
			writeTerminal(); //)
		}
		else if (currentToken.type in ["stringConstant", "integerConstant", "keyword"]) {
			writeTerminal(); //stringConstant|integerConstant|keyword
		}
		else {
			assert (exists secondToken = tokens[0]);
			if (secondToken.val in ["(", "."]) {
				parseSubroutineCall();
			}
			else {
				writeTerminal(); //varName
				if (secondToken.val == "[") {
					getNextToken();
					writeTerminal(); //[
					getNextToken();
					parseExpression();
					writeTerminal(); //]
				}
			}
		}
		writeNonTerminalClosing("term");
	}
	
	void parseExpressionList() {
		writeNonTerminalOpening("expressionList");
		if (currentToken.val != ")") {              //non-empty expressionList
			parseExpression();
			while (currentToken.val == ",") {
				writeTerminal(); //.
				getNextToken();
				parseExpression();
			}
		}
		writeNonTerminalClosing("expressionList");
	}
}	