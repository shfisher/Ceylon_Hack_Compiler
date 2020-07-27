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
			{Token*} tokensStream = jackTokenizer(file);
			
			Path outTokensFilePath = file.path.siblingPath(file.name + "T.xml");
			if (is File|Nil loc = outTokensFilePath.resource){
				File outFile = createFileIfNil(loc);
				try (fileAppender = outFile.Appender()){
					for (token in tokensStream) {
						fileAppender.writeLine(token.toXML);
					}					
				}
			}
		}
	
	}
	else {
		throw Exception("Path it missing: path must be first argument");
	}
}		


{Token*} jackTokenizer(File file) {
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
		
		if (mockFirst(bytesStream) == '/') {
			if (mockFirst(bytesStream.rest) == '/') {
				while (mockFirst(bytesStream) != '\n') {
					bytesStream = bytesStream.rest;
				}
				bytesStream = bytesStream.rest;
			}
			else if (mockFirst(bytesStream.rest) == '*') {
				value spanIndex = bytesStream.firstInclusion("*/");
				if (is Integer spanIndex) {
					bytesStream = bytesStream.spanFrom(spanIndex+2);
				}
			}
		
		}
		
		if (mockFirst(bytesStream) == '"') {
			bytesStream = bytesStream.rest;

			while (mockFirst(bytesStream) != '"') {
				builder.appendCharacter(mockFirst(bytesStream));
				bytesStream = bytesStream.rest;
			}
			bytesStream = bytesStream.rest;
			list.add(Token(builder.string, "stringConstant"));
			builder.clear();
			
		}
		
		else if (mockFirst(bytesStream) in '0'..'9') {
			builder.appendCharacter(mockFirst(bytesStream));
			bytesStream = bytesStream.rest;
			
			while (mockFirst(bytesStream) in '0'..'9') {
				builder.appendCharacter(mockFirst(bytesStream));
				bytesStream = bytesStream.rest;
			}
			list.add(Token(builder.string, "integerConstant"));
			builder.clear();
			
		}
		
		else if (mockFirst(bytesStream) in symbol) {
			builder.appendCharacter(mockFirst(bytesStream));
			list.add(Token(builder.string, "symbol"));
			bytesStream = bytesStream.rest;
			builder.clear();
		}
		
		else if (mockFirst(bytesStream) in 'A'..'Z' || mockFirst(bytesStream) in 'a'..'z' || mockFirst(bytesStream) == '_') {
			builder.appendCharacter(mockFirst(bytesStream));
//				print(mockFirst(bytesStream));
//				print(builder.string);
			bytesStream = bytesStream.rest;
			
			while (mockFirst(bytesStream) in '0'..'9' || mockFirst(bytesStream) in 'A'..'Z' || mockFirst(bytesStream) in 'a'..'z' || mockFirst(bytesStream) == '_') {
				builder.appendCharacter(mockFirst(bytesStream));
//					print(mockFirst(bytesStream));
//					print(builder.string);
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


shared Character mockFirst(String s) {
		value c = s.first;
        if (is Null c) {
//            print("mock first: @");
			return '@';
		}
		else {
//			print("mock first: ``c``");
			return c;
		}
}