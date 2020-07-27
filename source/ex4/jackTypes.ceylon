class Token(shared String name, shared String type) {
	shared String toXML {
		variable String nameXML = name.clone();
		if (name == "<") {nameXML = "&lt;";}
		if (name == ">") {nameXML = "&gt;";}
		if (name == "\\") {nameXML = "&quot;";}
		if (name == "&") {nameXML = "&amp;";}	
		return ("<``type``> ``nameXML`` </``type``>");
	}
}












/*
abstract class TokenType(shared String type) of keyw | symb | iConst | sConst | id {}

object keyw extends TokenType("keyword") {}
object symb extends TokenType("symbol") {}
object iConst extends TokenType("integerConstant") {}
object sConst extends TokenType("stringConstant") {}
object id extends TokenType("identifier") {}

class JackTypes() {
//	shared {String+} tokenType = {"keyword", "symbol", "integerConstant", "stringConstant", "identifier"};
	value keyword = set {"class", "constructor", "function", "method", "field", "static", "var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do", "if", "else", "while", "return"};
	value symbol = set {'{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~'};
}
*/