package net.fififox.ParaC;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class ParaCCompiler {

	public static void main(String[] args) throws Exception {
		ANTLRFileStream input;
		input = new ANTLRFileStream(args[0]);
		ParaCLexer lexer = new ParaCLexer(input);
		ParaCParser parser = new ParaCParser(new CommonTokenStream(lexer));
		ParserRuleContext tree = parser.program();
		ParseTreeWalker walker = new ParseTreeWalker();
		AssemblyGenerator assemblyGenerator = new AssemblyGenerator();
		walker.walk(assemblyGenerator, tree);
	}

}
