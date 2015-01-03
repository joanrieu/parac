package net.fififox.ParaC;
import net.fififox.ParaC.ParaCLexer;
import net.fififox.ParaC.ParaCParser;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTreeWalker;


public class ParaCCompiler {

	public static void main(String[] args) {
		ANTLRFileStream input;
		try {
			input = new ANTLRFileStream(args[0]);
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}
		ParaCLexer lexer = new ParaCLexer(input);
		ParaCParser parser = new ParaCParser(new CommonTokenStream(lexer));
		ParserRuleContext tree = parser.program();
		ParseTreeWalker walker = new ParseTreeWalker();
		AssemblyGenerator assemblyGenerator = new AssemblyGenerator();
		walker.walk(assemblyGenerator, tree);
	}

}
