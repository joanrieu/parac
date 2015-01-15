package net.fififox.ParaC;

import java.util.ArrayList;
import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import net.fififox.ParaC.ParaCParser.BinaryExpressionContext;
import net.fififox.ParaC.ParaCParser.CompoundStatementContext;
import net.fififox.ParaC.ParaCParser.DeclarationContext;
import net.fififox.ParaC.ParaCParser.DeclaratorContext;
import net.fififox.ParaC.ParaCParser.ExpressionContext;
import net.fififox.ParaC.ParaCParser.ExpressionWithAssignmentContext;
import net.fififox.ParaC.ParaCParser.FunctionDefinitionContext;
import net.fififox.ParaC.ParaCParser.IterationStatementContext;
import net.fififox.ParaC.ParaCParser.JumpStatementContext;
import net.fififox.ParaC.ParaCParser.ParallelIterationStatementContext;
import net.fififox.ParaC.ParaCParser.ParameterDeclarationContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithCallContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithExpressionContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithFloatContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithIdentifierContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithIntegerContext;
import net.fififox.ParaC.ParaCParser.ProgramContext;
import net.fififox.ParaC.ParaCParser.SelectionStatementContext;
import net.fififox.ParaC.ParaCParser.StatementContext;
import net.fififox.ParaC.ParaCParser.TypeNameContext;
import net.fififox.ParaC.ParaCParser.UnaryExpressionContext;

import org.antlr.v4.runtime.ANTLRFileStream;
import org.antlr.v4.runtime.BailErrorStrategy;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

// http://unixwiz.net/techtips/win32-callconv-asm.html
// https://en.wikipedia.org/wiki/X86_calling_conventions#cdecl
public class ParaCCompiler extends ParaCBaseListener {

	private enum Type {
		INT, FLOAT, INT_POINTER, FLOAT_POINTER, INT_ARRAY, FLOAT_ARRAY;
	}

	abstract private class Symbol {
		public String name;
	}

	private class VariableSymbol extends Symbol {

		public Type type;
		public String address;

		@Override
		public String toString() {
			return type + " " + name + "<" + address + ">";
		}

	}

	private class FunctionSymbol extends Symbol {

		public Type returnType;
		public List<VariableSymbol> parameters = new ArrayList<>();

		@Override
		public String toString() {
			return returnType + " " + name + parameters;
		}

		@Override
		public boolean equals(Object obj) {
			if (obj instanceof FunctionSymbol) {
				FunctionSymbol function = (FunctionSymbol) obj;
				if (!function.name.equals(name)
						|| function.returnType != returnType
						|| function.parameters.size() != parameters.size())
					return false;
				for (int i = 0; i < parameters.size(); ++i)
					if (function.parameters.get(i).type != parameters.get(i).type)
						return false;
				return true;
			} else
				return false;
		}

	}

	private static class CompileException extends RuntimeException {

		private static final long serialVersionUID = 1L;
		private ParserRuleContext context;

		public CompileException(ParserRuleContext context, String message) {
			super(message);
			this.context = context;
		}

		public String getMessage(ParaCParser parser) {
			Interval sourceInterval = Interval.of(
					getContext().start.getStartIndex(),
					getContext().stop.getStopIndex());
			CharStream inputStream = getContext().start.getInputStream();
			String previousText = inputStream.getText(Interval.of(0,
					sourceInterval.a));
			Interval lineInterval = Interval.of(
					previousText.lastIndexOf('\n') + 1, sourceInterval.b);
			int lineNum = previousText.split("\n").length;
			int colNum = 1 + sourceInterval.a - lineInterval.a;
			while (!inputStream.getText(lineInterval).endsWith("\n"))
				++lineInterval.b;
			String lines = inputStream.getText(lineInterval);
			StringBuilder message = new StringBuilder(
					inputStream.getSourceName());
			message.append(':').append(lineNum).append(':').append(colNum)
					.append(": error: ").append(super.getMessage())
					.append('\n');
			int position = 0;
			for (String line : lines.split("\n")) {
				message.append(line).append('\n');
				byte[] chars = line.getBytes();
				for (int i = 0; i < chars.length; ++i) {
					int sourcePosition = lineInterval.a + position + i;
					if (" \t\r\n".indexOf(chars[i]) == -1) {
						if (sourcePosition >= sourceInterval.a
								&& sourcePosition <= sourceInterval.b)
							chars[i] = '^';
						else
							chars[i] = ' ';
					}
				}
				position += line.length() + 1;
				message.append(new String(chars)).append('\n');
			}
			return message.toString();
		}

		public ParserRuleContext getContext() {
			return context;
		}

	}

	private Deque<Map<String, Symbol>> symbolTable = new LinkedList<>();
	private Deque<Map<Interval, Type>> typeCache = new LinkedList<>();
	private Deque<ParserRuleContext> buffersChildren = new LinkedList<>();
	private Map<ParserRuleContext, String> codeMap = new HashMap<>();
	private int branchCounter = 0;
	private FunctionSymbol currentFunction = null;
	private Integer variableOffset = null;
	private VariableSymbol parallelIterator = null;

	private static final int INT_SIZE = 4;
	private static final int FLOAT_SIZE = 4;
	private static final int POINTER_SIZE = 4;

	@Override
	public void enterProgram(ProgramContext ctx) {
		pushSymbolTable();
	}

	@Override
	public void exitProgram(ProgramContext ctx) {
		if (!codeMap.isEmpty())
			throw new IllegalStateException("codeMap = " + codeMap);
	}

	@Override
	public void enterCompoundStatement(CompoundStatementContext ctx) {
		pushSymbolTable();
	}

	@Override
	public void exitCompoundStatement(CompoundStatementContext ctx) {
		popSymbolTable();
	}

	@Override
	public void enterFunctionDefinition(FunctionDefinitionContext ctx) {
		currentFunction = addFunctionSymbol(ctx.returnType, ctx.declarator());
		emit(ctx, ".global " + currentFunction.name);
		emit(ctx, currentFunction.name + ":");
		pushSymbolTable();
		variableOffset = 2 * INT_SIZE;
		for (VariableSymbol parameter : currentFunction.parameters) {
			parameter.address = variableOffset + "(%ebp)";
			switch (parameter.type) {
			case INT:
				variableOffset += INT_SIZE;
				break;
			case FLOAT:
				variableOffset += FLOAT_SIZE;
				break;
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				variableOffset += POINTER_SIZE;
				break;
			}
			addSymbol(ctx, parameter);
		}
		emit2(ctx, "push %ebp");
		emit2(ctx, "mov %esp, %ebp");
		variableOffset = 0;
	}

	@Override
	public void exitFunctionDefinition(FunctionDefinitionContext ctx) {
		if (currentFunction.returnType == null) {
			emit2(ctx, "leave");
			emit2(ctx, "ret");
		} else {
			log(ctx, "leave/ret should already have been emitted by a return");
		}
		popSymbolTable();
		variableOffset = null;
		currentFunction = null;
	}

	@Override
	public void exitJumpStatement(JumpStatementContext ctx) {
		Type type = getCachedType(ctx.expression());
		Type wantedType = currentFunction.returnType;
		if (type == Type.INT && wantedType == Type.FLOAT)
			castIntToFloat(ctx);
		else if (type == Type.FLOAT && wantedType == Type.INT)
			castFloatToInt(ctx);
		else if ((type == Type.INT_POINTER && wantedType == Type.INT_ARRAY)
				|| (type == Type.FLOAT_POINTER && wantedType == Type.FLOAT_ARRAY)
				|| (type == Type.INT_ARRAY && wantedType == Type.INT_POINTER)
				|| (type == Type.FLOAT_ARRAY && wantedType == Type.FLOAT_POINTER))
			; // compatible
		else if (type != wantedType)
			throw new CompileException(ctx, "incompatible return type");
		if (wantedType != null) {
			switch (wantedType) {
			case INT:
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				emit2(ctx, "pop %eax");
				break;
			case FLOAT:
				emit2(ctx, "flds (%esp)");
				emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
				break;
			}
		}
		emit2(ctx, "leave");
		emit2(ctx, "ret");
	}

	@Override
	public void enterDeclaration(DeclarationContext ctx) {
		TypeNameContext typeName = ctx.typeName();
		if (ctx.declarator().size() == 1
				&& ctx.declarator(0).getChildCount() > 1
				&& ctx.declarator(0).getChild(1).getText().equals("(")) {
			DeclaratorContext declarator = ctx.declarator(0);
			addFunctionSymbol(typeName, declarator);
		} else {
			for (DeclaratorContext declarator : ctx.declarator()) {
				VariableSymbol variable = createVariable(typeName, declarator);
				int size;
				switch (variable.type) {
				case INT:
					size = INT_SIZE;
					break;
				case FLOAT:
					size = FLOAT_SIZE;
					break;
				case INT_POINTER:
				case FLOAT_POINTER:
					size = POINTER_SIZE;
					break;
				case INT_ARRAY:
					size = Integer.parseInt(declarator.ICONSTANT().getText())
							* INT_SIZE;
					break;
				case FLOAT_ARRAY:
					size = Integer.parseInt(declarator.ICONSTANT().getText())
							* FLOAT_SIZE;
					break;
				default:
					throw new RuntimeException();
				}
				if (variableOffset == null) { // global
					emit2(ctx, ".lcomm " + variable.name + ", " + size);
					variable.address = variable.name;
				} else { // stack
					emit2(ctx, "sub $" + size + ", %esp");
					variableOffset -= size;
					variable.address = variableOffset + "(%ebp)";
				}
				addSymbol(declarator, variable);
			}
		}
	}

	@Override
	public void exitPrimaryExpressionWithIdentifier(
			PrimaryExpressionWithIdentifierContext ctx) {
		VariableSymbol variable = getVariable(ctx, ctx.IDENTIFIER().getText());
		if (variable == parallelIterator) {
			if (ctx.getChildCount() != 1)
				throw new CompileException(ctx,
						"parallel iterator modification is forbidden");
			cacheType(ctx, Type.INT);
			emit2(ctx, "pushl (%ebx)");
			return;
		}
		if (ctx.getChildCount() <= 2) {
			switch (variable.type) {
			case INT:
			case FLOAT:
				emit2(ctx, "pushl " + variable.address);
				break;
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				emit2(ctx, "pushl $0");
				computeArrayIndexLocation(ctx, variable, 0);
				break;
			}
			cacheType(ctx, variable.type);
			if (ctx.getChildCount() == 2) {
				boolean inc = ctx.getChild(1).getText().equals("++");
				switch (variable.type) {
				case INT:
					emit2(ctx, (inc ? "incl " : "decl ") + variable.address);
					break;
				case FLOAT:
					emit2(ctx, "movl $" + Float.floatToRawIntBits(1) + ", -"
							+ FLOAT_SIZE + "(%esp)");
					emit2(ctx, "movss (%esp), %xmm0");
					emit2(ctx, (inc ? "addss " : "subss ") + -FLOAT_SIZE
							+ "(%esp), %xmm0");
					emit2(ctx, "movss %xmm0, " + variable.address);
					break;
				case INT_POINTER:
				case FLOAT_POINTER:
				case INT_ARRAY:
				case FLOAT_ARRAY:
					throw new CompileException(ctx,
							"pointer arithmetic is forbidden");
				}
			}
		} else {
			computeArrayIndexLocation(ctx, variable, 0);
			emit2(ctx, "pop %eax");
			emit2(ctx, "push (%eax)");
		}
	}

	/** @warning has side effect on type cache */
	private void computeArrayIndexLocation(ParserRuleContext ctx,
			VariableSymbol variable, int indexStackIndex) {
		boolean pointer = false;
		int size;
		switch (variable.type) {
		case INT_POINTER:
			pointer = true;
		case INT_ARRAY:
			cacheType(ctx, Type.INT);
			size = INT_SIZE;
			break;
		case FLOAT_POINTER:
			pointer = true;
		case FLOAT_ARRAY:
			cacheType(ctx, Type.FLOAT);
			size = FLOAT_SIZE;
			break;
		default:
			throw new IllegalArgumentException();
		}
		emit2(ctx, "mov " + indexStackIndex * size + "(%esp), %eax");
		emit2(ctx, "imul $" + size + ", %eax");
		if (pointer) {
			emit2(ctx, "add " + variable.address + ", %eax");
		} else {
			// XXX variable offset from address string hack
			if (variable.address.endsWith(")")) {
				int offset = Integer.parseInt(variable.address.replace(
						"(%ebp)", ""));
				emit2(ctx, "add $" + offset + ", %eax");
				emit2(ctx, "add %ebp, %eax");
			} else {
				emit2(ctx, "add $" + variable.address + ", %eax");
			}
		}
		emit2(ctx, "mov %eax, " + indexStackIndex * size + "(%esp)");
	}

	@Override
	public void exitPrimaryExpressionWithInteger(
			PrimaryExpressionWithIntegerContext ctx) {
		cacheType(ctx, Type.INT);
		emit2(ctx, "pushl $" + ctx.getText());
	}

	@Override
	public void exitPrimaryExpressionWithFloat(
			PrimaryExpressionWithFloatContext ctx) {
		cacheType(ctx, Type.FLOAT);
		emit2(ctx,
				"pushl $"
						+ Float.floatToRawIntBits(Float.parseFloat(ctx
								.getText())));
	}

	@Override
	public void exitPrimaryExpressionWithExpression(
			PrimaryExpressionWithExpressionContext ctx) {
		cacheType(ctx, getCachedType(ctx.expression()));
	}

	@Override
	public void enterPrimaryExpressionWithCall(
			PrimaryExpressionWithCallContext ctx) {
		buffersChildren.push(ctx);
	}

	@Override
	public void exitPrimaryExpressionWithCall(
			PrimaryExpressionWithCallContext ctx) {
		buffersChildren.pop();
		String functionName = ctx.IDENTIFIER().getText();
		FunctionSymbol functionSymbol = null;
		try {
			functionSymbol = (FunctionSymbol) findSymbol(functionName);
		} catch (ClassCastException e) {
			throw new CompileException(ctx, "symbol is not a function for call");
		}
		if (functionSymbol == null)
			throw new CompileException(ctx, "no such function for call");
		if (functionSymbol.parameters.size() != ctx.expression().size())
			throw new CompileException(ctx, "invalid argument count for call");
		int size = 0;
		Iterator<VariableSymbol> parameter = functionSymbol.parameters
				.iterator();
		for (int i = 0; i < ctx.getChildCount(); ++i) {
			String code = codeMap.remove(ctx.getChild(i));
			if (code == null)
				continue;
			emit(ctx, code);
			Type type = getCachedType(ctx.getChild(i));
			Type wantedType = parameter.next().type;
			if (type == Type.INT && wantedType == Type.FLOAT)
				castIntToFloat(ctx);
			else if (type == Type.FLOAT && wantedType == Type.INT)
				castFloatToInt(ctx);
			else if ((type == Type.INT_POINTER && wantedType == Type.INT_ARRAY)
					|| (type == Type.FLOAT_POINTER && wantedType == Type.FLOAT_ARRAY)
					|| (type == Type.INT_ARRAY && wantedType == Type.INT_POINTER)
					|| (type == Type.FLOAT_ARRAY && wantedType == Type.FLOAT_POINTER))
				; // compatible
			else if (type != wantedType)
				throw new CompileException(ctx, "wrong argument type for call");
			switch (wantedType) {
			case INT:
				size += INT_SIZE;
				break;
			case FLOAT:
				size += FLOAT_SIZE;
				break;
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				size += POINTER_SIZE;
				break;
			}
		}
		emit2(ctx, "call " + ctx.IDENTIFIER().getText());
		emit2(ctx, "add $" + size + ", %esp");
		if (functionSymbol.returnType != null) {
			switch (functionSymbol.returnType) {
			case INT:
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				emit2(ctx, "push %eax");
				break;
			case FLOAT:
				emit2(ctx, "sub $" + FLOAT_SIZE + ", %esp");
				emit2(ctx, "fstp (%esp)");
				break;
			}
		}
		cacheType(ctx, functionSymbol.returnType);
	}

	@Override
	public void exitUnaryExpression(UnaryExpressionContext ctx) {
		if (ctx.getChildCount() == 1) {
			cacheType(ctx, getCachedType(ctx.primaryExpression()));
			return;
		}
		String operator = ctx.getChild(0).getText();
		Type type = getCachedType(ctx.unaryExpression());
		Type returnType = null;
		switch (operator) {
		case "-":
			switch (type) {
			case INT:
				returnType = Type.INT;
				emit2(ctx, "pop %eax");
				emit2(ctx, "neg %eax");
				emit2(ctx, "push %eax");
				break;
			case FLOAT:
				returnType = Type.FLOAT;
				// Flip highest bit
				// - with regular instructions:
				// emit2(ctx, "pop %eax");
				// emit2(ctx, "xor $0x80000000, %eax");
				// emit2(ctx, "push %eax");
				// - with x87 FPU instructions:
				// emit2(ctx, "flds (%esp)");
				// emit2(ctx, "fchs");
				// emit2(ctx, "fstp (%esp)");
				// - with SSE instructions:
				emit2(ctx, "movl $" + Float.floatToRawIntBits(-0.f) + ", "
						+ -FLOAT_SIZE + "(%esp)");
				emit2(ctx, "movss " + -FLOAT_SIZE + "(%esp), %xmm0");
				emit2(ctx, "xorps (%esp), %xmm0");
				emit2(ctx, "movss %xmm0, (%esp)");
				break;
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				break;
			}
			break;
		case "!":
			switch (type) {
			case INT:
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				returnType = Type.INT;
				emit2(ctx, "pop %eax");
				emit2(ctx, "xor %ecx, %ecx");
				emit2(ctx, "test %eax, %eax");
				emit2(ctx, "setz %cl");
				emit2(ctx, "push %ecx");
				break;
			case FLOAT:
				returnType = Type.INT;
				emit2(ctx, "xor %eax, %eax");
				emit2(ctx, "pxor %xmm0, %xmm0");
				emit2(ctx, "comiss (%esp), %xmm0");
				emit2(ctx, "setz %al");
				emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
				emit2(ctx, "push %eax");
				break;
			}
			break;
		}
		if (returnType != null)
			cacheType(ctx, returnType);
		else
			throw new CompileException(ctx, "cannot apply unary operator");
	}

	@Override
	public void exitBinaryExpression(BinaryExpressionContext ctx) {
		if (ctx.getChildCount() == 1) {
			cacheType(ctx, getCachedType(ctx.unaryExpression()));
			return;
		}
		Type type1 = getCachedType(ctx.binaryExpression(0));
		Type type2 = getCachedType(ctx.binaryExpression(1));
		Type returnType = null;
		if (type1 == Type.INT && type2 == Type.INT)
			returnType = Type.INT;
		else if (type1 == Type.FLOAT && type2 == Type.FLOAT)
			returnType = Type.FLOAT;
		else if (type1 == Type.INT && type2 == Type.FLOAT) {
			emit2(ctx, "pop %ecx");
			emit2(ctx, "pop %eax");
			emit2(ctx, "push %ecx");
			emit2(ctx, "push %eax");
			castIntToFloat(ctx);
			emit2(ctx, "pop %ecx");
			emit2(ctx, "pop %eax");
			emit2(ctx, "push %ecx");
			emit2(ctx, "push %eax");
			returnType = Type.FLOAT;
		} else if (type1 == Type.FLOAT && type2 == Type.INT) {
			castIntToFloat(ctx);
			returnType = Type.FLOAT;
		}
		if (returnType != null) {
			String operator = ctx.getChild(1).getText();
			switch (returnType) {
			case INT:
				emit2(ctx, "pop %ecx");
				emit2(ctx, "pop %eax");
				switch (operator) {
				case "*":
					emit2(ctx, "imul %ecx, %eax");
					emit2(ctx, "push %eax");
					break;
				case "+":
					emit2(ctx, "add %ecx, %eax");
					emit2(ctx, "push %eax");
					break;
				case "-":
					emit2(ctx, "sub %ecx, %eax");
					emit2(ctx, "push %eax");
					break;
				case "<":
				case ">":
				case "<=":
				case ">=":
				case "==":
				case "!=":
					emit2(ctx, "cmp %ecx, %eax");
					emit2(ctx, "mov $0, %eax");
					switch (operator) {
					case "<":
						emit2(ctx, "setb %al");
						break;
					case ">":
						emit2(ctx, "seta %al");
						break;
					case "<=":
						emit2(ctx, "setbe %al");
						break;
					case ">=":
						emit2(ctx, "setae %al");
						break;
					case "==":
						emit2(ctx, "sete %al");
						break;
					case "!=":
						emit2(ctx, "setne %al");
						break;
					default:
						returnType = null;
					}
					emit2(ctx, "push %eax");
					break;
				default:
					returnType = null;
				}
				break;
			case FLOAT:
				emit2(ctx, "movss (%esp), %xmm0");
				emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
				switch (operator) {
				case "+":
					emit2(ctx, "addss (%esp), %xmm0");
					emit2(ctx, "movss %xmm0, (%esp)");
					break;
				case "-":
					emit2(ctx, "subss (%esp), %xmm0");
					emit2(ctx, "movss %xmm0, (%esp)");
					break;
				case "*":
					emit2(ctx, "mulss (%esp), %xmm0");
					emit2(ctx, "movss %xmm0, (%esp)");
					break;
				case "<":
				case ">":
				case "<=":
				case ">=":
				case "==":
				case "!=":
					returnType = Type.INT;
					emit2(ctx, "movss (%esp), %xmm1");
					emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
					emit2(ctx, "xor %eax, %eax");
					emit2(ctx, "comiss %xmm0, %xmm1");
					switch (operator) {
					case "<":
						emit2(ctx, "setb %al");
						break;
					case ">":
						emit2(ctx, "seta %al");
						break;
					case "<=":
						emit2(ctx, "setbe %al");
						break;
					case ">=":
						emit2(ctx, "setae %al");
						break;
					case "==":
						emit2(ctx, "sete %al");
						break;
					case "!=":
						emit2(ctx, "setne %al");
						break;
					default:
						returnType = null;
					}
					emit2(ctx, "push %eax");
					break;
				default:
					returnType = null;
				}
				break;
			case INT_POINTER:
			case FLOAT_POINTER:
			case INT_ARRAY:
			case FLOAT_ARRAY:
				returnType = null;
				break;
			}
		}
		if (returnType != null)
			cacheType(ctx, returnType);
		else
			throw new CompileException(ctx, "cannot apply binary operator");
	}

	// http://csapp.cs.cmu.edu/public/waside/waside-sse.pdf
	// type "intel ..."
	private void castIntToFloat(ParserRuleContext ctx) {
		emit2(ctx, "cvtsi2ss (%esp), %xmm0");
		emit2(ctx, "movss %xmm0, (%esp)");
	}

	private void castFloatToInt(ParserRuleContext ctx) {
		throw new CompileException(ctx, "cannot cast from float to int");
	}

	@Override
	public void exitExpressionWithAssignment(ExpressionWithAssignmentContext ctx) {
		VariableSymbol variable = getVariable(ctx, ctx.IDENTIFIER().getText());
		if (variable == parallelIterator)
			throw new CompileException(ctx,
					"parallel iterator modification is forbidden");
		Type type = getCachedType(ctx.binaryExpression());
		if (type == null)
			throw new CompileException(ctx, "cannot assign void");
		String castName = variable.type + "=" + type;
		boolean array = ctx.getChild(1).getText().equals("[");
		if (array)
			castName = castName.replace("_ARRAY=", "=").replace("_POINTER=",
					"=");
		if (castName.contains("_ARRAY="))
			throw new CompileException(ctx, "cannot assign to array variable");
		log(ctx, ctx.getText() + " → " + castName);
		switch (castName) {
		case "INT=INT":
			cacheType(ctx, Type.INT);
			break;
		case "FLOAT=FLOAT":
			cacheType(ctx, Type.FLOAT);
			break;
		case "INT_POINTER=INT_POINTER":
		case "INT_POINTER=INT_ARRAY":
			cacheType(ctx, Type.INT_POINTER);
			break;
		case "FLOAT_POINTER=FLOAT_POINTER":
		case "FLOAT_POINTER=FLOAT_ARRAY":
			cacheType(ctx, Type.FLOAT_POINTER);
			break;
		case "FLOAT=INT":
			castIntToFloat(ctx);
			break;
		case "INT=FLOAT":
			castFloatToInt(ctx);
			break;
		default:
			throw new CompileException(ctx, "invalid cast to "
					+ castName.replace('_', ' ').replace("=", " from "));
		}
		if (array) {
			computeArrayIndexLocation(ctx, variable, 1);
			emit2(ctx, "pop %eax");
			emit2(ctx, "pop %ecx");
			emit2(ctx, "push %eax");
			emit2(ctx, "mov %eax, (%ecx)");
		} else {
			emit2(ctx, "mov (%esp), %eax");
			emit2(ctx, "mov %eax, " + variable.address);
		}
	}

	@Override
	public void enterSelectionStatement(SelectionStatementContext ctx) {
		buffersChildren.add(ctx);
	}

	@Override
	public void exitSelectionStatement(SelectionStatementContext ctx) {
		buffersChildren.remove(ctx);
		String label = newLabel();
		String elseLabel = label + "else";
		String endLabel = label + "end";
		emitBuffered(ctx.expression());
		Type type = getCachedType(ctx.expression());
		if (type == null)
			throw new CompileException(ctx.expression(),
					"cannot branch on void expression");
		switch (type) {
		case INT:
		case INT_POINTER:
		case FLOAT_POINTER:
		case INT_ARRAY:
		case FLOAT_ARRAY:
			emit2(ctx, "pop %eax");
			emit2(ctx, "cmp $0, %eax");
			break;
		case FLOAT:
			emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
			emit2(ctx, "pxor %xmm0, %xmm0");
			emit2(ctx, "comiss " + -FLOAT_SIZE + "(%esp), %xmm0");
			break;
		}
		emit2(ctx, "je " + elseLabel);
		emitBuffered(ctx.statement(0));
		emit2(ctx, "jmp " + endLabel);
		emit(ctx, elseLabel + ":");
		emitBuffered(ctx.statement(1));
		emit(ctx, endLabel + ":");
	}

	@Override
	public void enterIterationStatement(IterationStatementContext ctx) {
		buffersChildren.add(ctx);
	}

	@Override
	public void exitIterationStatement(IterationStatementContext ctx) {
		buffersChildren.remove(ctx);
		String label = newLabel();
		String testLabel = label + "test";
		String endLabel = label + "end";
		emitBuffered(ctx.init);
		ignoreExpressionValue(ctx.init);
		emit(ctx, testLabel + ":");
		emitBuffered(ctx.condition);
		Type type = getCachedType(ctx.condition);
		if (type == null)
			throw new CompileException(ctx.condition,
					"cannot branch on void expression");
		switch (type) {
		case INT:
		case INT_POINTER:
		case FLOAT_POINTER:
		case INT_ARRAY:
		case FLOAT_ARRAY:
			emit2(ctx, "pop %eax");
			emit2(ctx, "cmp $0, %eax");
			break;
		case FLOAT:
			emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
			emit2(ctx, "pxor %xmm0, %xmm0");
			emit2(ctx, "comiss " + -FLOAT_SIZE + "(%esp), %xmm0");
			break;
		}
		emit2(ctx, "je " + endLabel);
		emitBuffered(ctx.statement());
		emitBuffered(ctx.next);
		ignoreExpressionValue(ctx.next);
		emit2(ctx, "jmp " + testLabel);
		emit(ctx, endLabel + ":");
	}

	@Override
	public void enterParallelIterationStatement(
			ParallelIterationStatementContext ctx) {
		if (parallelIterator != null)
			throw new CompileException(ctx, "parallel loops cannot be nested");
		buffersChildren.add(ctx);
		String iterator = ctx.i1.getText();
		if (!iterator.equals(ctx.i2.getText())
				|| !ctx.i2.getText().equals(ctx.i3.getText()))
			throw new CompileException(ctx,
					"only one identifier can be used as iterator");
		parallelIterator = getVariable(ctx, iterator);
		if (parallelIterator.type != Type.INT)
			throw new CompileException(ctx, "parallel iterator must be an int");
	}

	@Override
	public void exitParallelIterationStatement(
			ParallelIterationStatementContext ctx) {
		buffersChildren.remove(ctx);
		String label = newLabel();
		String threadLabel = label + "thread";
		String joinLabel = label + "join";

		emit2(ctx, "mov $" + threadLabel + ", %eax");
		emit2(ctx, "mov %eax, __parac_parallel_for_thread");
		emit2(ctx, "mov %ebp, __parac_parallel_for_ebp");
		emit2(ctx, "mov %esp, __parac_parallel_for_esp");

		emitBuffered(ctx.to);
		emit2(ctx, "mov (%esp), %eax");
		emit2(ctx, "mov %eax, " + parallelIterator.address);
		emitBuffered(ctx.from);
		emit2(ctx, "call __parac_run_parallel_for");
		emit2(ctx, "add $" + 2 * INT_SIZE + ", %esp");

		emit2(ctx, "jmp " + joinLabel);

		emit(ctx, threadLabel + ":");
		emit2(ctx, "push %ebp");
		emit2(ctx, "push %ebx");
		emit2(ctx, "mov __parac_parallel_for_ebp, %ebp");
		emit2(ctx, "mov " + 3 * INT_SIZE + "(%esp), %ebx");
		emitBuffered(ctx.statement());
		emit2(ctx, "pop %ebx");
		emit2(ctx, "pop %ebp");
		emit2(ctx, "ret");

		emit(ctx, joinLabel + ":");
		emit2(ctx, "mov __parac_parallel_for_esp, %esp");
		emit2(ctx, "movl $0, __parac_parallel_for_thread");
		emit2(ctx, "movl $0, __parac_parallel_for_ebp");
		emit2(ctx, "movl $0, __parac_parallel_for_esp");
		parallelIterator = null;
	}

	@Override
	public void enterStatement(StatementContext ctx) {
		log(ctx,
				"---- "
						+ ctx.start
								.getInputStream()
								.getText(
										Interval.of(ctx.start.getStartIndex(),
												ctx.stop.getStopIndex()))
								.replace('\n', ' '));
		typeCache.push(new HashMap<>());
	}

	@Override
	public void exitStatement(StatementContext ctx) {
		ignoreExpressionValue(ctx.expression());
		typeCache.pop();
		log(ctx, "----");
	}

	private void ignoreExpressionValue(ExpressionContext ctx) {
		Type type = getCachedType(ctx);
		if (type == null)
			return;
		switch (type) {
		case INT:
			emit2(ctx, "add $" + INT_SIZE + ", %esp");
			break;
		case FLOAT:
			emit2(ctx, "add $" + FLOAT_SIZE + ", %esp");
			break;
		case INT_POINTER:
		case FLOAT_POINTER:
			emit2(ctx, "add $" + POINTER_SIZE + ", %esp");
			break;
		case INT_ARRAY:
		case FLOAT_ARRAY:
			// only happens with useless "a[i];" statements
			break;
		}
	}

	private void pushSymbolTable() {
		symbolTable.push(new HashMap<>());
	}

	private void popSymbolTable() {
		log(null, symbolTable.toString());
		symbolTable.pop();
	}

	private void addSymbol(ParserRuleContext ctx, Symbol symbol) {
		Symbol old = symbolTable.element().put(symbol.name, symbol);
		if (old != null && !old.equals(symbol))
			throw new CompileException(ctx,
					"incompatible declarations of symbol " + symbol.name);
	}

	private FunctionSymbol addFunctionSymbol(TypeNameContext returnType,
			DeclaratorContext declarator) {
		FunctionSymbol function = new FunctionSymbol();
		DeclaratorContext nameDeclarator = declarator.declarator();
		boolean pointer = false;
		if (nameDeclarator.getChildCount() == 2
				&& nameDeclarator.getChild(0).getText().startsWith("*")) {
			pointer = true;
		} else if (nameDeclarator.getChildCount() != 1)
			throw new CompileException(nameDeclarator,
					"invalid function declaration");
		function.name = nameDeclarator.IDENTIFIER().getText();
		switch (returnType.getText() + (pointer ? "*" : "")) {
		case "void":
			function.returnType = null;
			break;
		case "int":
			function.returnType = Type.INT;
			break;
		case "float":
			function.returnType = Type.FLOAT;
			break;
		case "int*":
			function.returnType = Type.INT_POINTER;
			break;
		case "float*":
			function.returnType = Type.FLOAT_POINTER;
			break;
		default:
			throw new CompileException(returnType, "invalid return type");
		}
		for (ParameterDeclarationContext parameter : declarator
				.parameterDeclaration()) {
			function.parameters.add(createVariable(parameter.typeName(),
					parameter.declarator()));
		}
		addSymbol(declarator, function);
		return function;
	}

	private Symbol findSymbol(String name) {
		for (Map<String, Symbol> subTable : symbolTable) {
			Symbol symbol = subTable.get(name);
			if (symbol != null)
				return symbol;
		}
		return null;
	}

	private VariableSymbol getVariable(ParserRuleContext ctx, String name) {
		Symbol symbol = findSymbol(name);
		if (symbol == null)
			throw new CompileException(ctx, "no such variable");
		if (!(symbol instanceof VariableSymbol))
			throw new CompileException(ctx, "symbol is not a variable");
		return (VariableSymbol) symbol;
	}

	private VariableSymbol createVariable(TypeNameContext typeName,
			DeclaratorContext declarator) {
		VariableSymbol variable = new VariableSymbol();
		switch (declarator.getChildCount()) {
		case 1:
			variable.name = declarator.getText();
			switch (typeName.getText()) {
			case "int":
				variable.type = Type.INT;
				break;
			case "float":
				variable.type = Type.FLOAT;
				break;
			}
			break;
		case 2:
			if (declarator.getChild(0).getText().equals("*")) {
				variable.name = declarator.getChild(1).getText();
				switch (typeName.getText()) {
				case "int":
					variable.type = Type.INT_POINTER;
					break;
				case "float":
					variable.type = Type.FLOAT_POINTER;
					break;
				}
			}
			break;
		case 4:
			if (declarator.getChild(1).getText().equals("[")) {
				variable.name = declarator.getChild(0).getText();
				switch (typeName.getText()) {
				case "int":
					variable.type = Type.INT_ARRAY;
					break;
				case "float":
					variable.type = Type.FLOAT_ARRAY;
					break;
				}
			}
		}
		if (variable.name == null)
			throw new CompileException(declarator,
					"invalid declaration of variable");
		else if (variable.type == null)
			throw new CompileException(typeName, "invalid type for variable");
		else
			return variable;
	}

	private void cacheType(ParserRuleContext ctx, Type type) {
		Type oldType = typeCache.element().put(ctx.getSourceInterval(), type);
		if (type != oldType && !Objects.equals(oldType, type))
			log(ctx, ctx.getText() + " → "
					+ (type != null ? type.toString() : "VOID"));
	}

	private Type getCachedType(ParseTree ctx) {
		return ctx != null ? typeCache.element().get(ctx.getSourceInterval())
				: null;
	}

	private String newLabel() {
		return "__" + branchCounter++ + "_";
	}

	private void emit(ParserRuleContext ctx, String code) {
		if (buffersChildren.isEmpty()) {
			System.out.println(code);
		} else {
			ParserRuleContext parentCtx = (ParserRuleContext) ctx.parent;
			while (parentCtx != null && parentCtx != buffersChildren.peek()) {
				ctx = parentCtx;
				parentCtx = (ParserRuleContext) ctx.parent;
			}
			String existingCode = codeMap.get(ctx);
			codeMap.put(ctx, existingCode != null ? existingCode + "\n" + code
					: code);
		}
	}

	private void emit2(ParserRuleContext ctx, String code) {
		emit(ctx, "\t" + code.replaceFirst(" ", "\t"));
	}

	private boolean emitBuffered(ParserRuleContext ctx) {
		String code = codeMap.remove(ctx);
		if (code == null)
			return false;
		emit(ctx, code);
		return true;
	}

	private void log(ParserRuleContext ctx, String message) {
		emit(ctx, "# " + message);
	}

	public static void main(String[] args) throws Exception {
		if (args.length == 0) {
			System.err.println("error: missing source file");
			System.err.println("USAGE: ParaC CFILE [CFILE...]");
			System.exit(1);
		}
		for (String filename : args) {
			ANTLRFileStream input = null;
			try {
				input = new ANTLRFileStream(filename);
			} catch (Exception e) {
				System.err.println("error: cannot open source file: "
						+ filename);
				System.exit(1);
			}
			ParaCParser parser = new ParaCParser(new CommonTokenStream(
					new ParaCLexer(input)));
			parser.setErrorHandler(new BailErrorStrategy());
			try {
				new ParseTreeWalker().walk(new ParaCCompiler(),
						parser.program());
			} catch (CompileException e) {
				System.err.println(e.getMessage(parser));
				System.exit(1);
			}
		}
	}

}
