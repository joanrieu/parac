package net.fififox.ParaC;

import java.util.Deque;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
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
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithFloatContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithIdentifierContext;
import net.fififox.ParaC.ParaCParser.PrimaryExpressionWithIntegerContext;
import net.fififox.ParaC.ParaCParser.ProgramContext;
import net.fififox.ParaC.ParaCParser.SelectionStatementContext;
import net.fififox.ParaC.ParaCParser.StatementContext;
import net.fififox.ParaC.ParaCParser.TypeNameContext;
import net.fififox.ParaC.ParaCParser.UnaryExpressionContext;
import net.fififox.ParaC.VariableSymbol.Type;

import org.antlr.v4.runtime.RuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.antlr.v4.runtime.tree.ParseTree;

// http://unixwiz.net/techtips/win32-callconv-asm.html
// https://en.wikipedia.org/wiki/X86_calling_conventions#cdecl
public class AssemblyGenerator extends ParaCBaseListener {

	private Deque<Map<String, Symbol>> symbolTable = new LinkedList<>();
	private Map<RuleContext, String> codeMap = new HashMap<>();
	private Deque<RuleContext> buffersChildren = new LinkedList<>();
	private Integer variableOffset = null;
	private int branchCounter = 0;
	private String specialParallelVariableName = null;

	private Deque<Map<Interval, Type>> typeCache = new LinkedList<>();

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
			throw new RuntimeException("codeMap = " + codeMap);
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
		FunctionSymbol function = addFunctionSymbol(ctx.returnType,
				ctx.declarator());
		emit(ctx, ".global " + function.name);
		emit(ctx, function.name + ":");
		pushSymbolTable();
		variableOffset = 2 * INT_SIZE;
		for (VariableSymbol parameter : function.parameters) {
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
			addSymbol(parameter);
		}
		emit2(ctx, "push %ebp");
		emit2(ctx, "mov %esp, %ebp");
		variableOffset = 0;
	}

	@Override
	public void exitFunctionDefinition(FunctionDefinitionContext ctx) {
		// TODO check return type
		emit2(ctx, "leave");
		emit2(ctx, "ret");
		popSymbolTable();
		variableOffset = null;
	}

	@Override
	public void exitJumpStatement(JumpStatementContext ctx) {
		// TODO check return type
		if (ctx.expression() != null)
			emit2(ctx, "pop %eax");
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
				addSymbol(variable);
			}
		}
	}

	@Override
	public void exitPrimaryExpressionWithIdentifier(
			PrimaryExpressionWithIdentifierContext ctx) {
		String name = ctx.IDENTIFIER().getText();
		if (name.equals(specialParallelVariableName)) {
			if (ctx.getChildCount() != 1)
				throw new RuntimeException(
						"Parallel iterator modification is forbidden: " + name);
			// XXX is it really?
			cacheType(ctx, Type.INT);
			emit2(ctx, "pushl (%ebx)");
			return;
		}
		VariableSymbol variable = getVariable(name);
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
					// TODO break;
				case INT_POINTER:
				case FLOAT_POINTER:
				case INT_ARRAY:
				case FLOAT_ARRAY:
					throw new RuntimeException(
							"Pointer arithmetic is forbidden: " + ctx.getText());
				}
			}
		} else {
			computeArrayIndexLocation(ctx, variable, 0);
			emit2(ctx, "pop %eax");
			emit2(ctx, "push (%eax)");
		}
	}

	/** @warning has side effect on type cache */
	private void computeArrayIndexLocation(RuleContext ctx,
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
			throw new RuntimeException();
		}
		emit2(ctx, "mov " + indexStackIndex * size + "(%esp), %eax");
		emit2(ctx, "imul $" + size + ", %eax");
		if (pointer) {
			emit2(ctx, "add " + variable.address + ", %eax");
		} else {
			// XXX hack
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
			throw new RuntimeException("Symbol is not a function for call: "
					+ ctx.getText());
		}
		if (functionSymbol == null)
			throw new RuntimeException("No such function for call: "
					+ ctx.getText());
		if (functionSymbol.parameters.size() != ctx.expression().size())
			throw new RuntimeException("Invalid argument count for call: "
					+ ctx.getText());
		emit2(ctx, "push %ebx"); // XXX used in parallel for
		int size = 0;
		Iterator<VariableSymbol> parameter = functionSymbol.parameters
				.iterator();
		for (int i = ctx.getChildCount() - 1; i >= 0; --i) {
			String code = codeMap.remove(ctx.getChild(i));
			if (code == null)
				continue;
			emit(ctx, code);
			Type type = getCachedType(ctx.getChild(i));
			Type wantedType = parameter.next().type;
			// TODO type cast ptr←/→array
			// TODO maybe float→int (warning)
			if (type == Type.INT && wantedType == Type.FLOAT)
				castIntToFloat(ctx);
			else if (type != wantedType)
				throw new RuntimeException("Wrong argument type for call: "
						+ ctx.getText());
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
		emit2(ctx, "push %eax");
		emit2(ctx, "pop %ebx"); // XXX restore push above
		cacheType(ctx, functionSymbol.returnType);
	}

	@Override
	public void exitUnaryExpression(UnaryExpressionContext ctx) {
		if (ctx.getChildCount() == 1) {
			cacheType(ctx, getCachedType(ctx.primaryExpression()));
			return;
		}
		// TODO other types
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
				emit2(ctx, "test %eax, %eax");
				emit2(ctx, "lahf");
				emit2(ctx, "shrw $14, %ax");
				emit2(ctx, "and $1, %eax");
				emit2(ctx, "push %eax");
				break;
			}
			break;
		}
		if (returnType != null)
			cacheType(ctx, returnType);
		else
			throw new RuntimeException("Cannot apply unary operator: "
					+ ctx.getText());
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
				String label = null;
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
					label = newLabel() + "skip";
					emit2(ctx, "cmp %ecx, %eax");
					emit2(ctx, "mov $0, %ecx");
					switch (operator) {
					case ">":
						emit2(ctx, "jng " + label);
						break;
					case "<":
						emit2(ctx, "jnl " + label);
						break;
					case ">=":
						emit2(ctx, "jnge " + label);
						break;
					case "<=":
						emit2(ctx, "jnle " + label);
						break;
					case "==":
						emit2(ctx, "jne " + label);
						break;
					case "!=":
						emit2(ctx, "je " + label);
						break;
					}
					emit2(ctx, "inc %ecx");
					emit(ctx, label + ":");
					emit2(ctx, "push %ecx");
					break;
				}
				break;
			case FLOAT:
				switch (operator) {
				case "+":
					emit2(ctx, "movss (%esp), %xmm0");
					emit2(ctx, "pop %eax");
					emit2(ctx, "addss (%esp), %xmm0");
					emit2(ctx, "movss %xmm0, (%esp)");
					break;
				case "-":
					emit2(ctx, "movss (%esp), %xmm0");
					emit2(ctx, "pop %eax");
					emit2(ctx, "subss (%esp), %xmm0");
					emit2(ctx, "movss %xmm0, (%esp)");
					break;
				case "*":
					emit2(ctx, "movss (%esp), %xmm0");
					emit2(ctx, "pop %eax");
					emit2(ctx, "mulss (%esp), %xmm0");
					emit2(ctx, "movss %xmm0, (%esp)");
					break;
				default:
					// TODO float comparison operators
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
			throw new RuntimeException("Cannot apply binary operator: "
					+ ctx.getText());
	}

	// http://csapp.cs.cmu.edu/public/waside/waside-sse.pdf
	// type "intel ..."
	private void castIntToFloat(RuleContext ctx) {
		emit2(ctx, "cvtsi2ss (%esp), %xmm0");
		emit2(ctx, "movss %xmm0, (%esp)");
	}

	@Override
	public void exitExpressionWithAssignment(ExpressionWithAssignmentContext ctx) {
		VariableSymbol variable = getVariable(ctx.IDENTIFIER().getText());
		if (variable.name.equals(specialParallelVariableName))
			throw new RuntimeException(
					"Parallel iterator modification is forbidden: "
							+ variable.name);
		Type type = getCachedType(ctx.binaryExpression());
		if (type == null)
			throw new RuntimeException("Cannot assign void: " + ctx.getText());
		String castName = variable.type + "=" + type;
		boolean array = ctx.getChild(1).getText().equals("[");
		if (array)
			castName = castName.replace("_ARRAY=", "=")
					.replace("_POINTER", "=");
		if (castName.contains("_ARRAY="))
			throw new RuntimeException("Cannot assign to array variable: "
					+ ctx.getText());
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
			log(ctx, "todo: float to int cast"); // TODO
			/* break; */
		default:
			throw new RuntimeException("Invalid cast " + castName + " in: "
					+ ctx.getText());
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
		// TODO other types
		emit2(ctx, "pop %eax");
		emit2(ctx, "cmp $0, %eax");
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
		// TODO other types
		emit2(ctx, "pop %eax");
		emit2(ctx, "cmp $0, %eax");
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
		// FIXME check if declared
		// FIXME store symbol instead of name (cannot shadow otherwise)
		if (specialParallelVariableName != null)
			throw new RuntimeException("Parallel loops cannot be nested");
		buffersChildren.add(ctx);
		String iName = ctx.i1.getText();
		if (!iName.equals(ctx.i2.getText())
				|| !ctx.i2.getText().equals(ctx.i3.getText()))
			throw new RuntimeException(
					"Only one identifier can be used as index: " + iName + ", "
							+ ctx.i2.getText() + ", " + ctx.i3.getText());
		specialParallelVariableName = iName;
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
		specialParallelVariableName = null;
	}

	@Override
	public void enterStatement(StatementContext ctx) {
		log(ctx, "---- " + ctx.getText());
		typeCache.push(new HashMap<>());
	}

	@Override
	public void exitStatement(StatementContext ctx) {
		ignoreExpressionValue(ctx.expression());
		typeCache.pop();
		log(ctx, "----");
	}

	private void ignoreExpressionValue(ExpressionContext ctx) {
		// TODO other types
		if (ctx != null)
			emit2(ctx, "add $" + INT_SIZE + ", %esp");
	}

	private void pushSymbolTable() {
		symbolTable.push(new HashMap<>());
	}

	private void popSymbolTable() {
		log(null, symbolTable.toString());
		symbolTable.pop();
	}

	private Symbol addSymbol(Symbol symbol) {
		return symbolTable.element().put(symbol.name, symbol);
	}

	private FunctionSymbol addFunctionSymbol(TypeNameContext returnType,
			DeclaratorContext declarator) {
		FunctionSymbol function = new FunctionSymbol();
		// FIXME check if declarator is valid
		function.name = declarator.declarator().getText();
		switch (returnType.getText()) {
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
			throw new RuntimeException("Invalid return type: " + returnType
					+ " " + declarator);
		}
		for (ParameterDeclarationContext parameter : declarator
				.parameterDeclaration()) {
			function.parameters.add(createVariable(parameter.typeName(),
					parameter.declarator()));
		}
		Symbol oldSymbol = findSymbol(function.name);
		if (oldSymbol != null) {
			// TODO check if identical
			// throw new RuntimeException("Symbol already defined: " +
			// function.name);
		}
		addSymbol(function);
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

	private VariableSymbol getVariable(String name) {
		Symbol symbol = findSymbol(name);
		if (symbol == null)
			throw new RuntimeException("No such variable: " + name);
		if (!(symbol instanceof VariableSymbol))
			throw new RuntimeException("Symbol is not a variable: " + name);
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
		if (variable.name != null && variable.type != null)
			return variable;
		else
			throw new RuntimeException("Invalid declaration of variable: "
					+ variable.name);
	}

	private void cacheType(RuleContext ctx, Type type) {
		Type oldType = typeCache.element().put(ctx.getSourceInterval(), type);
		if (type != oldType && !Objects.equals(oldType, type))
			log(ctx, ctx.getText() + " → "
					+ (type != null ? type.toString() : "VOID"));
	}

	private Type getCachedType(ParseTree ctx) {
		return typeCache.element().get(ctx.getSourceInterval());
	}

	private String newLabel() {
		return "__" + branchCounter++ + "_";
	}

	private void emit(RuleContext ctx, String code) {
		if (buffersChildren.isEmpty()) {
			System.out.println(code);
		} else {
			RuleContext parentCtx = ctx.parent;
			while (parentCtx != null && parentCtx != buffersChildren.peek()) {
				ctx = parentCtx;
				parentCtx = ctx.parent;
			}
			String existingCode = codeMap.get(ctx);
			codeMap.put(ctx, existingCode != null ? existingCode + "\n" + code
					: code);
		}
	}

	private void emit2(RuleContext ctx, String code) {
		emit(ctx, "\t" + code.replaceFirst(" ", "\t"));
	}

	private boolean emitBuffered(RuleContext ctx) {
		String code = codeMap.remove(ctx);
		if (code == null)
			return false;
		emit(ctx, code);
		return true;
	}

	private void log(RuleContext ctx, String message) {
		emit(ctx, "# " + message);
	}

}
