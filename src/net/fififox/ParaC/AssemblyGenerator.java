package net.fififox.ParaC;

import java.util.Deque;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;

import net.fififox.ParaC.ParaCParser.BinaryExpressionContext;
import net.fififox.ParaC.ParaCParser.CompoundStatementContext;
import net.fififox.ParaC.ParaCParser.DeclarationContext;
import net.fififox.ParaC.ParaCParser.DeclaratorContext;
import net.fififox.ParaC.ParaCParser.ExpressionContext;
import net.fififox.ParaC.ParaCParser.ExpressionWithAssignmentContext;
import net.fififox.ParaC.ParaCParser.ExpressionWithoutAssignmentContext;
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

import org.antlr.v4.runtime.RuleContext;

// http://unixwiz.net/techtips/win32-callconv-asm.html
// https://en.wikipedia.org/wiki/X86_calling_conventions#cdecl
public class AssemblyGenerator extends ParaCBaseListener {

	private Deque<Map<String, Symbol>> symbolTable = new LinkedList<>();
	private Map<RuleContext, String> codeMap = new HashMap<>();
	private Deque<RuleContext> buffersChildren = new LinkedList<>();
	private int variableOffset = 0;
	private int branchCounter = 0;
	private String specialParallelVariableName = null;

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
		emit(ctx, ".text");
		FunctionSymbol function = addFunctionSymbol(ctx.returnType,
				ctx.declarator());
		emit(ctx, ".global " + function.name);
		emit(ctx, function.name + ":");
		pushSymbolTable();
		emit2(ctx, "push %ebp");
		emit2(ctx, "mov %esp, %ebp");
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
				variableOffset += POINTER_SIZE;
				break;
			case INT_ARRAY:
			case FLOAT_ARRAY:
				throw new RuntimeException(
						"Array forbidden in function prototype, use a pointer: "
								+ parameter.name);
			}
			addSymbol(parameter);
		}
		variableOffset = -INT_SIZE;
	}

	@Override
	public void exitFunctionDefinition(FunctionDefinitionContext ctx) {
		emit2(ctx, "leave");
		emit2(ctx, "ret");
		popSymbolTable();
		variableOffset = 0;
	}

	@Override
	public void exitJumpStatement(JumpStatementContext ctx) {
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
					size = INT_SIZE
							* Integer
									.parseInt(declarator.ICONSTANT().getText());
					break;
				case FLOAT_ARRAY:
					size = FLOAT_SIZE
							* Integer
									.parseInt(declarator.ICONSTANT().getText());
					break;
				default:
					throw new RuntimeException();
				}
				if (variableOffset == 0) { // global
					// emit(ctx, ".bss"); // XXX
					emit2(ctx, ".lcomm " + variable.name + ", " + INT_SIZE);
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
			// XXX is it really ?
			emit2(ctx, "pushl (%ebx)");
			return;
		}
		VariableSymbol variable = getVariable(name);
		switch (ctx.getChildCount()) {
		case 1:
			emit2(ctx, "pushl " + variable.address);
			break;
		case 2:
			switch (ctx.getChild(1).getText()) {
			case "++":
				switch (variable.type) {
				case INT:
					emit2(ctx, "incl " + variable.address);
					break;
				}
				break;
			case "--":
				switch (variable.type) {
				case INT:
					emit2(ctx, "decl " + variable.address);
					break;
				}
				break;
			}
			emit2(ctx, "pushl %eax");
		case 4:
			int size;
			switch (variable.type) {
			case INT_POINTER:
			case INT_ARRAY:
				size = INT_SIZE;
				break;
			case FLOAT_POINTER:
			case FLOAT_ARRAY:
				size = FLOAT_SIZE;
				break;
			default:
				throw new RuntimeException();
			}
			emit2(ctx, "movl $" + variable.address + ", %eax");
			emit2(ctx, "add $" + size + ", %eax");
			emit2(ctx, "push (%eax)");
			break;
		}
	}

	@Override
	public void exitPrimaryExpressionWithInteger(
			PrimaryExpressionWithIntegerContext ctx) {
		emit2(ctx, "pushl $" + ctx.getText());
	}

	@Override
	public void exitPrimaryExpressionWithFloat(
			PrimaryExpressionWithFloatContext ctx) {
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
		int pushedInts = 0;
		for (int i = ctx.getChildCount() - 1; i >= 0; --i) {
			String code = codeMap.remove(ctx.getChild(i));
			if (code == null)
				continue;
			emit(ctx, code);
			++pushedInts;
			// TODO other types
			// TODO check argument types
		}
		emit2(ctx, "call " + ctx.IDENTIFIER().getText());
		emit2(ctx, "add $" + pushedInts * INT_SIZE + ", %esp");
		emit2(ctx, "push %eax");
	}

	@Override
	public void exitUnaryExpression(UnaryExpressionContext ctx) {
		if (ctx.getChildCount() == 1)
			return;
		// TODO other types
		emit2(ctx, "pop %eax");
		String operator = ctx.getChild(0).getText();
		switch (operator) {
		case "-":
			emit2(ctx, "neg %eax");
			break;
		case "!":
			emit2(ctx, "test %eax, %eax");
			emit2(ctx, "lahf");
			emit2(ctx, "shrw $14, %ax");
			emit2(ctx, "and $1, %eax");
			break;
		}
		emit2(ctx, "push %eax");
	}

	@Override
	public void exitBinaryExpression(BinaryExpressionContext ctx) {
		if (ctx.getChildCount() == 1)
			return;
		// TODO other types
		String operator = ctx.getChild(1).getText();
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
			emit2(ctx, "mov $0, %ecx");
			emit2(ctx, "cmp %ecx, %eax");
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
	}

	@Override
	public void exitExpressionWithAssignment(ExpressionWithAssignmentContext ctx) {
		String name = ctx.IDENTIFIER().getText();
		if (name.equals(specialParallelVariableName))
			throw new RuntimeException(
					"Parallel iterator modification is forbidden: " + name);
		// TODO other types
		emit2(ctx, "pop %eax");
		emit2(ctx, "mov %eax, " + getVariable(name).address);
		emit2(ctx, "push %eax");
	}

	@Override
	public void exitExpressionWithoutAssignment(
			ExpressionWithoutAssignmentContext ctx) {
		// TODO other types
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
		log(ctx, "enter statement: " + ctx.getText());
	}

	@Override
	public void exitStatement(StatementContext ctx) {
		ignoreExpressionValue(ctx.expression());
		log(ctx, "exit statement: " + ctx.getText());
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
		function.returnType = returnType.getText();
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
				variable.type = VariableSymbol.Type.INT;
				break;
			case "float":
				variable.type = VariableSymbol.Type.FLOAT;
				break;
			}
			break;
		case 2:
			if (declarator.getChild(0).getText().equals("*")) {
				variable.name = declarator.getChild(1).getText();
				switch (typeName.getText()) {
				case "int":
					variable.type = VariableSymbol.Type.INT_POINTER;
					break;
				case "float":
					variable.type = VariableSymbol.Type.FLOAT_POINTER;
					break;
				}
			}
			break;
		case 4:
			if (declarator.getChild(1).getText().equals("[")) {
				variable.name = declarator.getChild(0).getText();
				switch (typeName.getText()) {
				case "int":
					variable.type = VariableSymbol.Type.INT_ARRAY;
					break;
				case "float":
					variable.type = VariableSymbol.Type.FLOAT_ARRAY;
					break;
				}
			}
		}
		log(null, variable.toString());
		if (variable.name != null && variable.type != null)
			return variable;
		else
			throw new RuntimeException("Invalid declaration of variable: "
					+ variable.name);
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
