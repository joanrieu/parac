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
			StackVariableSymbol local = new StackVariableSymbol();
			local.name = parameter.name;
			local.type = parameter.type;
			// TODO other types
			local.offset = variableOffset;
			variableOffset += INT_SIZE;
			addSymbol(local);
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
				VariableSymbol variable;
				if (variableOffset != 0) {
					StackVariableSymbol stackVariable = new StackVariableSymbol();
					stackVariable.offset = variableOffset;
					variableOffset -= INT_SIZE;
					// TODO other types
					variable = stackVariable;
				} else {
					variable = new VariableSymbol();
				}
				variable.name = declarator.getText();
				variable.type = typeName.getText();
				if (variable instanceof StackVariableSymbol) {
					emit2(ctx, "sub $" + INT_SIZE + ", %esp");
				} else {
					emit(ctx, ".bss");
					emit2(ctx, ".lcomm " + variable.name + ", " + INT_SIZE);
				}
				addSymbol(variable);
			}
		}
	}

	@Override
	public void exitPrimaryExpressionWithIdentifier(
			PrimaryExpressionWithIdentifierContext ctx) {
		String name = ctx.getChildCount() == 2 ? ctx.IDENTIFIER().getText()
				: ctx.getText();
		if (name.equals(specialParallelVariableName)) {
			if (ctx.getChildCount() != 1)
				throw new RuntimeException(
						"Parallel iterator modification is forbidden: " + name);
			emit2(ctx, "pushl (%ebx)");
			return;
		}
		String address = getVariableAddress(name);
		emit2(ctx, "movl " + address + ", %eax");
		if (ctx.getChildCount() == 2) {
			switch (ctx.getChild(1).getText()) {
			case "++":
				emit2(ctx, "incl " + address);
				break;
			case "--":
				emit2(ctx, "decl " + address);
				break;
			}
		}
		emit2(ctx, "pushl %eax");
		// TODO other types
	}

	@Override
	public void exitPrimaryExpressionWithInteger(
			PrimaryExpressionWithIntegerContext ctx) {
		emit2(ctx, "pushl $" + ctx.ICONSTANT().getText());
	}

	@Override
	public void exitPrimaryExpressionWithFloat(
			PrimaryExpressionWithFloatContext ctx) {
		// TODO other types
		todo(ctx, "pushl " + ctx.getText());
	}

	@Override
	public void enterPrimaryExpressionWithCall(
			PrimaryExpressionWithCallContext ctx) {
		buffersChildren.push(ctx);
		String functionName = ctx.IDENTIFIER().getText();
		FunctionSymbol functionSymbol = null;
		try {
			functionSymbol = (FunctionSymbol) findSymbol(functionName);
		} catch (ClassCastException e) {
		}
		if (functionSymbol == null)
			throw new RuntimeException("No such function for call: "
					+ ctx.getText());
		if (functionSymbol.parameters.size() != ctx.expression().size())
			throw new RuntimeException("Invalid argument count for call: "
					+ ctx.getText());
	}

	@Override
	public void exitPrimaryExpressionWithCall(
			PrimaryExpressionWithCallContext ctx) {
		buffersChildren.pop();
		FunctionSymbol functionSymbol = (FunctionSymbol) findSymbol(ctx
				.IDENTIFIER().getText());
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
		emit2(ctx, "call " + functionSymbol.name);
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
		emit2(ctx, "mov %eax, " + getVariableAddress(name));
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
		function.name = declarator.declarator().getText();
		function.returnType = returnType.getText();
		for (ParameterDeclarationContext parameter : declarator
				.parameterDeclaration()) {
			VariableSymbol variable = new VariableSymbol();
			variable.name = parameter.declarator().getText();
			variable.type = parameter.typeName().getText();
			// TODO other types
			function.parameters.add(variable);
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

	private String getVariableAddress(String name) {
		VariableSymbol symbol = null;
		try {
			symbol = (VariableSymbol) findSymbol(name);
		} catch (ClassCastException e) {
		}
		if (symbol == null)
			throw new RuntimeException("No such variable: " + name);
		return symbol instanceof StackVariableSymbol ? ""
				+ ((StackVariableSymbol) symbol).offset + "(%ebp)" : name;
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

	private void todo(RuleContext ctx, String message) {
		emit2(ctx, "#todo " + message);
	}

}
