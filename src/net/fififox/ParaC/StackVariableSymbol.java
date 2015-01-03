package net.fififox.ParaC;

public class StackVariableSymbol extends VariableSymbol {
	
	public int offset;

	@Override
	public String toString() {
		return "StackVariable<" + type + ", " + name + ", " + offset
				+ "(%ebp)>";
	}

}
