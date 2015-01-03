package net.fififox.ParaC;

public class VariableSymbol extends Symbol {
	
	public String type;
	
	@Override
	public String toString() {
		return "Variable<" + type + ", " + name + ">";
	}
	
}
