package net.fififox.ParaC;

public class VariableSymbol extends Symbol {
	
	public String type;
	public String address;
	
	@Override
	public String toString() {
		return type + " " + name;
	}
	
}
