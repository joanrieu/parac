package net.fififox.ParaC;

public class VariableSymbol extends Symbol {

	public enum Type {
		INT, INT_POINTER, FLOAT, FLOAT_POINTER
	}

	public Type type;
	public String address;

	@Override
	public String toString() {
		return type + " " + name;
	}

}
