package net.fififox.ParaC;

public class VariableSymbol extends Symbol {

	public enum Type {
		INT, FLOAT, INT_POINTER, FLOAT_POINTER, INT_ARRAY, FLOAT_ARRAY;
	}

	public Type type;
	public String address;

	@Override
	public String toString() {
		return type + " " + name + "<" + address + ">";
	}

}
