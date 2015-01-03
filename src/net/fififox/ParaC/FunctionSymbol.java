package net.fififox.ParaC;

import java.util.ArrayList;
import java.util.List;

public class FunctionSymbol extends Symbol {
	
	public String returnType;
	public List<VariableSymbol> parameters = new ArrayList<>();
	
	@Override
	public String toString() {
		return "Function<" + returnType + ", " + name + ", " + parameters + ">";
	}

}
