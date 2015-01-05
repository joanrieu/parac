package net.fififox.ParaC;

import java.util.ArrayList;
import java.util.List;

import net.fififox.ParaC.VariableSymbol.Type;

public class FunctionSymbol extends Symbol {
	
	public Type returnType;
	public List<VariableSymbol> parameters = new ArrayList<>();
	
	@Override
	public String toString() {
		return returnType + " " + name + parameters;
	}

}
