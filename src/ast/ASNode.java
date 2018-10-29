package ast;


import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author tazik
 */
public class ASNode {
    
    /**
     * Enumeration of different types for AS nodes.
     */
    public enum Type {
        ROOT("ROOT"),
        IMPORTS("IMPORTS"),
        PACKAGE("PACKAGE"),
        MODFIER("MODIFIER"),
        CLASS("CLASS"),
        NAME("NAME"),
        EXTENDS("EXTENDS"),
        IMPLEMENTS("IMPLEMENTS"),
        BLOCK("BLOCK"),
        FIELDS("FIELDS"),
        METHODS("METHODS"),
        CONSTRUCTORS("CONSTRUCTORS"),
        TYPES("TYPES"),
        TYPE("TYPE"),
        VARDECLARATION ("VARIABLE"),
        RETURN ("RETURN"),
        PARAMETERS ("PARAMETERS"),
        PARAMETER ("PARAMETER"),
        METHODBODY ("METHODBODY"),
        IFSTATEMENT("IF"),
        VARIABLE("VARIABLE"),
        INITIALVALUE("INITIAL VALUE"),
        CONDITION("CONDITIONS"),
        ELSEIF("ELSE"),
        EPSILON(""),
        FOR("FOR"),
        FORCONTROOL("FOR CONTROL"),
        FOR_INIT("FOR INIT"),
        FOR_UPDATE("FOR UPDATE"),
        WHILE ("WHILE"),
        DOWHILE("DO WHILE"),
        TRY("TRY"),
        SWITCH("SWITCH"),
        FINALLY("FINALLY"),
        CATCH("CATCH"),
        STATEMENT("STATEMENT"),
        RESOURCE("RESOURCE"),
        EXPRESSION("EXPRESSION"),
        CASE("CASE");

        public final String label;

        private Type(String lbl) {
            label = lbl;
        }

        @Override
        public String toString() {
            return label;
        }
    }

    private Map<String, Object> properties;
	
	public ASNode(Type type) {
		properties = new LinkedHashMap<>();
        setType(type);
	}
    
    public void setType(Type type) {
        properties.put("type", type);
    }
    
    public Type getType() {
        return (Type) properties.get("type");
    }
	
	public void setLineOfCode(int line) {
		properties.put("line", line);
	}
	
	public int getLineOfCode() {
		return (Integer) properties.get("line");
	}
	
	public void setValue(String code) {
		properties.put("value", code);
	}
	
	public String getValue() {
		return (String) properties.get("value");
	}
	
	public void setProperty(String key, Object value) {
		properties.put(key.toLowerCase(), value);
	}
	
	public Object getProperty(String key) {
		return properties.get(key.toLowerCase());
	}
	
	public Set<String> getAllKeys() {
		return properties.keySet();
	}
	
	@Override
	public String toString() {
        String value = getValue();
        if (value == null)
            return getType().label;
        return getType().label + ": " + value;
	}
    
}
