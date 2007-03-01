package taglet;

import java.util.EnumSet;
import java.util.Map;

import com.sun.javadoc.Tag;
import com.sun.tools.doclets.Taglet;

/**
 * Represents a taglet for specification tags:  <code>@specfield</code>, <code>@invariant</code>,
 * <code>@effects</code>, and <code>@requires</code>.
 * 
 * @specfield name: String
 * @specfield usage: set Usage
 * @author Emina Torlak
 */
public abstract class SpecTaglet implements Taglet {
	protected static enum Usage {
		FIELD, CONSTRUCTOR, METHOD, OVERVIEW, PACKAGE, TYPE, INLINE
	}
	private final String name;
	private final EnumSet<Usage> usage;
	
	protected SpecTaglet(String name, EnumSet<Usage> usage) {
		if (name==null || usage==null) throw new NullPointerException("name=null || usage=null");
		this.name = name;
		this.usage = usage;
	}
	
	 
    /**
     * Return the name of this tag.
     * @return this.name
     */
    public String getName() {
        return name;
    }
	
	/**
	 * Return true if this tag can be used in a field; otherwise returns false.
	 * @return FIELD in this.usage
	 */
	public boolean inField() {
		return usage.contains(Usage.FIELD);
	}
	
	/**
	 * Return true if this tag can be used in a constructor; otherwise returns false.
	 * @return CONSTRUCTOR in this.usage
	 */
	public boolean inConstructor() {
		return usage.contains(Usage.CONSTRUCTOR);
	}
	
	/**
	 * Return true if this tag can be used in a method; otherwise returns false.
	 * @return METHOD in this.usage
	 */
	public boolean inMethod() {
		return usage.contains(Usage.METHOD);
	}
	
	/**
	 * Return true if this tag can be used in the overview; otherwise returns false.
	 * @return OVERVIEW in this.usage
	 */
	public boolean inOverview() {
		return usage.contains(Usage.OVERVIEW);
	}
	
	/**
	 * Return true if this tag can be used in a package; otherwise returns false.
	 * @return PACKAGE in this.usage
	 */
	public boolean inPackage() {
		return usage.contains(Usage.PACKAGE);
	}
	
	/**
	 * Return true if this tag can be used in a type; otherwise returns false.
	 * @return TYPE in this.usage
	 */
	public boolean inType() {
		return usage.contains(Usage.TYPE);
	}
	
	/**
	 * Return true if this tag can be used inline; otherwise returns false.
	 * @return INLINE in this.usage
	 */
	public boolean isInlineTag() {
		return usage.contains(Usage.INLINE);
	}

	public String toString(Tag tag) {
		return "<DT><B>" + getName() + ":</B></DT><DD>"
		+ "<table cellpadding=2 cellspacing=0><tr><td><code>"
		+ tag.text()
		+ "</code></td></tr></table></DD>\n";
	}
	
	public String toString(Tag[] tags) {
		if (tags.length==0) return null;
		final StringBuilder ret = new StringBuilder();
		ret.append("<DT><B>");
		ret.append(getName());
		ret.append(":</B></DT>");
		ret.append("\n<DD><table cellpadding=2 cellspacing=0>");
		for (Tag tag : tags) {
			ret.append("<tr><td><code>");
			ret.append(tag.text());
			ret.append("</code></td></tr>");
		}
		ret.append("</table></DD>\n");
		return ret.toString();
	}
	
	
	protected void registerThis(Map<String, Taglet> tagletMap) {
		tagletMap.put(getName(), this);
	}
	
	
}
