package taglet;

import java.util.EnumSet;
import java.util.Map;

import com.sun.tools.doclets.Taglet;

import static taglet.SpecTaglet.Usage.*;

/**
 * Represents the </code>@invariant</code> tag.  The
 * tage can be used in TYPE, METHOD, CONSTRUCTOR, and FIELD comments.
 * 
 * @author Emina Torlak
 */
public final class InvariantTaglet extends SpecTaglet {
	  
    private InvariantTaglet() {
    		super("invariant", EnumSet.of(TYPE, METHOD, CONSTRUCTOR, FIELD));
    }
    
    /**
     * Register this Taglet.
     * @param tagletMap  the map to register this tag to.
     */
    public static void register(Map<String, Taglet> tagletMap) {
    	   (new InvariantTaglet()).registerThis(tagletMap);
    }
}
