package taglet;

import static taglet.SpecTaglet.Usage.*;

import java.util.EnumSet;
import java.util.Map;

import com.sun.tools.doclets.Taglet;

/**
 * Represents the </code>@requires</code> tag.  The
 * tage can be used in METHOD and CONSTRUCTOR comments.
 * 
 * @author Emina Torlak
 */
public final class RequiresTaglet extends SpecTaglet {

	private RequiresTaglet() {
		super("requires", EnumSet.of(METHOD, CONSTRUCTOR));
	}

	/**
     * Register this Taglet.
     * @param tagletMap  the map to register this tag to.
     */
    public static void register(Map<String, Taglet> tagletMap) {
    	   (new RequiresTaglet()).registerThis(tagletMap);
    }
    
}
