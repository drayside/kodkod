package taglet;

import java.util.EnumSet;
import java.util.Map;

import com.sun.tools.doclets.Taglet;

import static taglet.SpecTaglet.Usage.*;

/**
 * Represents the </code>@effects</code> tag.  The
 * tage can be used in METHOD and CONSTRUCTOR comments.
 * 
 * @author Emina Torlak
 */
public class EffectsTaglet extends SpecTaglet {

	private EffectsTaglet() {
		super("effects", EnumSet.of(METHOD, CONSTRUCTOR));
	}

	/**
     * Register this Taglet.
     * @param tagletMap  the map to register this tag to.
     */
    public static void register(Map<String, Taglet> tagletMap) {
    	   (new EffectsTaglet()).registerThis(tagletMap);
    }
}
