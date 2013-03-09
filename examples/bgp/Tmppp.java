package bgp;
import kodkod.instance.Instance;
import kodkod.instance.Universe;

import com.thoughtworks.xstream.XStream;


public class Tmppp {

    public static void main(String[] args) {
        Universe universe = new Universe("a", "b", "c");
        Instance inst = new Instance(universe);
        XStream xstream = new XStream();
//        xstream.alias("instance", Instance.class);
//        xstream.alias("universe", Universe.class);
        System.out.println(inst);
        String str = xstream.toXML(inst);
//        System.out.println(str);
        inst = (Instance) xstream.fromXML(str);
        System.out.println(inst);
    }
    
    
}
