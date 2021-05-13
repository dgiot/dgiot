package erlport.terms;

import java.util.*;
import java.lang.*;

public class Reference {
    public Atom node;
    public Long id;
    public Long creation;

    public Reference(Atom node, Long id, Long creation) {
       this.node = node;
       this.id = id;
       this.creation = creation;
    }

    @Override
    public String toString() {
        return String.format("Reference(node: %s, id: %d, creation: %d)", node.toString(), id, creation);
    }
}
