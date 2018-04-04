//TYPE_LINKING

/**
 * TypeLinking:
 * Check that no package names or prefixes hereof (consisting of whole
 * identifiers) of declared packages, single-type-import declarations
 * or used import-on-demand declarations resolve to qualified types,
 * i.e. types not in the default package.
 *
 * The declared class javax.swing.tree does not clash with the package
 * of the same name because javax.swing.tree is not refered, nor does
 * is clash because javax.swing.tree is implicitly loaded by the use
 * of JTree.
 */
import javax.swing.*;

public class Main {
	public Main() {}

	public static int test() {
		JTree tree = new JTree();
    boolean b = new JTree() instanceof Object;
    b = (new JTree()) instanceof Object;
    {
        boolean b2 = new JTree() instanceof Object;
    }
		return 123;
	}
}
