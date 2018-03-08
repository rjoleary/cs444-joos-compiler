import java.lang.String;
// Demonstrates many of the AST nodes.
public class AstExamples {
    protected int x;
    public AstExamples() {
        int x = 2 + 3;
        int y = 5 * 6;
        boolean b = false;
        x = y * 4 / 5 + this.x - (this.x);
        b = (x < y) == (x >= y) != (x <= y) && !(x > y);
        if (null instanceof AstExamples) {
            x = - -x;
            String s = "A";
            char c = 'B';
            b = false;
        }
        int[] arr = new int[80];
        arr[3] = x + y;
        y = (short)x(x, x);
    }
    public int x(int a, int b) {
        return a + b;
    }
}
