public class CastExpression {
    public CastExpression() {}
    public void f(CastExpression z) {
        int x = (int)-1;
        int y = (x)-1;
        CastExpression w = (CastExpression)z;
    }
}
