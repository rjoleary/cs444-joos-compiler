public class DivideByZero {
    public DivideByZero() {}

    public static int test() {
        int x = 0;
        int y = 123;
        return y / x;
    }
}
