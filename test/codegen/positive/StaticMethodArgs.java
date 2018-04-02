public class StaticMethodArgs {
    public static StaticMethodArgs() {}

    public static int f(int x, int y, int z) {
        return x - y + z;
    }

    public static int test() {
        return f(200, 80, 3);
    }
}
