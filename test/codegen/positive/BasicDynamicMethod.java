public class BasicDynamicMethod {
    public BasicDynamicMethod() {}

    public int foo(int x) {
        return x + 1;
    }

    public static int test() {
        BasicDynamicMethod o = new BasicDynamicMethod();

        return o.foo(122);
    }
}
