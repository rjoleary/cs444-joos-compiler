public class BasicStaticMethod {
    public static BasicStaticMethod() {}

    public static int f() {
        return 123;
    }

    public static int test() {
        return BasicStaticMethod.f();
    }
}
