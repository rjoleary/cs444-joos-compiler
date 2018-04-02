public class BasicHello {
    public BasicHello() {}

    public static int test() {
        java.io.OutputStream.nativeWrite((int)'H');
        java.io.OutputStream.nativeWrite((int)'e');
        java.io.OutputStream.nativeWrite((int)'l');
        java.io.OutputStream.nativeWrite((int)'l');
        java.io.OutputStream.nativeWrite((int)'o');
        java.io.OutputStream.nativeWrite((int)'\n');
        java.io.OutputStream.nativeWrite((int)'W');
        java.io.OutputStream.nativeWrite((int)'o');
        java.io.OutputStream.nativeWrite((int)'r');
        java.io.OutputStream.nativeWrite((int)'l');
        java.io.OutputStream.nativeWrite((int)'d');
        java.io.OutputStream.nativeWrite((int)'\n');
        return 123;
    }
}
