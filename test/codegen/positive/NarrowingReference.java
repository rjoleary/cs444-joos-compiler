public class NarrowingReference {
    public NarrowingReference() {
    }

    public static int test() {
        Object x = new NarrowingReference();
        NarrowingReference y = (NarrowingReference)x;
        return 123;
    }
}
