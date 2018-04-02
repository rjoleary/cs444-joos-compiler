public class StaticFields {
    public static StaticFields() {}

    public static int a = 1 * 2;
    public static int b = 3 / 4;
    public static int c = 5 % 6;
    public static int d = 7 + 8;
    public static String e = "a" + "b";
    public static String f = "a" + 9;
    public static int g = 10 - 11;
    public static boolean h = 12 < 13;
    public static boolean i = 45 > 23;
    public static boolean j = 12 <= 12;
    public static boolean k = 32 >= 23;
    public static boolean l = 12 == 34;
    public static boolean m = "abc" == "abc";
    public static boolean n = 342 != 32;
    public static boolean o = true & false;
    public static boolean p = false | true;
    public static boolean q = true && false;
    public static boolean r = true || true;
    public static boolean s = (true || true) && false || true && false;

    public static int test() {
        return 123;
    }
}
