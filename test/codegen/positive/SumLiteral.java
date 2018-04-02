public class SumLiteral {
    public SumLiteral() {}

    public static int test() {
        String x = "A\043"; // 65 + 35
        return x.charAt(0) + x.charAt(1) + 23;
    }
}
