public class BasicLocals {
    public static BasicLocals() {}

    public static int test() {
        int x = 100;
        int y = 10;
        int sum = x + y;
        {
            int z = 2;
            sum = sum + z;
        }
        {
            int z = 1;
            sum = sum + z;
            sum = sum + x;
        }
        sum = sum + y;
        sum = sum - x;
        return sum;
    }
}
