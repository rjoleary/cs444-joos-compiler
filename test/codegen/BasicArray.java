public class BasicArray {
    public BasicArray() {}

    public int test() {
        int[] a = new int[1+2];
        int sum = a[0] + a[1] + a[2];
        a[0] = 1;
        a[1] = 2;
        a[2] = 3;
        return sum + a[0] * 100 + a[1] * 20 + a[2];
    }
}
