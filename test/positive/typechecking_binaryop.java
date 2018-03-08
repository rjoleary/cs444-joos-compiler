public class typechecking_binaryop {
    public typechecking_binaryop() {}
    protected int x;
    protected int min = -2147483648;
    protected int max = 2147483647;
    public int f(){
        x = min / max;
        x = min * max;
        x = min % max;
        return x;
    }
}
