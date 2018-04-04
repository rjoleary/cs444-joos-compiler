public class NonstaticField {
    protected int x = 100;
    public int y = x - 80;
    public int z;

    public NonstaticField() {
        z = 3;
    }

    public static int test() {
        return new NonstaticField().x + new NonstaticField().y + new NonstaticField().z;
    }
}
