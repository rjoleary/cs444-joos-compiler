public class Main {
    public static void main(String args[]) {
        B b = new B();
        System.out.println("b.f");
        System.out.println(b.f);

        A a = (A) b;
        System.out.println("a.f");
        System.out.println(a.f);
    }
}
